# 170420 CHL
# PlumX <3 DiVA
#
#skriptet läser in data från json-fil* på server och kombinerar PlumX-data med DiVA-data.
# *json-filen nedladdad via vår prenumeration på PlumX.
#
#
source('/home/shub/src/common/lib/sh.archive.R')
#
#library(tidyverse) library purrr i konflikt med library jsonlite i tidyverse. Uppdatera när det är löst.
#
library(jsonlite)
library(dplyr)
library(reshape2)
library(lazyeval)
library(ggplot2)

sh.archive.start("beer")
#
#Load PlumX-data ------------------------------------------------------------------------------------------------------------
#
total_plumxdata <- data.frame(fromJSON(txt="/home/shub/assets/plumx.JSON")) #kräver jsonlite/tidyverse.
#flatten i jsonlite i konflikt med flatten i purr
total_plumxdata <- flatten(total_plumxdata, recursive = TRUE) #hämta upp nested data så att all data får egna kolumner
#begränsa datamängden genom att endast arbeta med de kolumner som behövs
urval_plumxdata <- select(total_plumxdata, matches(".sortCount.|.repoUrl"))


#gör om tomma Url-celler samt felformaterade Url:er till NAs
urval_plumxdata$document.identifier.repoUrl <- ifelse((urval_plumxdata$document.identifier.repoUrl == "NULL"), 
                                                      NA, urval_plumxdata$document.identifier.repoUrl)
urval_plumxdata$document.identifier.repoUrl <- ifelse(grepl("^c", urval_plumxdata$document.identifier.repoUrl), 
                                                      NA, urval_plumxdata$document.identifier.repoUrl)

#dela på Url till förled och PID för att kunna matcha mot DiVA
urval_plumxdata <- cbind(urval_plumxdata, colsplit(urval_plumxdata$document.identifier.repoUrl, "-", c("url", "idnr")))
#urval_plumxdata$idnr <- as.integer(urval_plumxdata$idnr) #behövs inte
urval_plumxdata$document.identifier.repoUrl = unlist(urval_plumxdata$document.identifier.repoUrl) #default är lista, vilket inte fungerar med sh.archive-funktionen
sh.archive.df(urval_plumxdata, "plumx.urval.data")

#
#Load DiVA-data -------------------------------------------------------------------------------------------------------------
#
csvall2 <- read.csv("/home/shub/assets/diva/diva_csvall2_allt_latest.csv", header = TRUE, sep = ",", quote = "\"",
                    stringsAsFactors = FALSE)
csvall2 <- cbind(csvall2, colsplit(csvall2$NBN, "-", c("url", "idnr")))
#csvall2mod$idnr <- as.integer(csvall2mod$idnr) #behövs ej
csvall2 <- mutate(csvall2, baltic = ifelse(grepl("Baltic", csvall2mod$ResearchSubjects), T, F))
csvall2$oss <- grepl("Östersjöstiftelsen", csvall2$Funder) #kan inte använda Mutate?
csvall2 <- mutate(csvall2, beer = ifelse((baltic == TRUE|csvall2$oss == TRUE), TRUE, FALSE)) #ifelse((csvall2mod$Baltic == TRUE|csvall2mod$OSS == TRUE), TRUE, FALSE)
###csvall2 <- select(csvall2, PID, Title, PublicationType, Journal, Year, Publisher, idnr, beer)
sh.archive.df(csvall2, "csvall2.urval.data")

test <- mutate(csvall2, CCT = ifelse(grepl("Critical and Cultural Theory", ResearchSubjects), T, F))

#
# Combine data-----------------------------------------------------------------------------------------------------------
#
gemensam_tabell <- left_join(csvall2, urval_plumxdata, by = "idnr")
sh.archive.df(gemensam_tabell, "gemensam.urval.data")

#
# Calculate Top10--------------------------------------------------------------------------------------------------------
#
#Kräver dplyr/tidyverse. Resultatet bör vara en tabell med sortering på 1.år och 2.rankning (klart) och 
#BEER-publ uppmärkta (att göra).
#referera till kolumner med grep?
#
alt_top10 <- function(df, column.name) {
  mutate_call <- list(interp(~ percent_rank(a), a = as.name(column.name)))
  filter_call <- interp(~ b >= 0.9, b = as.name("xrank"))
  select_call <- list(~PID, ~Year, ~Title, ~Journal, ~Publisher, ~beer, ~PublicationType, column.name, "xrank")
  arrange_call <- list(~Year, "desc(xrank)")
  results <- df %>% 
    group_by_(~Year) %>% 
    mutate_(.dots = setNames(mutate_call, "xrank")) %>% #(.dots = setNames(list(mutate.call), "xrank"))
    filter_(filter_call) %>%
    select_(.dots = select_call) %>%
    arrange_(.dots = arrange_call)
    
  #outputfile <- paste(column.name,"-output.txt", sep="")
  #write.csv(results, outputfile)
  return(results)
}

export_saves_ebsco <- alt_top10(gemensam_tabell, "document.sortCount.capture.EXPORTS_SAVES.EBSCO")
sh.archive.df(export_saves_ebsco, "Export_saves_ebsco")
#PDF_VIEWS.EBSCO verkar inte finnas längre
#pdf_views_ebsco <- alt_top10(gemensam_tabell, "document.sortCount.usage.PDF_VIEWS.EBSCO")
#sh.archive.df(pdf_views_ebsco, "pdf_views_ebsco")
holdings_worldcat <- alt_top10(gemensam_tabell, "document.sortCount.usage.HOLDINGS_COUNT.WorldCat")
sh.archive.df(holdings_worldcat, "holdings_worldcat")
readers_mendeley <- alt_top10(gemensam_tabell, "document.sortCount.capture.READER_COUNT.Mendeley")
sh.archive.df(readers_mendeley, "readers_mendeley")


#
#Create scatterplot---------------------------------------------------------------------------------------------------------
#
#Kräver ggplot2/tidyverse.
#hur referera till kolumner?
alt_scatterplot_beer <- function(df, x, y){
  P <- ggplot(data = df) +
        geom_point(mapping = aes(x, y, color = as.factor(gemensam_tabell$beer))) +
        theme(legend.position = "none") +
        labs(x = "Publiceringsår", y = "Antal")
  return(P)
}

exports_saves_ebsco_scatterplot <- alt_scatterplot_beer(gemensam_tabell, gemensam_tabell$Year, gemensam_tabell$document.sortCount.capture.EXPORTS_SAVES.EBSCO)
#PDF_VIEWS.EBSCO verkar inte finnas längre
#usage_pdf_scatterplot <- alt_scatterplot_beer(gemensam_tabell, gemensam_tabell$Year, gemensam_tabell$document.sortCount.usage.PDF_VIEWS.EBSCO)
usage_holdings_scatterplot <- alt_scatterplot_beer(gemensam_tabell, gemensam_tabell$Year, gemensam_tabell$document.sortCount.usage.HOLDINGS_COUNT.WorldCat)
mendeley_readers_scatterplot <- alt_scatterplot_beer(gemensam_tabell, gemensam_tabell$Year, gemensam_tabell$document.sortCount.capture.READER_COUNT.Mendeley)

print(usage_holdings_scatterplot)
#
#Save scatterplots as images?
#

sh.archive.end()

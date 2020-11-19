#
# ostersjo
# GL 200831
# Rapportering till Östersjöstiftelsen
# Statistik med DiVA som källa.
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

n_issn <- read.csv(file="/home/shub/assets/nsd.issn.csv",
                   header=TRUE,
                   sep=";",
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE,
                   encoding = "latin1")

n_forlag <- read.csv(file="/home/shub/assets/nsd.forlag.csv",
         header=TRUE,
         sep=";",
         na.strings = c("", "NA"),
         stringsAsFactors = FALSE,
         encoding = "latin1")
n_forlag$Original.tittel <- recode(n_forlag$Original.tittel, "Södertörns Högskola" = "Södertörns högskola")

sh_archive_start("ÖSS")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2015, 2019))

diva <- funder_oss(diva)
diva <- subject_baltic(diva)

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$SeriesISSN[is.na(diva$SeriesISSN)] <- 0L
diva$SeriesEISSN[is.na(diva$SeriesEISSN)] <- 0L


# Publikationer -----------------------------------------------------------

publ_vet <- diva %>%
  filter(PublicationType=="Artikel i tidskrift"|PublicationType=="Artikel, forskningsöversikt"
         |PublicationType=="Kapitel i bok, del av antologi"|PublicationType=="Bok"|PublicationType=="Konferensbidrag") %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published")%>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper")%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|(JournalEISSN %in% n_issn$`Online.ISSN`)))

publ_vet$PublicationType <- recode(publ_vet$PublicationType,
                                  "Artikel i tidskrift" = "Artikel",
                                  "Artikel, forskningsöversikt" = "Artikel",
                                  "Kapitel i bok, del av antologi" = "Kapitel",
                                  "Bok" = "Monografi",
                                  "Konferensbidrag" = "Publicerat konferensbidrag")

publ_tabell <- publ_vet %>%
  group_by(PublicationType, ContentType) %>% 
  count(Year) %>%
  spread(Year, n) %>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

publ_tabell["definition"] <- "SH totalt"
publ_tabell <- publ_tabell %>% unite(PublicationType, PublicationType, definition, sep=": ")

# Östersjöforskning -------------------------------------------------------
# Redovisa finansiär ÖSS, men använd sedan forskningsområdet

oss <- publ_vet %>%
  filter(oss == TRUE) %>%
  group_by(PublicationType, ContentType) %>% 
  count(Year) %>%
  spread(Year, n) %>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

oss["definition"] <- "ÖSS finansiär"
oss <- oss %>% unite(PublicationType, PublicationType, definition, sep=": ")

baltic <- publ_vet %>%
  filter(baltic == TRUE) %>%
  group_by(PublicationType, ContentType) %>% 
  count(Year) %>%
  spread(Year, n) %>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

baltic["definition"] <- "Östersjöforskning"
baltic <- baltic %>% unite(PublicationType, PublicationType, definition, sep=": ")

publ_tabell <- bind_rows(publ_tabell, baltic, oss)
publ_tabell[is.na(publ_tabell)] <- 0L
write_csv(publ_tabell, "Tabell publikationer.csv")

# Språk -------------------------------------------------------------------

lang_publ <- publ_vet %>%
  group_by(Language) %>%
  count(Year) %>%
  spread(Year, n)

lang_publ["definition"] <- "SH totalt"
lang_publ <- lang_publ %>% unite(Language, Language, definition, sep=": ")

lang_baltic <- publ_vet %>%
  filter(baltic == TRUE) %>%
  group_by(Language) %>% 
  count(Year) %>%
  spread(Year, n)

lang_baltic["definition"] <- "Östersjöforskning"
lang_baltic <- lang_baltic %>% unite(Language, Language, definition, sep=": ")

lang_publ <- bind_rows(lang_publ, lang_baltic)
lang_publ[is.na(lang_publ)] <- 0L
write_csv(lang_publ, "Språk.csv")


# DOI -----------------------------------------------------------
# Skapa en DOI-fil som går att kopiera till Scopus och WoS avancerade sökning.
doi <- publ_vet %>%
  filter(baltic == TRUE) %>%
  filter(PublicationType == "Artikel") %>%
  filter(!(is.na(DOI))) %>%
  transmute(scop = str_c("DOI(", DOI,") OR "))
  #transmute(wos =str_c("DO=", DOI, " OR "))

write_csv(doi, "DOI")

# Open Access kapitel -------------------------------------------------------------

options(OutDec= ",")
  
sh_kap <- publ_vet %>%
  filter(PublicationType == "Kapitel") %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_sh_kap <- publ_vet %>%
  filter(PublicationType == "Kapitel") %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

baltic_kap <- publ_vet %>%
  filter(PublicationType == "Kapitel") %>%
  filter(baltic == TRUE) %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_baltic_kap <- publ_vet %>%
  filter(PublicationType == "Kapitel") %>%
  filter(baltic == TRUE) %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

oa_stats_kap <- full_join(sh_kap, oa_sh_kap, by = "Year")
names(oa_stats_kap) <- c("Year", "sh_kap", "sh_kap_oa")
oa_stats_kap <- full_join(oa_stats_kap, baltic_kap, by = "Year")
names(oa_stats_kap) <- c("Year", "sh_kap", "sh_kap_oa", "baltic_kap")
oa_stats_kap <- full_join(oa_stats_kap, oa_baltic_kap, by = "Year")
names(oa_stats_kap) <- c("Year", "sh_kap", "sh_kap_oa", "baltic_kap", "baltic_kap_oa")
oa_stats_kap[is.na(oa_stats_kap)] <- 0L

oa_andel_kap <- oa_stats_kap %>%
  mutate(sh_kap_andel_oa = sh_kap_oa/sh_kap, 
         baltic_kap_andel = baltic_kap_oa/baltic_kap) %>%
  select(Year, sh_kap_andel_oa, baltic_kap_andel) %>%
  arrange(Year)

write_excel_csv2(oa_andel_kap, "Andel_oa_kapitel.csv")

# Open Access artikel -----------------------------------------------------

sh_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_sh_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

baltic_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  filter(baltic == TRUE) %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_baltic_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  filter(baltic == TRUE) %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

oss_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  filter(oss == TRUE) %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_oss_art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  filter(baltic == TRUE) %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

oa_stats_art <- full_join(sh_art, oa_sh_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa")
oa_stats_art <- full_join(oa_stats_art, baltic_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa", "baltic_art")
oa_stats_art <- full_join(oa_stats_art, oa_baltic_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa", "baltic_art", "baltic_art_oa")
oa_stats_art <- full_join(oa_stats_art, oss_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa", "baltic_art", "baltic_art_oa", "oss_art")
oa_stats_art <- full_join(oa_stats_art, oa_oss_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa", "baltic_art", "baltic_art_oa", "oss_art", "oss_art_oa")


oa_andel_art <- oa_stats_art %>%
  mutate(sh_art_andel_oa = sh_art_oa/sh_art, 
         baltic_art_andel = baltic_art_oa/baltic_art,
         oss_art_andel = oss_art_oa/oss_art) %>%
  select(Year, sh_art_andel_oa, baltic_art_andel, oss_art_andel) %>%
  arrange(Year)
  

write_excel_csv2(oa_andel_art, "Andel_oa_artiklar.csv")


# Norska listan -----------------------------------------------------------
# Matchning norska listan.
# nsd_kol = vektor som bestämmer vilken kolumn nivåvärdet hämtas från ur norska filerna
# Omatchade publikationer måste tas bort innan nivån hämtas. Läggs tillbaka efteråt.

year1 <- 2015
year2 <- 2016
year3 <- 2017
year4 <- 2018
year5 <- 2019


art <- publ_vet %>%
  filter(PublicationType == "Artikel") %>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year, Publisher, ContentType, ISI, ScopusId, oss, baltic)

art_1 <- art %>%
  filter(Year == year1) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))
  
nsd_kol <- str_c("Nivå.", year1) 

art_norsk <- art_1 %>%
  filter(nsd_row > 0) %>%
  #rowwise behövs för att indexeringen i nästa rad ska fungera:
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  #för att ta bort rowwise:
  ungroup()

art_ej_norsk <- art_1 %>%
  filter(nsd_row == 0)

year_1 <- bind_rows(art_norsk, art_ej_norsk)

art_2 <- art %>%
  filter(Year == year2) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year2) 

art_norsk <- art_2 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_2 %>%
  filter(nsd_row == 0)

year_2 <- bind_rows(art_norsk, art_ej_norsk)

art_3 <- art %>%
  filter(Year == year3) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year3)

art_norsk <- art_3 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_3 %>%
  filter(nsd_row == 0)

year_3 <- bind_rows(art_norsk, art_ej_norsk)

art_4 <- art %>%
  filter(Year == year4) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year4)

art_norsk <- art_4 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_4 %>%
  filter(nsd_row == 0)

year_4 <- bind_rows(art_norsk, art_ej_norsk)

art_5 <- art %>%
  filter(Year == year5) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year5)

art_norsk <- art_5 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_5 %>%
  filter(nsd_row == 0)

year_5 <- bind_rows(art_norsk, art_ej_norsk)

art_alla <- bind_rows(year_1, year_2, year_3, year_4, year_5)
art_alla$ISI[!is.na(art_alla$ISI)] <- 1L
art_alla$ScopusId[!is.na(art_alla$ScopusId)] <- 1L
art_alla[is.na(art_alla)] <- 0L


# Tabell indexerade artiklar

norsk <- art_alla %>%
  group_by(nivå) %>% 
  count(Year) %>%
  spread(Year, n)
norsk ["Indikator"] <- "Nivå norska listan"
norsk <- norsk %>%
  unite(nivå, Indikator, nivå, sep=": ") %>%
  rename(Index = nivå)

ISI <- art_alla %>%
  group_by(ISI) %>% 
  count(Year) %>%
  spread(Year, n)
ISI ["Indikator"] <- "Web of Science"
ISI <- ISI %>% 
  unite(ISI, Indikator, ISI, sep=": ") %>%
  rename(Index = ISI)

Scopus <- art_alla %>%
  group_by(ScopusId) %>% 
  count(Year) %>%
  spread(Year, n)
Scopus ["Indikator"] <- "Scopus"
Scopus <- Scopus %>% 
  unite(ScopusId, Indikator, ScopusId, sep=": ") %>%
  rename(Index = ScopusId)

index_art <- bind_rows(norsk, ISI, Scopus)

# Begränsa till Östersjöforskning

art_selection <- art_alla %>%
  filter(baltic == TRUE)

norsk <- art_selection %>%
  group_by(nivå) %>% 
  count(Year) %>%
  spread(Year, n)
norsk ["Indikator"] <- "Nivå norska listan"
norsk <- norsk %>%
  unite(nivå, Indikator, nivå, sep=": ") %>%
  rename(Index = nivå)

ISI <- art_selection %>%
  group_by(ISI) %>% 
  count(Year) %>%
  spread(Year, n)
ISI ["Indikator"] <- "Web of Science"
ISI <- ISI %>% 
  unite(ISI, Indikator, ISI, sep=": ") %>%
  rename(Index = ISI)

Scopus <- art_selection %>%
  group_by(ScopusId) %>% 
  count(Year) %>%
  spread(Year, n)
Scopus ["Indikator"] <- "Scopus"
Scopus <- Scopus %>% 
  unite(ScopusId, Indikator, ScopusId, sep=": ") %>%
  rename(Index = ScopusId)

index_selection <- bind_rows(norsk, ISI, Scopus)

write_csv(art_alla, "Artiklar_norska.csv")
write_csv(index_art, "SH_artiklar_index.csv")
write_csv(index_selection, "Östersjö_artiklar_index.csv")

# Norska listan böcker ----------------------------------------------------
# Viktigt att förlagsnamnen stämmer mot Norska listan. Samma år som definerats under artiklar.
# Dubbla serier behöver kollas manuellt i filen.

bok <- publ_vet %>%
  filter(PublicationType == "Monografi"| PublicationType == "Kapitel"|PublicationType == "Publicerat konferensbidrag") %>% 
  select(PID, Name, Title, Publisher, Year, Series, SeriesISSN, SeriesEISSN, PublicationType, ContentType, oss, baltic)

bok_1 <- bok %>%
  filter(Year == year1) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year1)   

bok_forlag_norsk <- bok_1 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_1 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_1 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_1 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok_2 <- bok %>%
  filter(Year == year2) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year2)

bok_forlag_norsk <- bok_2 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_2 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_2 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_2 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok_3 <- bok %>%
  filter(Year == year3) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year3)

bok_forlag_norsk <- bok_3 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_3 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_3 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_3 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))


bok_4 <- bok %>%
  filter(Year == year4) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year4)

bok_forlag_norsk <- bok_4 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_4 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_4 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_4 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok_5 <- bok %>%
  filter(Year == year5) %>% 
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year5)

bok_forlag_norsk <- bok_5 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_5 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_5 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_5 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok <- bind_rows(year_1, year_2, year_3, year_4, year_5)

index_bok <- bok %>%
  group_by(nivåMax) %>% 
  count(Year) %>%
  spread(Year, n)
index_bok ["Indikator"] <- "Nivå norska listan"
index_bok <- index_bok %>%
  unite(nivåMax, Indikator, nivåMax, sep=": ") %>%
  rename(Index = nivåMax)

# Östesjöforskning

bok_selection <- bok %>%
  filter(baltic == TRUE)

index_bok_selection <- bok_selection %>%
  group_by(nivåMax) %>% 
  count(Year) %>%
  spread(Year, n)
index_bok_selection ["Indikator"] <- "Nivå norska listan"
index_bok_selection <- index_bok_selection %>%
  unite(nivåMax, Indikator, nivåMax, sep=": ") %>%
  rename(Index = nivåMax)

write_csv(bok, "Övriga_norska.csv")
write_csv(index_bok, "SH_övriga_index.csv")
write_csv(index_bok_selection, "Östersjö_övriga_index.csv")


# Avslut ------------------------------------------------------------------

sh_archive_resource("Tabell publikationer.csv")
sh_archive_resource("Artiklar_norska.csv")
sh_archive_resource("Östersjö_artiklar_index.csv")
sh_archive_resource("SH_artiklar_index.csv")
sh_archive_resource("Övriga_norska.csv")
sh_archive_resource("SH_övriga_index.csv")
sh_archive_resource("Östersjö_övriga_index.csv")
sh_archive_resource("Andel_oa_artiklar.csv")
sh_archive_resource("Andel_oa_kapitel.csv")
sh_archive_resource("Språk.csv")
sh_archive_df(diva, "Diva_rådata.csv")
sh_archive_df(publ_vet, "Medräknade publikationer.csv")

sh_archive_end()


# thesaurus

Skriptet mappar lärosätesnamn till ett standardnamn för varje lärosäte, med fokus på svenska lärosäten.

Nedladdning och uppladdning av filer behöver förbättras.

Indata består av Scopus csv-filer nedladdade via Scopus webgränssnitt, prenumeration krävs.
Nedladdning av Scopus-data:
Gör en sökning i Scopus. Välj de poster du vill ladda ned i träfflistan. Om du vill ha all data är varje uttag begränsat till 2000 poster, vill du ha begränsat antal fält är uttaget begränsat till 20 000 poster (171221). Välj Export. Välj de fält du vill ska ingå i uttaget samt csv-fil. Den nedladdade csv-filen utgör indata till skriptet thesaurus.R

en temporär csv-fil (temp_table.csv) skapas för manuell genomgång i Excel. En utvecklingsmöjlighet är att göra den manuella genomgången via Shiny.

Den uppladdade rättade temporära filen bygger på master_thesaurus så att alla resultat sparas i en master-fil.
Sh-intern information: Det finns en sådan för Östersjö- och Östeuropaprojektet på G.

Utdata består av en thesaurusfil. För att den ska kunna användas tillsammans med ovanstående csv-fil för att skapa nätverkskartor i VOSviewer bhöver den sparas om via exv Notepad som en tsv-fil.

Observera att detta inte är ett projekt initierat av Scopus eller VOSviewer.
# Load all necessary packages
library(dplyr)
library(stringr)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(highcharter)
library(ggplot2)
library(shinybrowser)
library(fontawesome)
library(bslib)
library(memoise)
library(shinythemes)
library(tgAPI)
library(BFS)
library(readxl)
library(openxlsx)
library(TGexcel)



# Wirtschaftsstruktur TG aus OGD ----
df_angestellte_tg <- get_dataset("sk-stat-150")

df_angestellte_tg_section <- df_angestellte_tg %>%
  group_by(noga_section_code,noga_section_text, gemeinde,bfs_nr_gemeinde, jahr) %>%
  summarise(beschaeftigte_total = sum(beschaeftigte_total),
            beschaeftigte_frauen = sum(beschaeftigte_frauen),
            beschaeftigte_maenner = sum(beschaeftigte_maenner))

# Bezirke Matching, da nicht im OGD DF ----
df_bezirke <- get_dataset("sk-stat-79")

df_bezirke <- df_bezirke %>%
  select(bezirk, bfs_gemeindenummer) %>%
  rename(bfs_nr_gemeinde = bfs_gemeindenummer)

df_angestellte_tg_section <- df_angestellte_tg_section %>%
  left_join(df_bezirke, by = c("bfs_nr_gemeinde"))

# Schweizweite Daten für den Standort Koefizent ----
df_angestellte_ch <- bfs_get_data(number_bfs = "px-x-0602010000_101", language = "de")

df_angestellte_ch <- df_angestellte_ch %>%
  filter(Kanton == "Schweiz", Beobachtungseinheit == "Beschäftigte", Wirtschaftsabteilung != "Wirtschaftsabteilung - Total")

df_angestellte_ch <- df_angestellte_ch %>%
  mutate(noga_zweisteller_code = str_extract(Wirtschaftsabteilung, "^\\d{2}") %>%
           str_remove("^0")) %>%
  mutate(noga_zweisteller_code = as.numeric(noga_zweisteller_code))

# Mapping zwischen NOGA-Codes erstellen

match_section <- read_xlsx("Daten/match_zweisteller_sektion.xlsx")

match_section <- match_section %>%
  mutate(noga_zweisteller_code = as.numeric(noga_zweisteller_code))

df_angestellte_ch <- df_angestellte_ch %>%
  left_join(match_section, by = c("noga_zweisteller_code"))

df_angestellte_ch_section <- df_angestellte_ch %>%
  group_by(noga_section_code, noga_section_text_lower, Jahr) %>%
  summarise(beschaeftigte_total = sum(`Arbeitsstätten und Beschäftigte`, na.rm = TRUE))


# Schweizweite Indikatoren berechnen (nach Section-Code aggregiert)
df_angestellte_ch_section <- df_angestellte_ch_section %>%
  group_by(Jahr) %>%
  mutate(indikator_ch = beschaeftigte_total/sum(beschaeftigte_total, na.rm = TRUE)) %>%
  ungroup()

# Schweizweite Indikatoren für Join vorbereiten
df_angestellte_ch_match <- df_angestellte_ch_section %>%
  select(Jahr, noga_section_code, noga_section_text_lower, indikator_ch) %>%
  rename(jahr = Jahr) %>%
  unique()

# TG Daten mit schweizweiten Indikatoren verknüpfen
df_angestellte_tg_section <- df_angestellte_tg_section %>%
  left_join(df_angestellte_ch_match, by = c("jahr", "noga_section_code"))

# Standortquotient berechnen ----
df_angestellte_tg_section <- df_angestellte_tg_section %>%
  group_by(jahr, gemeinde) %>%
  mutate(indikator_tg_gemeinde = beschaeftigte_total / sum(beschaeftigte_total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Standortquotient_gemeinde = indikator_tg_gemeinde / indikator_ch)

# Berechnung Bezirke

df_angestellte_tg_section_bezirk <- df_angestellte_tg_section %>%
  group_by(jahr, bezirk, noga_section_code,noga_section_text_lower, indikator_ch) %>%
  summarise(beschaeftigte_total = sum(beschaeftigte_total, na.rm = TRUE))

df_angestellte_tg_section_bezirk <- df_angestellte_tg_section_bezirk %>%
  group_by(jahr, bezirk) %>%
  mutate(indikator_tg_bezirk = beschaeftigte_total / sum(beschaeftigte_total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Standortquotient_bezirk = indikator_tg_bezirk / indikator_ch)


# Berechung Kanton

df_angestellte_tg_section_tg <- df_angestellte_tg_section %>%
  group_by(jahr, noga_section_code, noga_section_text_lower, indikator_ch) %>%
  summarise(beschaeftigte_total = sum(beschaeftigte_total, na.rm = TRUE))

df_angestellte_tg_section_tg <- df_angestellte_tg_section_tg %>%
  group_by(jahr) %>%
  mutate(indikator_tg_tg = beschaeftigte_total / sum(beschaeftigte_total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Standortquotient_tg = indikator_tg_tg / indikator_ch)


# Wachstum der Branche

# ((Endwert / Anfangswert)^(1 / Anzahl der Perioden)) - 1

# (beschaeftigte_total[max(jahr)] / beschaeftigte_total[max(jahr)-10] ^ (1/10)) - 1

# Gemeindeebene

df_angestellte_tg_section <- df_angestellte_tg_section %>%
  group_by(gemeinde, noga_section_code) %>%  # oder welche Gruppierungsvariablen Sie haben
  arrange(jahr) %>%
  mutate(zehn_jahr_wachstum_gemeinde = ((beschaeftigte_total / lag(beschaeftigte_total, 10))^(1/10) - 1)*100) %>%
  ungroup()

# Bezirksebene

df_angestellte_tg_section_bezirk <- df_angestellte_tg_section_bezirk %>%
  group_by(bezirk, noga_section_code) %>%  # oder welche Gruppierungsvariablen Sie haben
  arrange(jahr) %>%
  mutate(zehn_jahr_wachstum_bezirk = ((beschaeftigte_total / lag(beschaeftigte_total, 10))^(1/10) - 1)*100) %>%
  ungroup()

# Kantonsebene

df_angestellte_tg_section_tg <- df_angestellte_tg_section_tg %>%
  group_by(noga_section_code) %>%  # oder welche Gruppierungsvariablen Sie haben
  arrange(jahr) %>%
  mutate(zehn_jahr_wachstum_tg = ((beschaeftigte_total / lag(beschaeftigte_total, 10))^(1/10) - 1)*100) %>%
  ungroup()


# Nur C für den Kanton

df_angestellte_ch_c = df_angestellte_ch %>%
  group_by(Jahr) %>%
  mutate(indikator_ch_c = `Arbeitsstätten und Beschäftigte`/sum(`Arbeitsstätten und Beschäftigte`, na.rm = TRUE)) %>%
  ungroup()

df_angestellte_ch_c <- df_angestellte_ch_c %>%
  filter(noga_section_code == "C")

match_angestellte_ch_c <- df_angestellte_ch_c %>%
  select(noga_zweisteller_code, indikator_ch_c, Jahr) %>%
  rename(jahr = Jahr)

df_angestellte_tg_c <- df_angestellte_tg %>%
  group_by(jahr, noga_zweisteller_code, noga_zweisteller_text, noga_section_code) %>%
  summarise(beschaeftigte_total = sum(beschaeftigte_total, na.rm = TRUE))

df_angestellte_tg_c <- df_angestellte_tg_c %>%
  group_by(jahr) %>%
  mutate(indikator_tg_c = beschaeftigte_total / sum(beschaeftigte_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(noga_section_code == "C")

df_angestellte_tg_c <- df_angestellte_tg_c %>%
  left_join(match_angestellte_ch_c, by = c("noga_zweisteller_code", "jahr"))

df_angestellte_tg_c <- df_angestellte_tg_c %>%
  mutate(Standortquotient_tg_c = indikator_tg_c / indikator_ch_c)

df_angestellte_tg_c <- df_angestellte_tg_c %>%
  group_by(noga_zweisteller_code) %>%  # oder welche Gruppierungsvariablen Sie haben
  arrange(jahr) %>%
  mutate(zehn_jahr_wachstum_tg = ((beschaeftigte_total / lag(beschaeftigte_total, 10))^(1/10) - 1)*100) %>%
  ungroup()


df_angestellte_tg_section$jahr <- as.numeric(df_angestellte_tg_section$jahr)

df_angestellte_tg_section_bezirk$jahr <- as.numeric(df_angestellte_tg_section_bezirk$jahr)

df_angestellte_tg_section_tg$jahr <- as.numeric(df_angestellte_tg_section_tg$jahr)

df_angestellte_tg_c$jahr <- as.numeric(df_angestellte_tg_c$jahr)



df_angestellte_tg_section_tg_xlsx <- df_angestellte_tg_section_tg %>%
  select(jahr,noga_section_code,noga_section_text_lower,beschaeftigte_total,Standortquotient_tg,zehn_jahr_wachstum_tg)

write.xlsx(df_angestellte_tg_section_tg_xlsx, "Daten/daten_kanton_use.xlsx")

df_angestellte_tg_section_bezirke_xlsx <- df_angestellte_tg_section_bezirk %>%
  select(jahr,bezirk, noga_section_code,noga_section_text_lower,beschaeftigte_total,Standortquotient_bezirk,zehn_jahr_wachstum_bezirk)

write.xlsx(df_angestellte_tg_section_bezirke_xlsx, "Daten/daten_bezirk_use.xlsx")


df_angestellte_tg_section_xlsx <- df_angestellte_tg_section %>%
  select(jahr,gemeinde,bezirk, noga_section_code, noga_section_text_lower,beschaeftigte_total,Standortquotient_gemeinde,zehn_jahr_wachstum_gemeinde)

write.xlsx(df_angestellte_tg_section_xlsx, "Daten/daten_gemeinde_use.xlsx")


df_angestellte_tg_c_xlsx <- df_angestellte_tg_c %>%
  select(jahr, noga_zweisteller_code, noga_zweisteller_text,beschaeftigte_total,Standortquotient_tg_c,zehn_jahr_wachstum_tg)

write.xlsx(df_angestellte_tg_c_xlsx, "Daten/daten_sectorC_use.xlsx")

max_year <- max(df_angestellte_tg_section_xlsx$jahr, na.rm = TRUE)
min_year <- max_year - 10


df_angestellte_tg_section_tg_xlsx <- df_angestellte_tg_section_tg_xlsx %>%
  filter(jahr >= min_year)

wb_kanton <- createWorkbook()
addWorksheet(wb_kanton, paste0("Kanton Thurgau (",min_year,"-",max_year, ")"))

create_table_style (
  wb = wb_kanton,
  sheet = paste0("Kanton Thurgau (",min_year,"-",max_year, ")"),
  header = "Wirtschaftsstruktur des Kantons Thurgau",
  subheader = paste0("Kanton Thurgau, ",min_year,"-",max_year),
  varnames = c("Jahr", "Noga Sektion Code", "Beschreibung", "Beschäftigte", "Standort-\nquotient", "Mittleres jährliches Wachstum (letzten 10 Jahre) in %"),
  data = df_angestellte_tg_section_tg_xlsx,
  datenquelle = "Bundesamt für Statistik, STATENT",
  gemeinde_format = FALSE,
  year = 1
)

setColWidths(wb_kanton, paste0("Kanton Thurgau (",min_year,"-",max_year, ")"), cols = 3, widths = 57)
setColWidths(wb_kanton, paste0("Kanton Thurgau (",min_year,"-",max_year, ")"), cols = 6, widths = 17)

saveWorkbook(wb_kanton, "Daten/daten_kanton.xlsx", overwrite = T)




df_angestellte_tg_section_bezirke_xlsx <- df_angestellte_tg_section_bezirke_xlsx %>%
  filter(jahr >= min_year)

wb_bezirk <- createWorkbook()
addWorksheet(wb_bezirk, paste0("Bezirke (",min_year,"-",max_year, ")"))

create_table_style (
  wb = wb_bezirk,
  sheet = paste0("Bezirke (",min_year,"-",max_year, ")"),
  header = "Wirtschaftsstruktur der Bezirke des Kantons Thurgau",
  subheader = paste0("Bezirke des Kantons Thurgau, ",min_year,"-",max_year),
  varnames = c("Jahr", "Bezirk", "Noga Sektion Code", "Beschreibung", "Beschäftigte", "Standort-\nquotient", "Mittleres jährliches Wachstum (letzten 10 Jahre) in %"),
  data = df_angestellte_tg_section_bezirke_xlsx,
  datenquelle = "Bundesamt für Statistik, STATENT",
  gemeinde_format = FALSE,
  year = 1
)


setColWidths(wb_bezirk, paste0("Bezirke (",min_year,"-",max_year, ")"), cols = 4, widths = 57)
setColWidths(wb_bezirk, paste0("Bezirke (",min_year,"-",max_year, ")"), cols = 7, widths = 17)


saveWorkbook(wb_bezirk, "Daten/daten_bezirk.xlsx", overwrite = T)



df_angestellte_tg_section_xlsx <- df_angestellte_tg_section_xlsx %>%
  filter(jahr >= min_year)


wb_gemeinde <- createWorkbook()
addWorksheet(wb_gemeinde, paste0("Gemeinde (",min_year,"-",max_year, ")"))

create_table_style (
  wb = wb_gemeinde,
  sheet = paste0("Gemeinde (",min_year,"-",max_year, ")"),
  header = "Wirtschaftsstruktur der Gemeinden des Kantons Thurgau",
  subheader = paste0("Gemeinden des Kantons Thurgau, ",min_year,"-",max_year),
  varnames = c("Jahr", "Gemeinde","Bezirk", "Noga Sektion Code", "Beschreibung", "Beschäftigte", "Standort-\nquotient", "Mittleres jährliches Wachstum (letzten 10 Jahre) in %"),
  data = df_angestellte_tg_section_xlsx,
  datenquelle = "Bundesamt für Statistik, STATENT",
  gemeinde_format = FALSE,
  year = 1
)

setColWidths(wb_gemeinde, paste0("Gemeinde (",min_year,"-",max_year, ")"), cols = 2, widths = 21)
setColWidths(wb_gemeinde, paste0("Gemeinde (",min_year,"-",max_year, ")"), cols = 5, widths = 57)
setColWidths(wb_gemeinde, paste0("Gemeinde (",min_year,"-",max_year, ")"), cols = 8, widths = 17)

saveWorkbook(wb_gemeinde, "Daten/daten_gemeinde.xlsx", overwrite = T)



df_angestellte_tg_c_xlsx <- df_angestellte_tg_c_xlsx %>%
  filter(jahr >= min_year)

wb_sektorC <- createWorkbook()
addWorksheet(wb_sektorC, paste0("Verarbeit. Gewerbe (",min_year,"-",max_year, ")"))

create_table_style (
  wb = wb_sektorC,
  sheet = paste0("Verarbeit. Gewerbe (",min_year,"-",max_year, ")"),
  header = "Verarbeitendes Gewerbe des Kantons Thurgau",
  subheader =paste0("Verarbeitendes Gewerbe im Kanton Thurgau, ",min_year,"-",max_year),
  varnames = c("Jahr", "Noga Zweisteller Code", "Beschreibung", "Beschäftigte", "Standort-\nquotient", "Mittleres jährliches Wachstum (letzten 10 Jahre) in %"),
  data = df_angestellte_tg_c_xlsx,
  datenquelle = "Bundesamt für Statistik, STATENT",
  gemeinde_format = FALSE,
  year = 1
)

setColWidths(wb_sektorC, paste0("Verarbeit. Gewerbe (",min_year,"-",max_year, ")"), cols = 3, widths = 57)
setColWidths(wb_sektorC, paste0("Verarbeit. Gewerbe (",min_year,"-",max_year, ")"), cols = 6, widths = 17)

saveWorkbook(wb_sektorC, "Daten/daten_sectorC.xlsx", overwrite = T)

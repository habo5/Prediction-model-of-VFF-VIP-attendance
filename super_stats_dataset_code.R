# Indlæs nødvendige biblioteker
pacman::p_load(tidyverse, palmerpenguins, ggthemes, ggridges, nycflights13, 
               Lahman, janitor, vroom, arrow, readxl, writexl, googlesheets4,
               RSQLite, babynames, stringi, gapminder, tidymodels, caret, 
               rvest, httr, jsonlite, rlist, rjson, Rcrawler, hrbrthemes, knitr,
               glmnet, hms, leaps, car, httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler, openxlsx)


#### Hent Superstats data

sl_tabs_alle_sæsoner <- list()
sl_tabs_alle_sæsoner_stilling <- list()

for (år in 2007:2023) { 
  url <- paste0("https://superstats.dk/program?aar=", år,"%2F", år+1) 
  sl_tabs_sæson <- read_html(url, encoding = "UTF-8") |> 
    html_elements("table") |> # Find alle <table> elementer
    html_table(header = FALSE, convert = FALSE) # Konverter dem til dataframes
  
  kanaler_sæson <- read_html(url, encoding = "UTF-8") |> 
    html_elements("img") |>   # Find alle <img> elementer relateret til én sæson år/år+1
    html_attr("alt")
  
  sl_tabs_valgte_sæson <- sl_tabs_sæson[1:(length(sl_tabs_sæson)-4)] # Fjern de sidste 4 tabeller, der ikke skal bruges
  kamps_data_sæson <- bind_rows(sl_tabs_valgte_sæson, .id = "Runde") |> # Kombiner runderne i én tabel for sæsonen år/år+1
    filter(!str_detect(X1, "Runde")) |> # Fjern rækker, der indeholder "Runde"
    mutate(X7 = kanaler_sæson) # Udfyld den tomme kolonne med de tilsvarende tv-kanaler fra kanaler_sæson   
  
  sl_tabs_alle_sæsoner[[paste0(år, "/", år+1)]] <- kamps_data_sæson # Tilføj sæsonens data til listen med alle sæsoners data
  
  # Stillingen af VFF i Superligaen kan påvirke VIP-tilskuertallet, så vi udtrækker stillingstabellen for hver sæson:
  
  sl_tabs_valgte_sæson_stilling <- sl_tabs_sæson[(length(sl_tabs_sæson)) - 2]
  sl_tabs_alle_sæsoner_stilling[[paste0(år, "/", år+1)]] <- sl_tabs_valgte_sæson_stilling[[1]] |> 
    slice(-1:-2) # Fjern de to første rækker, som indeholder unødvendige værdier
}


# Kombiner alle Superstats data i én samlet data frame
alle_kamps_data <- bind_rows(sl_tabs_alle_sæsoner, .id = "Sæson") |> # Kombiner data fra alle sæsoner
  rename(
    ugedag = X1, Dato = X2, kampholdene = X3, kampresultat = X4,
    samlet_tilskuertal = X5, dommer = X6, TV_kanal = X7 # Omdøb kolonnerne
  )

write.xlsx(alle_kamps_data, file = "superstats_kamps_data.xlsx")


# Udtræk stillingen for VFF og andre hold i hver sæson
alle_stillinger_data <- bind_rows(sl_tabs_alle_sæsoner_stilling, .id = "Sæson") # Kombiner stillingerne for alle sæsoner

write.xlsx(alle_stillinger_data, file = "hold_stillinger_data.xlsx")

alle_stillinger_data <- read_excel("hold_stillinger_data.xlsx")

hold <- alle_stillinger_data |> # Udtræk holdnavne
  distinct(X2) |> 
  pull() |> 
  str_remove("\\s*\\([^)]*\\)")

# Funktion til at udtrække stillingen for et specifikt hold
udtræk_stilling_for_hold <- function(hold) {
  alle_stillinger_data |>
    filter(str_detect(X2, regex(hold, ignore_case = TRUE))) |>
    select(Sæson, X1) |>
    rename(superliga_rang = X1)
}

stillinger_i_superliga <- lapply(hold, udtræk_stilling_for_hold) # Anvend funktionen på alle hold
names(stillinger_i_superliga) <- hold

stilling_vff <- stillinger_i_superliga[["VFF"]]


# Udtræk kun data for VFF og tilføj den tilsvarende stilling
alle_kamps_data <- read.xlsx("superstats_kamps_data.xlsx")

super_stats_data <- alle_kamps_data |>
  filter(str_detect(kampholdene, regex("vff", ignore_case = TRUE))) |>
  left_join(stilling_vff, by = "Sæson")


# Oprydning og formatering af datoer og tidspunkter i Superstats data
super_stats_dataset <- super_stats_data |> 
  separate_wider_delim(Dato, delim = " ", names = c("Dato", "tidspunkt")) |> 
  mutate(
    Dato = case_when(
      as.numeric(str_extract(Dato, "\\d+$")) >= 7 ~ str_c(Dato, "/", str_extract(Sæson, "\\d+")),
      as.numeric(str_extract(Dato, "\\d+$")) < 7 ~ str_c(Dato, "/", str_extract(Sæson, "\\d+$"))
    ),
    Dato = as.Date(Dato, format = "%d/%m/%Y"),
    tidspunkt = as_hms(ymd_hm(paste(Dato, str_replace(tidspunkt, "\\.", ":")))),
    år = year(Dato)
  )


# Tilføj stillingen for modstanderhold i Superligaen
super_stats_dataset <- super_stats_dataset |> 
  mutate(
    modstanderhold = str_replace(kampholdene, regex("(VFF\\s*-\\s*|\\s*-\\s*VFF)", ignore_case = TRUE), ""),
    modstanderhold_stilling = NA
  )


for (i in seq_along(super_stats_dataset$modstanderhold)) {
  modstanderhold <- super_stats_dataset$modstanderhold[i]
  sæson <- super_stats_dataset$Sæson[i]
  
  # Udtræk stillingstabellen for det nuværende modstanderhold
  pos_tabel <- stillinger_i_superliga[[modstanderhold]]
  
  super_stats_dataset$modstanderhold_stilling[i] <- pos_tabel$superliga_rang[pos_tabel$Sæson == sæson]
}



# Rensning af Superstats dataset
super_stats_dataset <- super_stats_dataset |>
  mutate(
    across(
      c(superliga_rang, modstanderhold_stilling), 
      ~ str_remove(., ".$") # Fjern punktum fra stillingsværdier
    ),
    superliga_rang = as.double(superliga_rang),
    modstanderhold_stilling = as.double(modstanderhold_stilling),
    samlet_tilskuertal = as.double(samlet_tilskuertal)
  )


# Funktion til at tilføje kumulative statistikker for VFF
tilføj_kumulative_stats_vff <- function(data, resultater_kolonne, kamphold_kolonne) {
  data %>%
    group_by(Sæson) %>%
    mutate(
      # Udtræk mål scoret af VFF og modstanderen
      vff_mål = ifelse(str_detect(!!sym(kamphold_kolonne), regex(paste0("^", "vff"), ignore_case = TRUE)),# Identificer om VFF spiller hjemme eller ude
                       as.numeric(str_extract(!!sym(resultater_kolonne), "\\d+")), # VFF hjemme
                       as.numeric(str_extract(!!sym(resultater_kolonne), "\\d+$"))), # VFF ude
      modstander_mål = ifelse(str_detect(!!sym(kamphold_kolonne), regex(paste0("^", "vff"), ignore_case = TRUE)),# Identificer om VFF spiller hjemme eller ude
                              as.numeric(str_extract(!!sym(resultater_kolonne), "\\d+$")), # Modstander ude
                              as.numeric(str_extract(!!sym(resultater_kolonne), "\\d+"))), # Modstander hjemme
      
      # Bestem kampresultat for VFF
      vff_resultat = case_when(
        vff_mål > modstander_mål ~ "vundet",
        vff_mål < modstander_mål ~ "tabt",
        vff_mål == modstander_mål ~ "uafgjort"
      ),
      
      # Kumulative statistikker for VFF
      kumulative_vff_mål = cumsum(vff_mål),
      kumulative_vff_sejre = cumsum(vff_resultat == "vundet"),
      kumulative_vff_tab = cumsum(vff_resultat == "tabt"),
      kumulative_vff_uafgjorte = cumsum(vff_resultat == "uafgjort"),
      kumulative_vff_point = (kumulative_vff_sejre * 3) + kumulative_vff_uafgjorte
    ) %>%
    ungroup()
}

super_stats_dataset <- tilføj_kumulative_stats_vff(super_stats_dataset, "kampresultat", "kampholdene")


# Tilføj kolonner for "lagged" kumulative statistikker
super_stats_dataset <- super_stats_dataset |>
  mutate(
    across(
      c(kumulative_vff_mål, kumulative_vff_sejre, kumulative_vff_tab, kumulative_vff_uafgjorte, kumulative_vff_point, superliga_rang, modstanderhold_stilling),
      ~ lag(., default = 0), # Tilføj lag til hver valgt kolonne
      .names = "{.col}_lag"  # Tilføj "_lag" som suffix til nye kolonner
    )
  )


# Gruppér tv-kanalerne og bevar kun de mest populære
super_stats_dataset <- super_stats_dataset |>
  mutate(TV_kanal = fct_lump_n(TV_kanal, n = 3))

table(super_stats_dataset$TV_kanal)


# Opret en kolonne, der kombinerer tidspunkt og dagtype (arbejdsdag eller weekend)
super_stats_dataset <- super_stats_dataset |>
  mutate(
    dag_tid_kategori = case_when(
      ugedag %in% c("Lør", "Søn") & parse_hms(tidspunkt) < parse_hms("16:00:00") ~ "weekend før fire",
      ugedag %in% c("Lør", "Søn") & parse_hms(tidspunkt) >= parse_hms("16:00:00") ~ "weekend efter fire",
      !ugedag %in% c("Lør", "Søn") & parse_hms(tidspunkt) < parse_hms("16:00:00") ~ "hverdag før fire",
      !ugedag %in% c("Lør", "Søn") & parse_hms(tidspunkt) >= parse_hms("16:00:00") ~ "hverdag efter fire"
    ),
    dag_tid_kategori = factor(dag_tid_kategori) # Konverter til faktor
  ) |>
  relocate(dag_tid_kategori, .after = tidspunkt)


# Filtrer kun data, hvor VFF spiller hjemme, og tilføj tidligere samlet tilskuertal
super_stats_dataset <- super_stats_dataset |>
  filter(str_starts(kampholdene, regex("vff", ignore_case = TRUE))) |>
  mutate(samlet_tilskuertal_lag = lag(samlet_tilskuertal)) |> 
  slice(-1)



# Tilføj en kolonne, der angiver, om kampdagen er en ferie (sommerferie, vinterferie, efterårsferie)

# Der er ingen kilde eller webservice tilgængelig til at hente officielle skoleferier i Viborg for hele perioden med kampdata. 
# Derfor har vi oprettet en liste med feriedatoer baseret på typiske ferieperioder.

# Sommerferien i Viborg starter normalt den sidste lørdag i juni og slutter den anden søndag i august. 
# Vi samler alle tilsvarende datoer (sommerferiestart og -slut) for hvert år i dataset.

anden_søndag_i_august <- as.Date(character())

# Loop gennem årene 2007 til 2024
for (år in 2007:2024) {
  # Generér alle datoer i august for det pågældende år
  august_datoer <- seq(as.Date(paste0(år, "-08-01")), 
                       as.Date(paste0(år, "-08-31")), 
                       by = "1 day")
  
  # Filtrer søndage i august
  søndage <- august_datoer[wday(august_datoer) == 1] # 1 svarer til søndag
  
  # Gem den anden søndag og tilføj den til listen
  anden_søndag_i_august <- c(anden_søndag_i_august, søndage[2])
}

sidste_lørdag_i_juni <- as.Date(character())

# Loop gennem årene 2007 til 2024
for (år in 2007:2024) {
  # Generér alle datoer i juni for det pågældende år
  juni_datoer <- seq(as.Date(paste0(år, "-06-01")), 
                     as.Date(paste0(år, "-06-30")), 
                     by = "1 day")
  
  # Filtrer lørdage i juni
  lørdage <- juni_datoer[wday(juni_datoer) == 7] # 7 svarer til lørdag
  
  # Gem den sidste lørdag og tilføj den til listen
  sidste_lørdag_i_juni <- c(sidste_lørdag_i_juni, tail(lørdage, 1))
}

sommerferie_liste <- tibble(
  år = c(2007:2024),
  start_dato = sidste_lørdag_i_juni,
  slut_dato = anden_søndag_i_august
)

super_stats_dataset <- super_stats_dataset |>
  left_join(sommerferie_liste, by = c("år" = "år")) |> # Tilføj sommerferiestart og -slutdatoer for hvert år
  mutate(
    sommerferie = if_else(Dato >= start_dato & Dato <= slut_dato, 1, 0), # Tilføj kolonne med 1 eller 0 for sommerferie
    vinterferie = if_else(isoweek(Dato) == 7, 1, 0), # Vinterferien i Viborg er normalt uge 7
    efterårsferie = if_else(isoweek(Dato) == 42, 1, 0)) |># Efterårsferien er normalt uge 42
  select(-start_dato, -slut_dato) |># Fjern kolonner, der ikke længere er nødvendige
  rowwise() |>
  mutate(
    ferie = if_else(
      any(across(c(sommerferie, vinterferie, efterårsferie)) == 1), 
      1, 
      0  
    )
  ) |> ungroup()


write.xlsx(super_stats_dataset, file = "super_stats_dataset.xlsx")

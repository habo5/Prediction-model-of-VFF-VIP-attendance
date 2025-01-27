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
super_stats_dataset <- read_excel("super_stats_dataset.xlsx")



# Læs og ryd op i guld-data
guld_data <- read_excel("data/guld.xlsx")

VIP_tilskuertal_dataset <- guld_data |>
  filter(!is.na(Guld_menu_stk)) |>
  select(Dato, Guld_menu_stk) |>
  mutate(
    Dato = if_else(
      !str_detect(Dato, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$"), # Hvis datoen ikke er i formatet dd.mm.åååå
      as.Date(as.numeric(Dato), origin = "1899-12-30"), # Konverter Excel-datoer til R-datoer
      as.Date(Dato, format = "%d.%m.%Y") # Konverter normalt format
    )
  )

write.xlsx(VIP_tilskuertal_dataset, file = "VIP_tilskuertal_dataset.xlsx")



# Kombinér guld-data og Superstats data ved hjælp af datoer

super_stats_dataset <- read_excel("super_stats_dataset.xlsx")
VIP_tilskuertal_daaset <- read_excel("VIP_tilskuertal_dataset.xlsx")

superstat_guld_dataset <- super_stats_dataset |>
  inner_join(VIP_tilskuertal_dataset, by = "Dato")




# Opret dataset for vejrdata baseret på kampdatoer i superstat_guld_dataset

vejr_data<-tibble(
  Dato = superstat_guld_dataset$Dato,
  tid = superstat_guld_dataset$tidspunkt
  
)

match_dag_vejr<- function(dato,tid){
  base_url <- "https://dmigw.govcloud.dk/v2/"
  info_url <- "metObs/collections/observation/items?"
  req_url <- paste0("bbox=9.1030,56.2037,9.3328,56.3146&datetime=", dato, "T", tid, "Z")#bbox localizes viborg with a box of coordinates ...
  api_key <- "&api-key=a5746ebe-9206-4e40-9ad0-1e4867107729"
  
  
  full_url <- base::paste0(base_url, info_url, req_url, api_key)
  
  ## call API
  api_call <- httr::GET(full_url)# her sender vi så et GET request, og samtidig 
  # sender vi vores full_url med
  http_type(api_call) # her tester vi, hvad vi får tilbage, og det skal være 
  # application/json 
  ## API response
  api_call$status_code # hvis vi får status code 200, så har vi kontakt til api'et 
  #Hvis vi får  status code over 400, er der sket en fejl
  api_call$content# her kan vi se, om der kommer noget med tilbage,
  # og det ses som hexadecimal
  api_char <- base::rawToChar(api_call$content) #her laver vi data til char 
  # med library(rjson)
  api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)# her bruger vi library(jsonlite) 
  if (is.null(api_JSON$features) || length(api_JSON$features) == 0) {
    return(NA)
  }
  list_dmi <- api_JSON# her laver vi vores JSON objekt om til en list
  
  col1<-as.vector(api_JSON$features[6])# Vi kan hente data direkte ved hjælp af JSON-data transformeret til en liste
  col2<-as.vector(api_JSON$features[7])
  col3<-as.vector(api_JSON$features[9])
  if (length(col1) == 0 || length(col2) == 0 || length(col3) == 0) {
    return(NA)
  }
  add_cols <- c(col1, col2, col3)
  dmi <- as_tibble(add_cols)
  
  dmi <- dmi |>   
    rename("Observationstidspunkt" ="properties.observed", "Observationer" = "properties.parameterId", "Value" = "properties.value" )
  
  wide_dmi <- dmi|>filter(grepl("temp_dry|wind_speed|visib|precip", Observationer, ignore.case = TRUE))|>
    pivot_wider(names_from = Observationer, values_from = Value)   
  
}
## Denne funktion returnerer en tabel med vejrparametre for en specifik dato/tid kombination


vejr_data_list <- list()# Funktionen anvendes på datoerne i superstat_guld_dataset for at hente vejrdata på de nødvendige datoer

for (i in 1:nrow(vejr_data)) {
  dato <- vejr_data$Dato[i]
  tid <- vejr_data$tid[i]
  
  # Hent vejrdata for en specifik dato/tid
  vejr_data_pr_dag_tid <- match_dag_vejr(dato, tid)
  
  
  if (is.null(vejr_data_pr_dag_tid) || all(is.na(vejr_data_pr_dag_tid))){
    afrundet_tid <- floor_date(ymd_hms(paste0(dato,tid)), "hour")  # Afrund til nærmeste hele time
    tid <- format(afrundet_tid, "%H:%M:%S")
    vejr_data_pr_dag_tid <- 
      match_dag_vejr(dato, tid)
    
  }
  
  # Gem data i listen
  vejr_data_list[[paste0(dato, "/", tid)]] <- vejr_data_pr_dag_tid

}


# Kombinér alle tabeller til én samlet tabel
vejr_tabel <- bind_rows(vejr_data_list)

write.xlsx(vejr_tabel, file = "alle_vejr_data.xlsx")


alle_vejr_data <- read_excel("alle_vejr_data.xlsx")

# Ved gennemgang af datasættet ser det ud til, at de eneste parametre, der er tilgængelige for alle datoer (uden NA), er: precip_past10min, visib_mean_last10min, wind_speed og temp_dry

vejr_dataset<-vejr_tabel|>
  select(precip_past10min,visib_mean_last10min,wind_speed , temp_dry,Observationstidspunkt)|>
  separate_wider_delim(Observationstidspunkt, delim = "T", names = c("Dato", "Tid"))|>
  mutate(Dato = as.Date(Dato))|>
  select(-Tid)|>
  relocate(Dato,.before=everything())


write.xlsx(vejr_dataset, file = "nødvendige_vejr_data.xlsx")

# Gem vejrdata i en Excel-fil for at undgå gentagelse af løkken

vejr_data <- read_excel("nødvendige_vejr_data.xlsx")

# Kombinér Superstats og vejrdata
superstat_guld_vejr_dataset <- superstat_guld_dataset |>
  left_join(vejr_data, by = "Dato")

write.xlsx(superstat_guld_vejr_dataset, file = "superstat_guld_vejr_dataset.xlsx")

sidste_dataset <- read_excel("superstat_guld_vejr_dataset.xlsx")

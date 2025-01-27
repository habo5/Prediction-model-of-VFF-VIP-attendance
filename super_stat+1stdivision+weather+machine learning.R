# Indlæs nødvendige biblioteker
pacman::p_load(tidyverse, palmerpenguins, ggthemes, ggridges, nycflights13, 
               Lahman, janitor, vroom, arrow, readxl, writexl, googlesheets4,
               RSQLite, babynames, stringi, gapminder, tidymodels, caret, 
               rvest, httr, jsonlite, rlist, rjson, Rcrawler, hrbrthemes, knitr,boot,reshape2,
               glmnet, hms, leaps, car,   rjstat, Rcrawler, openxlsx)


#### Hent 1st division data

first_division_data <- read_excel("data/1st division.xlsx")
str(first_division_data)



convert_result <- function(result) {
  # Check if result does not contain a dash, suggesting it's a numeric serial date
  if (!str_detect(result, "-")) {
    # Convert serial date to Date object
    date_result <- as.Date(as.numeric(result), origin = "1899-12-30")
    # Extract month and day, and format them without leading zeros manually
    month <- as.integer(format(date_result, "%m"))
    day <- as.integer(format(date_result, "%d"))
    # Construct the "month-day" string without leading zeros
    return(paste(month, day, sep=" - "))
  } else {
    # Return result as is if it's not a numeric serial
    return(result)
  }
}

first_division_data<- first_division_data |>
  mutate(kampresultat = sapply(kampresultat, convert_result))


super_ligua_data<- read_excel("super_stats_dataset.xlsx")

super_ligua_data<- super_ligua_data|>
  select(ugedag,Dato,superliga_rang,år,modstanderhold_stilling,kampresultat)|>
  rename(`VFF-stilling`= superliga_rang)


super_ligua_data<- super_ligua_data|>
  mutate(across(c(modstanderhold_stilling, `VFF-stilling`), as.character))

first_division_data<- first_division_data |>
  mutate(
   Dato=as.Date(Dato),
    ugedag=  factor(weekdays(Dato),levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),

   
   år=year(Dato)
)|>
  arrange(Dato)

levels(first_division_data$ugedag)
levels(first_division_data$ugedag)<- c("Søn", "Man", "Tir", "Ons", "Tor", "Fre", "Lør")

str(first_division_data)

str(super_ligua_data)
first_division_data<- first_division_data |>
  
  select(-modstand)|>
  filter(`VFF-stilling` == "1.D")



VFF_dataset<-bind_rows(super_ligua_data,first_division_data)

# Funktion til at tilføje kumulative statistikker for VFF

VFF_dataset<-VFF_dataset |>
  arrange(Dato)|>
    mutate(
      # Udtræk mål scoret af VFF og modstanderen
      vff_mål = as.numeric(str_extract(kampresultat, "\\d+")),
                       
      modstander_mål =  as.numeric(str_extract(kampresultat, "\\d+$")),
                            
      
      # Bestem kampresultat for VFF
      vff_resultat = case_when(
        vff_mål > modstander_mål ~ "vundet",
        vff_mål < modstander_mål ~ "tabt",
        vff_mål == modstander_mål ~ "uafgjort"
      ),
      
    
       
      vundet = if_else(vff_resultat == "vundet", 1, 0),
      tabt = if_else(vff_resultat == "tabt", 1, 0),
      uafgjort = if_else(vff_resultat == "uafgjort", 1, 0))



  

n=nrow(VFF_dataset)

cum_3<-function(result) {
  sums_3<-numeric(n)
 
  for (i in 1:(n-3)) {
    j<-i+3
   sums_3[j]<-sum(result[(j-3):(j-1)])
    
  }
  return(sums_3)
  }

win_ratio_3<- cum_3(VFF_dataset$vundet)/3
loss_ratio_3 <- cum_3(VFF_dataset$tabt)/3
draw_ratio_3 <- cum_3(VFF_dataset$uafgjort)/3
         

VFF_dataset<-VFF_dataset |>
  mutate(win_ratio_3=win_ratio_3,
         loss_ratio_3=loss_ratio_3,
         draw_ratio_3=draw_ratio_3)

VFF_dataset<- VFF_dataset[-c(1,2,3), ]



#make levels for stilling

  VFF_dataset <- VFF_dataset|>
  mutate(
    across(c(`VFF-stilling`, modstanderhold_stilling),
           ~ case_when(
             is.na(as.numeric(.)) ~ "1st_division",  # Assume non-numeric implies special category
             as.numeric(.) < 5   ~ "top_superligua",
             as.numeric(.) >= 5 & as.numeric(.) <= 8 ~ "medium_superligua",
             as.numeric(.) >= 9  ~ "low_superligua",
          
           ))
    
  )|>
    mutate(
      across(c(`VFF-stilling`, modstanderhold_stilling),
             ~ as.factor(.)
             ))|>
  mutate(
    dag_kategori = as.factor(if_else(ugedag %in% c("Lør", "Søn") , "weekend ","hverdag"))
      
    )
str(  VFF_dataset)




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

VFF_dataset <- VFF_dataset |>
  left_join(sommerferie_liste, by = c("år" = "år")) |> # Tilføj sommerferiestart og -slutdatoer for hvert år
  mutate(
    sommerferie = if_else(Dato >= start_dato & Dato <= slut_dato, 1, 0), # Tilføj kolonne med 1 eller 0 for sommerferie
    vinterferie = if_else(isoweek(Dato) == 7, 1, 0), # Vinterferien i Viborg er normalt uge 7
    efterårsferie = if_else(isoweek(Dato) == 42, 1, 0)) |># Efterårsferien er normalt uge 42
  select(-start_dato, -slut_dato) |># Fjern kolonner, der ikke længere er nødvendige
  rowwise() |>
  mutate(
    ferie = as.factor(case_when(
      sommerferie == 1  ~ "sommerferie",
       vinterferie == 1  ~ "vinterferie",
      efterårsferie == 1  ~ "efterårsferie",
      TRUE ~ "ikke_ferie" 
    )
    )) |>
  select(-sommerferie, -vinterferie, -efterårsferie)|> ungroup()

VIP_tilskuertal_dataset <- read_excel("VIP_tilskuertal_dataset.xlsx")

VFF_guld_dataset <- VFF_dataset |>
  inner_join(VIP_tilskuertal_dataset, by = "Dato")


# Opret dataset for vejrdata baseret på kampdatoer i superstat_guld_dataset

vejr_data<-tibble(
  Dato = VFF_guld_dataset$Dato
)

match_dag_vejr<- function(dato){
  base_url <- "https://dmigw.govcloud.dk/v2/"
  info_url <- "metObs/collections/observation/items?"
  req_url <- paste0("bbox=9.1030,56.2037,9.3328,56.3146&datetime=", dato, "T", "17:00:00", "Z")#bbox localizes viborg with a box of coordinates ...
  api_key <- "use your API key "
  
  
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
  
  
  # Hent vejrdata for en specifik dato
  vejr_data_pr_dag<- match_dag_vejr(dato)

  # Gem data i listen
  vejr_data_list[[paste0(dato)]] <- vejr_data_pr_dag
  
}


# Kombinér alle tabeller til én samlet tabel
vejr_tabel <- bind_rows(vejr_data_list)

vejr_dataset<-vejr_tabel|>
  select(precip_past10min,visib_mean_last10min,wind_speed , temp_dry,Observationstidspunkt)|>
  separate_wider_delim(Observationstidspunkt, delim = "T", names = c("Dato", "Tid"))|>
  mutate(Dato = as.Date(Dato))|>
  select(-Tid)|>
  relocate(Dato,.before=everything())


# Kombinér Superstats og vejrdata
VFF_guld_vejr_dataset  <- VFF_guld_dataset  |>
  left_join(vejr_dataset, by = "Dato")
VFF_guld_vejr_dataset <- VFF_guld_vejr_dataset |>
  mutate(rain = ifelse(precip_past10min > 0.0,1, 0))


ml_dataset <- VFF_guld_vejr_dataset %>%
  select(år, `VFF-stilling`, modstanderhold_stilling, ferie, rain, temp_dry, wind_speed,
         Guld_menu_stk, win_ratio_3,loss_ratio_3, draw_ratio_3, dag_kategori) %>%
  filter(complete.cases(.))

str(ml_dataset)
ml <- lm(Guld_menu_stk~ ., data = ml_dataset)
summary(ml)

par(mfrow = c(2, 2))
plot(ml)




adjusted_dataset <-ml_dataset[-c(73,221),]
adjusted_dataset <-adjusted_dataset|>filter(år!=2020)
adjusted_ml <- lm(Guld_menu_stk ~ ., data = adjusted_dataset)




ggplot(adjusted_dataset, aes(x= år, y=Guld_menu_stk)) + 
  geom_point() + 
  geom_smooth(method="lm",formula = y ~ poly(x, 4), col="blue")+
  labs(
    x = "år^4",
    y = "y"
  ) 





# Assuming 'adjusted_dataset' is already loaded and preprocessed
adjusted_dataset <- adjusted_dataset %>%
  mutate(
    år_new= I(år^4),
    
    
    # Interaction term
    dag_kategori_ferie = interaction(dag_kategori, ferie)
  )

adjusted_ml <- lm(Guld_menu_stk ~ ., data = adjusted_dataset)


par(mfrow = c(2, 2))
plot(adjusted_ml)

# Prepare matrix for glmnet
x <- model.matrix(Guld_menu_stk ~ . - 1, data = adjusted_dataset)  # -1 to exclude intercept
y <- adjusted_dataset$Guld_menu_stk
set.seed(1)

train <- sample(1:nrow(x), nrow(x) *2/3)#2/3 to train og 1/3 test
length(train)
test <- (-train)

grid <- 10^seq(10, -2, length = 100)

#lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, # alpha nu lig med 1
                    lambda = grid)
set.seed(1)
cv.out.lasso <- cv.glmnet(x[train, ], y[train], alpha = 1,lambda=grid,nfolds=10)
bestlam.lasso <- cv.out.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam.lasso,
                      newx = x[test, ])

mse_lasso_test <- mean((lasso.pred - y[test])^2)
rmse_lasso_test <-sqrt(mse_lasso_test )
lasso_mse_cv <- min(cv.out.lasso$cvm)
rmse_lasso_cv<-sqrt(lasso_mse_cv )


# Her fittes modellen til ALLE data

out.lasso <- glmnet(x, y, alpha = 1)
lasso.coef <- predict(out.lasso, type = "coefficients",
                      s = bestlam.lasso)[1:26, ]
lasso.coef 

length(lasso.coef[lasso.coef != 0])#11 remaining
prædiktorer_antal_lasso<-length(lasso.coef[lasso.coef != 0]) 

predicted_lasso <- predict(out.lasso, s = bestlam.lasso, newx = x)
res_lasso <- sum((y - predicted_lasso)^2)
tss <- sum((y - mean(y))^2)
r_squared_lasso <- 1 - (res_lasso / tss)

r_squared_lasso # 0.67

str(adjusted_dataset)
#Ridge regression

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid)
set.seed(1)
cv.out.ridge <- cv.glmnet(x[train, ], y[train],nfolds = 7, alpha = 0,lambda=grid) 
bestlam.ridge <- cv.out.ridge$lambda.min
cv.out.ridge

bestlam.ridge 

ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x[test, ])
mse_ridge_test <- mean((ridge.pred - y[test])^2)
ridge_mse_cv <- min(cv.out.ridge$cvm)

rmse_ridge_test <- sqrt(mse_ridge_test )
rmse_ridge_cv<-sqrt(ridge_mse_cv)

# Her fittes modellen til ALLE data

out.ridge <- glmnet(x, y, alpha = 0)
ridge.coef<-predict(out.ridge, type = "coefficients", s = bestlam.ridge)[1:26, ]
ridge.coef
prædiktorer_antal_ridge <- dim(x)[2]+1 #here beregner vi den intercept som coefficient Betta0

predicted_Ridge <- predict(out.ridge, s = bestlam.ridge, newx = x)
res_Ridge <- sum((y - predicted_Ridge)^2)
tss <- sum((y - mean(y))^2)
r_squared_Ridge <- 1 - (res_Ridge / tss)

r_squared_Ridge # 0. 67 

# Best subset selection

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

adjusted_dataset_train <- adjusted_dataset[train,]
adjusted_dataset_test <- adjusted_dataset[test,]

k <- 10 # Vi danner 10 folds
n <- nrow(adjusted_dataset_train) # registrerer hvor mange observationer, vi har.
set.seed(1) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim<-dim(adjusted_dataset_train)[2]  

cv.errors <- matrix(NA, k, dim,
                    dimnames = list(NULL, paste(1:dim)))
cv.errors


for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(Guld_menu_stk ~ .,
                         data = adjusted_dataset_train[folds != j, ],
                         nvmax = dim)
  for (i in 1:dim) { # her gennemløbes alle kandidatmodeller
    pred <- predict(best.fit, adjusted_dataset_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((adjusted_dataset_train$Guld_menu_stk[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
optimal<-which.min(mean.cv.errors)



# Her fittes modellen til ALLE træningsdata
reg.best <- regsubsets(Guld_menu_stk ~ ., data = adjusted_dataset_train,
                       nvmax = dim)
coef(reg.best, optimal)



pred_best_subset <- predict(reg.best, adjusted_dataset_test, id = optimal)


mse_best_subset <- mean((adjusted_dataset_test$Guld_menu_stk - pred_best_subset)^2)
rmse_bestsubset_test  <- sqrt(mse_best_subset )
rmse_bestsubset_cv  <- sqrt(min(mean.cv.errors))


# Her fittes modellen til ALLE dataset

reg.best <- regsubsets(Guld_menu_stk ~ ., data = adjusted_dataset,
                       nvmax = dim)
coef(reg.best, optimal)

pred_best_subset <- predict(reg.best, adjusted_dataset, id = optimal)

res_best <- sum((y - pred_best_subset)^2)
tss <- sum((y - mean(y))^2)
r_squared_best <- 1 - (res_best / tss)

r_squared_best


coef(reg.best, optimal)==0
normalized_rmse_train_ridge <- rmse_ridge_cv / sd(y[train])
normalized_rmse_test_ridge <- rmse_ridge_test / sd(y[test])

normalized_rmse_train_subset <- rmse_bestsubset_cv / sd(y[train])
normalized_rmse_test_subset <- rmse_bestsubset_test / sd(y[test])

normalized_rmse_train_lasso <- rmse_lasso_cv / sd(y[train])
normalized_rmse_test_lasso <- rmse_lasso_test / sd(y[test])


str(adjusted_dataset)
table(adjusted_dataset$dag_kategori_ferie)


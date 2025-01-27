
pacman::p_load(tidyverse, palmerpenguins, ggthemes, ggridges, nycflights13, 
               Lahman, janitor, vroom, arrow, readxl, writexl, googlesheets4,
               RSQLite, babynames, stringi, gapminder, tidymodels, caret, 
               rvest, httr, jsonlite, rlist, rjson, Rcrawler, hrbrthemes, knitr,boot,reshape2,
               glmnet, hms, leaps, car,   rjstat, Rcrawler, openxlsx)
# machine learning ---------------------------------------------------------

final_dataset<-read_excel("superstat_guld_vejr_dataset.xlsx")
str(final_dataset)
names(final_dataset)

#forvalg af x variable............................

#Her undersøger vi korrelationen mellem variabler, der sandsynligvis er korrelerede, inklusive y-variablen, for at identificere, hvilken variabel der har størst indflydelse på y-variablen.

final_dataset |>
  select(Guld_menu_stk, kumulative_vff_mål_lag,kumulative_vff_sejre_lag,kumulative_vff_tab_lag,
         kumulative_vff_uafgjorte_lag,kumulative_vff_point_lag)  |>cor()

#kumulative_vff_sejre_lag og kumulative_vff_tab_lag er de variabler, der er mest korrelerede med y-variablen, og de er ikke stærkt korrelerede med hinaden.

final_dataset |>
  select(Guld_menu_stk ,ferie,sommerferie,efterårsferie,vinterferie)|>
  cor()


# Det ser ud til, at efterårsferie ikke har nogen indflydelse på y-variablen (lav korrelation). Det ser også ud til, at ferie er stærkt korreleret med sommerferie. Vi kan overveje at fjerne disse to variabler(ferie + efterårferie)

#Vi vil starte vores maskinlæringsanalyse med multipel lineær regression ved at bruge både et stort datasæt og et mindre, hvor nogle prediktorer er fjernet baseret på korrelationsanalysen. Derefter vil vi sammenligne de to modeller.
ml_dataset<-final_dataset|>
  select( dag_tid_kategori,Runde,TV_kanal,år,kumulative_vff_mål_lag,kumulative_vff_sejre_lag,kumulative_vff_tab_lag,
          kumulative_vff_uafgjorte_lag,kumulative_vff_point_lag, superliga_rang_lag,
          modstanderhold_stilling_lag,samlet_tilskuertal_lag,sommerferie,
          vinterferie,efterårsferie,ferie,Guld_menu_stk,precip_past10min,visib_mean_last10min,
          wind_speed,temp_dry )|> 
  mutate(
    TV_kanal = as.factor(TV_kanal),
    dag_tid_kategori = as.factor(dag_tid_kategori),
    Runde=as.double(Runde)
  ) 



str(ml_dataset)



# Beregn korrelationsmatricen
cor_matrix <- ml_dataset %>%
  select(-dag_tid_kategori, -TV_kanal) %>%
  cor()

# Tag den absolutte værdi af korrelationsmatrixen
cor_matrix_abs <- abs(cor_matrix)


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri<-get_upper_tri (cor_matrix_abs)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="abs(Pearson\nCorrelation)") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 7, hjust = 1,face = "bold"))+
  theme(axis.text.y = element_text( vjust = 1, 
                                   size = 7, hjust = 1,face = "bold"))
  coord_fixed()

 
  
  
  
  
  sportslige_korrelationer <- final_dataset |>
    select(Guld_menu_stk, kumulative_vff_mål_lag, kumulative_vff_sejre_lag, 
           kumulative_vff_tab_lag, kumulative_vff_uafgjorte_lag, kumulative_vff_point_lag) |>
    cor()
  
  #kumulative_vff_sejre_lag og kumulative_vff_tab_lag er de variabler, der er mest korrelerede med y-variablen, og de er ikke stærkt korrelerede med hinaden.
  


  correlation_sportelige_df <- melt(sportslige_korrelationer)
  
  #Plot heatmap
  
  # Generer heatmap
  ggplot(data = correlation_sportelige_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "Korrelation"
    ) +
    theme_minimal(base_size = 10) +
    coord_fixed() +
    labs(
      title = "Korrelation mellem sportslige variabler og Guld_menu_stk",
      x = "",
      y = ""
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 10, face = "bold")
    )
  
  
  
  
  
  # Korrelation mellem ferievariabler og Guld_menu_stk
  ferie_korrelationer <- final_dataset |>
    select(Guld_menu_stk, sommerferie, efterårsferie, vinterferie,ferie) |>
    cor()
 
  ferie_korrelationer <- matrix(c(
    1.000000000, -0.16134686,  -0.007492807,  0.14031739, -0.1084370,
    -0.161346865,  1.00000000,  -0.066380455, -0.03792761,  0.8456455,
    -0.007492807, -0.06638045,   1.000000000, -0.01804317,  0.4022960,
    0.140317391, -0.03792761,  -0.018043175,  1.00000000,  0.2298588,
    -0.108437019,  0.84564548,   0.402296042,  0.22985876,  1.0000000
  ), nrow = 5, byrow = TRUE)
  
  # Navngiv rækker og kolonner
  rownames(ferie_korrelationer) <- c("Guld_menu_stk", "sommerferie", "efterårsferie", "vinterferie","ferie")
  colnames(ferie_korrelationer) <- rownames(ferie_korrelationer)
  
  # Konverter matrix til data frame for ggplot
  ferie_korrelationer_df <- melt(ferie_korrelationer)
  
  # Plot heatmap
  ggplot(data = ferie_korrelationer_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Korrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=8)) +
    coord_fixed() +
    labs(title = "Korrelation mellem ferievariabler og Guld_menu_stk",
         x = "", y = "")+
    theme(

      plot.title = element_text(size = 10, face = "bold")
    )

#multiple linear regression

complex_ml<- lm(Guld_menu_stk~ . , data = ml_dataset)
summary(complex_ml)

ml_dataset_lille<-ml_dataset|>
  select(
    -ferie,-kumulative_vff_point_lag,-efterårsferie
  )# ferie,kumulative_vff_point_lag har en høj colinearity og de kunne ikke beregnes med linear regression de er så fjernet





ml <- lm(Guld_menu_stk~ ., data = ml_dataset_lille)

summary(ml)
vif(ml)# Runde  har viser en relativt høj VIF-værdi, hvilket potentielt indikerer moderat multikollinearitet
#kumulative_vff_mål_lag, kumulative_vff_uafgjorte_lag og kumulative_vff_sejre_lag har høj kolinearitet , men kumulative_vff_sejre_lag har en god correlation med y variable så er det bedre at beholde den



ml_dataset_lille<-ml_dataset_lille|>
  select(
    -kumulative_vff_uafgjorte_lag,-kumulative_vff_mål_lag
  )


str(ml_dataset_lille)
simple_ml <- lm(Guld_menu_stk~ ., data = ml_dataset_lille)

summary(simple_ml)
vif(simple_ml)#ikke mere høj kollinearitet

# før at anvende regularisering for at vælge de passende forudsigelser, lad os tjekke outliers og leverages
par(mfrow = c(2, 2))
plot(simple_ml)

#Residualerne er generelt spredt omkring den horisontale linje ved 0, hvilket indikerer god linearitet. 
#En polynomisk model eller ikke-lineær transformation (xi^2,xi^3...) er sandsynligvis unødvendig.
#Observation 7 ser dog ud til at være en indflydelsesrig outlier og bør undersøges nøje.



#vi vil ikke fjerne den nu. I stedet vil vi udføre regularisering og derefter sammenligne ydeevnen for den valgte model med og uden denne observation


x <- model.matrix(Guld_menu_stk ~ . , ml_dataset_lille)[,-1]
y<-ml_dataset_lille$Guld_menu_stk
set.seed(1)
train <- sample(1:nrow(x), nrow(x) *2/3)#2/3 to train og 1/3 test
length(train)
test <- (-train)

grid <- 10^seq(10, -2, length = 100)

#Ridge regression

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
set.seed(1)
cv.out.ridge <- cv.glmnet(x[train, ], y[train], alpha = 0,lambda=grid) 
bestlam.ridge <- cv.out.ridge$lambda.min
cv.out.ridge

bestlam.ridge # optimale 

ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x[test, ])#Vi beregner dette udelukkende for at bestemme MSE og senere sammenligne det med det, der opnås gennem Lasso.
mse_ridge_test <- mean((ridge.pred - y[test])^2)
rmse_ridge_test<-sqrt(mse_ridge_test)
ridge_mse_cv <- min(cv.out.ridge$cvm)#henter den gennemsnitlige cv MSE på tværs af alle fold for bedste lambda
rmse_ridge_cv<-sqrt(ridge_mse_cv )

# Endelig gen-estimerer vi vores ridge regression model på det fulde datasæt med 
# den optimale lambda-værdi, der blev fundet ved cross-validation

out.ridge <- glmnet(x, y, alpha = 0)
ridge.coef<-predict(out.ridge, type = "coefficients", s = bestlam.ridge)[1:20, ]
ridge.coef

prædiktorer_antal_ridge <- dim(x)[2]+1# here beregner vi intercept som betta0

predicted_Ridge <- predict(out.ridge, s = bestlam.ridge, newx = x)
res_Ridge <- sum((y - predicted_Ridge)^2)
tss <- sum((y - mean(y))^2)
r_squared_Ridge <- 1 - (res_Ridge / tss)


r_squared_Ridge # er lav: 0,42, det kan være acceptabelt, men vi kan optimere det ved at fjerne outlieren
#lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, # alpha nu lig med 1
                    lambda = grid)
set.seed(1)
cv.out.lasso <- cv.glmnet(x[train, ], y[train], alpha = 1,lambda=grid)
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
                      s = bestlam.lasso)[1:20, ]
lasso.coef

length(lasso.coef[lasso.coef != 0])
prædiktorer_antal_lasso<-length(lasso.coef[lasso.coef != 0]) 

predicted_lasso <- predict(out.lasso, s = bestlam.lasso, newx = x)
res_lasso <- sum((y - predicted_lasso)^2)
tss <- sum((y - mean(y))^2)
r_squared_lasso <- 1 - (res_lasso / tss)

r_squared_lasso # 0.39 


# Best subset selection

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

ml_dataset_lille_train <- ml_dataset_lille[train,]
ml_dataset_lille_test <- ml_dataset_lille[test,]

k <- 10 # Vi danner 10 folds
n <- nrow(ml_dataset_lille_train) # registrerer hvor mange observationer, vi har.
set.seed(1) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim(ml_dataset_lille)[2]  # Der er 16 prædiktorer 

cv.errors <- matrix(NA, k, 16,
                    dimnames = list(NULL, paste(1:16)))
cv.errors


for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(Guld_menu_stk ~ .,
                         data = ml_dataset_lille_train[folds != j, ],
                         nvmax = 16)
  for (i in 1:16) { # her gennemløbes alle kandidatmodeller
    pred <- predict.regsubsets(best.fit, ml_dataset_lille_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((ml_dataset_lille_train$Guld_menu_stk[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
which.min(mean.cv.errors)# model med 2 prædiktorer er vælges

prædiktorer_antal_best<- as.numeric(names(which.min(mean.cv.errors)))

# Her fittes modellen til ALLE træningsdata
reg.best <- regsubsets(Guld_menu_stk ~ ., data = ml_dataset_lille_train,
                       nvmax = 16)
coef(reg.best, 2)

pred_best_subset <- predict(reg.best, ml_dataset_lille_test, id = 2)


mse_best_subset <- mean((ml_dataset_lille_test$Guld_menu_stk - pred_best_subset)^2)
rmse_bestsubset_test <- sqrt(mse_best_subset)
rmse_bestsubset_cv <- sqrt(min(mean.cv.errors))

# Her fittes modellen til ALLE data

reg.best <- regsubsets(Guld_menu_stk ~ ., data = ml_dataset_lille,
                       nvmax = 16)
coef(reg.best, 2)
pred_best_subset <- predict(reg.best, ml_dataset_lille, id = 2)

res_best<- sum((y - pred_best_subset)^2)
tss <- sum((y- mean(y))^2)
r_squared_best <- 1 - (res_best / tss)

r_squared_best #0.29


# 0 features : naiv model
set.seed(1)
glm.fit <- glm( Guld_menu_stk~ 1, data = ml_dataset_lille[train, ])
rmse_0_cv <- sqrt(cv.glm(ml_dataset_lille[train, ], glm.fit , K = 10)$delta[1])
rmse_0_test <- sqrt(mean((ml_dataset_lille[test, ]$Guld_menu_stk - predict(glm.fit, ml_dataset_lille[test, ]))^2))

# sammenligning --------------------------------------------------------------

rmse_ridge_test
rmse_lasso_test
rmse_bestsubset_test
rmse_0_test

rmse_ridge_cv
rmse_lasso_cv
rmse_bestsubset_cv
rmse_0_cv

r_squared_lasso
r_squared_Ridge
r_squared_best


# Uden outlier ------------------------------------------------------------


# nu fjerner vi outlieren: observation 7 og se, om vi får en bedre R_squared
# Vi vil udføre det samme arbejde, sammenligne modeller for at se, hvordan de klarer sig på usete data, 
# og derefter beregne den bedste lambda på hele datasættet for til sidst at beregne R_squared.



ml_dataset_uden_outlier<-ml_dataset_lille[-7,]

#Lad os starte med en simpel lineær regression for at få en idé om, hvilke prediktorer der kan være betydningsfulde
data<-ml_dataset_uden_outlier
lm<-lm(Guld_menu_stk~., data = data)
summary(lm)

#Vejrforhold,VFF Præstationen, tidligere tilskuertal, året og tv-kanalen ser ud til at have en betydelig indflydelse på VIP-deltagelsen. For at bygge en robust og præcis model vil vi bruge Ridge- og Lasso-regression til at håndtere multikollinearitet og identificere de vigtigste prediktorer.


#Ridge + lasso
x_uden_outlier <- model.matrix(Guld_menu_stk ~ . , ml_dataset_uden_outlier)[,-1]
y_uden_outlier <-ml_dataset_uden_outlier$Guld_menu_stk



#Ridge regression

set.seed(1)
train <- sample(1:nrow(x_uden_outlier), nrow(x_uden_outlier) *2/3)#2/3 to train og 1/3 test
length(train)
test <- (-train)

grid <- 10^seq(10, -2, length = 100)

#Ridge regression

ridge.mod <- glmnet(x_uden_outlier[train, ], y_uden_outlier[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
set.seed(1)
cv.out.ridge <- cv.glmnet(x_uden_outlier[train, ], y_uden_outlier[train],nfolds = 5, alpha = 0,lambda=grid) 
bestlam.ridge <- cv.out.ridge$lambda.min
cv.out.ridge

bestlam.ridge 

ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x_uden_outlier[test, ])
mse_ridge_uden_outlier_test <- mean((ridge.pred - y_uden_outlier[test])^2)
ridge_mse_cv_uden_outlier <- min(cv.out.ridge$cvm)

rmse_ridge_uden_outlier_test <- sqrt(mse_ridge_uden_outlier_test )
rmse_ridge_cv_uden_outlier<-sqrt(ridge_mse_cv_uden_outlier)

# Her fittes modellen til ALLE data

out.ridge <- glmnet(x_uden_outlier, y_uden_outlier, alpha = 0)
ridge.coef<-predict(out.ridge, type = "coefficients", s = bestlam.ridge)[1:20, ]
ridge.coef
prædiktorer_antal_ridge_uden_outlier <- dim(x_uden_outlier)[2]+1 #here beregner vi den intercept som coefficient Betta0

predicted_Ridge <- predict(out.ridge, s = bestlam.ridge, newx = x_uden_outlier)
res_Ridge_uden_outlier <- sum((y_uden_outlier - predicted_Ridge)^2)
tss_uden_outlier <- sum((y_uden_outlier - mean(y_uden_outlier))^2)
r_squared_Ridge_uden_outlier <- 1 - (res_Ridge_uden_outlier / tss_uden_outlier)

r_squared_Ridge_uden_outlier # 0.5 den var 0.42 med outlier 

#lasso
lasso.mod <- glmnet(x_uden_outlier[train, ], y_uden_outlier[train], alpha = 1,lambda = grid)
set.seed(1)
cv.out.lasso <- cv.glmnet(x_uden_outlier[train, ], y_uden_outlier[train], alpha = 1,lambda=grid,nfolds=5)
bestlam.lasso <- cv.out.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam.lasso,
                      newx = x_uden_outlier[test, ])

mse_lasso_uden_outlier_test <- mean((lasso.pred - y_uden_outlier[test])^2)
lasso_mse_cv_uden_outlier <- min(cv.out.lasso$cvm)
rmse_lasso_uden_outlier_test<- sqrt(mse_lasso_uden_outlier_test)
rmse_lasso_cv_uden_outlier <- sqrt(lasso_mse_cv_uden_outlier )

# Her fittes modellen til ALLE data

out.lasso <- glmnet(x_uden_outlier, y_uden_outlier, alpha = 1)
lasso.coef <- predict(out.lasso, type = "coefficients",
                      s = bestlam.lasso)[1:20, ]
lasso.coef

length(lasso.coef[lasso.coef != 0]) #14 prædiktorer beholdes
prædiktorer_antal_lasso_uden_outlier<-length(lasso.coef[lasso.coef != 0]) 

predicted_lasso <- predict(out.lasso, s = bestlam.lasso, newx = x_uden_outlier)
res_lasso_uden_outlier <- sum((y_uden_outlier - predicted_lasso)^2)
tss_uden_outlier <- sum((y_uden_outlier - mean(y_uden_outlier))^2)
r_squared_lasso_uden_outlier <- 1 - (res_lasso_uden_outlier / tss_uden_outlier)

r_squared_lasso_uden_outlier # 0.46 , det var 0.39 før vi fjerner den outlier

# Best subset selection



ml_dataset_uden_outlier_train <- ml_dataset_uden_outlier[train,]
ml_dataset_uden_outlier_test <- ml_dataset_uden_outlier[test,]

k <- 10 # Vi danner 10 folds
n <- nrow(ml_dataset_uden_outlier_train) # registrerer hvor mange observationer, vi har.
set.seed(1) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim(ml_dataset_uden_outlier_train)[2]  # Der er 16 prædiktorer 

cv.errors <- matrix(NA, k, 16,
                    dimnames = list(NULL, paste(1:16)))
cv.errors


for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(Guld_menu_stk ~ .,
                         data = ml_dataset_uden_outlier_train[folds != j, ],
                         nvmax = 16)
  for (i in 1:16) { # her gennemløbes alle kandidatmodeller
    pred <- predict(best.fit, ml_dataset_uden_outlier_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((ml_dataset_uden_outlier_train$Guld_menu_stk[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
which.min(mean.cv.errors)



# Her fittes modellen til ALLE træningsdata
reg.best <- regsubsets(Guld_menu_stk ~ ., data = ml_dataset_uden_outlier_train,
                       nvmax = 16)
coef(reg.best, 2)

prædiktorer_antal_best_uden_outlier <- as.numeric(names(which.min(mean.cv.errors)))

pred_best_subset <- predict(reg.best, ml_dataset_uden_outlier_test, id = 2)


mse_best_subset_uden_outlier <- mean((ml_dataset_uden_outlier_test$Guld_menu_stk - pred_best_subset)^2)
rmse_bestsubset_test_uden_outlier  <- sqrt(mse_best_subset_uden_outlier )
rmse_bestsubset_cv_uden_outlier  <- sqrt(min(mean.cv.errors))


# Her fittes modellen til ALLE dataset

reg.best <- regsubsets(Guld_menu_stk ~ ., data = ml_dataset_uden_outlier,
                       nvmax = 16)
coef(reg.best, 2)

pred_best_subset <- predict(reg.best, ml_dataset_uden_outlier, id = 2)

res_best_uden_outlier <- sum((y_uden_outlier - pred_best_subset)^2)
tss_uden_outlier <- sum((y_uden_outlier - mean(y_uden_outlier))^2)
r_squared_best_uden_outlier <- 1 - (res_best_uden_outlier / tss_uden_outlier)

r_squared_best_uden_outlier

# 0 features uden outlier
set.seed(1)
glm.fit <- glm( Guld_menu_stk~ 1, data = ml_dataset_uden_outlier[train, ])
rmse_0_cv_uden_outlier <- sqrt(cv.glm(ml_dataset_uden_outlier[train, ], glm.fit , K = 10)$delta[1])
rmse_0_test_uden_outlier <- sqrt(mean((ml_dataset_uden_outlier[test, ]$Guld_menu_stk - predict(glm.fit, ml_dataset_uden_outlier[test, ]))^2))



# sammenligning --------------------------------------------------------------
r_squared_Ridge_uden_outlier
r_squared_lasso_uden_outlier
r_squared_best_uden_outlier

rmse_bestsubset_cv_uden_outlier 
rmse_ridge_cv_uden_outlier
rmse_lasso_cv_uden_outlier
rmse_0_cv_uden_outlier

rmse_bestsubset_test_uden_outlier  
rmse_lasso_uden_outlier_test
rmse_ridge_uden_outlier_test 
rmse_0_test_uden_outlier

#plot til sammeling :

prædiktorer_antal_ridge 
prædiktorer_antal_lasso
prædiktorer_antal_ridge_uden_outlier
prædiktorer_antal_lasso_uden_outlier
prædiktorer_antal_best
prædiktorer_antal_best_uden_outlier
prædiktorer_antal_best
prædiktorer_antal_naive_uden_outlier<-0
prædiktorer_antal_naive<-0

# data for histogram plot (med outlier)
mse_data_hist <- data.frame(
  Model = c("Lasso", "Lasso", "Naive", "Naive", "Ridge", "Ridge", "Best Subset", "Best Subset"),
  MSE_Type = rep(c("CV", "Test"), 4),
  MSE = c(
    rmse_lasso_cv, 
    rmse_lasso_test, 
    rmse_0_cv, 
    rmse_0_test, 
    rmse_ridge_cv, 
    rmse_ridge_test, 
    rmse_bestsubset_cv, 
    rmse_bestsubset_test
  ),
  Outlier_Status = rep("med outlier", 8)  
)

# data for histogram plot (uden outlier)
mse_data_hist_no_outlier <- data.frame(
  Model = c("Lasso", "Lasso", "Naive", "Naive", "Ridge", "Ridge", "Best Subset", "Best Subset"),
  MSE_Type = rep(c("CV", "Test"), 4),
  MSE = c(
    rmse_lasso_cv_uden_outlier, 
    rmse_lasso_uden_outlier_test, 
    rmse_0_cv_uden_outlier, 
    rmse_0_test_uden_outlier, 
    rmse_ridge_cv_uden_outlier, 
    rmse_ridge_uden_outlier_test, 
    rmse_bestsubset_cv_uden_outlier, 
    rmse_bestsubset_test_uden_outlier
  ),
  Outlier_Status = rep("uden outlier", 8)  
)

# Kombiner begge datasæt
mse_data_combined <- rbind(mse_data_hist, mse_data_hist_no_outlier)


# tilføj  prædiktorer antal
mse_data_combined <- mse_data_combined |>
  mutate(Predictors = case_when(
    Model == "Ridge" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_ridge,"p",sep=""),
    Model == "Ridge" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_ridge_uden_outlier,"p",sep=""),
    Model == "Lasso" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_lasso,"p",sep=""),
    Model == "Lasso" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_lasso_uden_outlier,"p",sep=""),
    Model == "Best Subset" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_best,"p",sep=""),
    Model == "Best Subset" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_best_uden_outlier,"p",sep=""),
    Model == "Naive" ~ paste0(prædiktorer_antal_naive ,"p",sep="") 
    
  ))
# Filter  MSE data for "uden outlier"
mse_uden_outlier <- mse_data_combined |>
  filter(Outlier_Status == "uden outlier")

# Plot 
mse_hist_plot <- ggplot(mse_data_combined, aes(x = Model, y = MSE, fill = Outlier_Status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +  # Bars
  geom_text(
    aes(label = Predictors), 
    position = position_dodge(width = 0.8), 
    vjust = 10, 
    size = 3
  ) +  # tilføj prædiktorer 
  geom_point(
    data = mse_uden_outlier,
    aes(x = Model, y = MSE),
    color = "red", size = 3, shape = 21, fill = "gold"
  ) +  
  geom_text(
    data = mse_uden_outlier,
    aes(x = Model, y = MSE, label = round(MSE, 2)),
    position = position_dodge(width = 0.7),
    vjust = -1, size = 3, color = "red"
  ) + 
  facet_wrap(~ MSE_Type) +  # 2 plots  CV og Test
  labs(
    x = "Model",
    y = "MSE",
    title = "Sammenligning af MSE for forskellige modeller (Lasso, Naive, Ridge, Best Subset)\nMed og uden outliers"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.5, 0.95),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  ) +
  scale_fill_manual(values = c(
    "med outlier" = "skyblue", 
    "uden outlier" = "orange"
  ))


mse_hist_plot



#  dataset for R-anden værdier
r_squared_data <- data.frame(
  Model = c("Ridge", "Ridge", "Lasso", "Lasso", "Best Subset", "Best Subset"),
  Outlier_Status = c("med outlier", "uden outlier", "med outlier", "uden outlier", "med outlier", "uden outlier"),
  R_Squared = c(
    r_squared_Ridge, 
    r_squared_Ridge_uden_outlier,
    r_squared_lasso, 
    r_squared_lasso_uden_outlier,
    r_squared_best,
    r_squared_best_uden_outlier
  )
)

r_squared_uden_outlier <- r_squared_data |>
  filter(Outlier_Status == "uden outlier") 



# tilføj prædiktorerne antal
r_squared_data <- r_squared_data |>
  mutate(Predictors = case_when(
    Model == "Ridge" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_ridge,"p",sep=""),
    Model == "Ridge" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_ridge_uden_outlier,"p",sep=""),
    Model == "Lasso" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_lasso,"p",sep=""),
    Model == "Lasso" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_lasso_uden_outlier,"p",sep=""),
    Model == "Best Subset" & Outlier_Status == "med outlier" ~ paste0(prædiktorer_antal_best,"p",sep=""),
    Model == "Best Subset" & Outlier_Status == "uden outlier" ~ paste0(prædiktorer_antal_best_uden_outlier,"p",sep=""),
    Model == "Naive" ~ paste0(prædiktorer_antal_naive ,"p",sep="")
  ))


r_squared_plot <- ggplot(r_squared_data, aes(x = Model, y = R_Squared, fill = Outlier_Status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bars
  geom_text(
    aes(label = Predictors), 
    position = position_dodge(width = 0.7), 
    vjust = 20, 
    size = 3
  ) +  
  geom_point(
    data = r_squared_uden_outlier,
    aes(x = Model, y = R_Squared),
    color = "red", size = 3, shape = 21, fill = "gold"
  ) + 
  geom_text(
    data = r_squared_uden_outlier,
    aes(x = Model, y = R_Squared, label = round(R_Squared, 2)),
    position = position_dodge(width = 0.7),
    vjust = -1, size = 3, color = "red"
  ) +  
  labs(
    x = "Model",
    y = expression(R^2),
    title = "Sammenligning af R-squared for forskellige modeller (med og uden outliers)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  ) +
  scale_fill_manual(values = c(
    "med outlier" = "skyblue", 
    "uden outlier" = "orange"
  ))

r_squared_plot
sd(y_uden_outlier[train])

normalized_rmse_train_lasso <- rmse_lasso_cv_uden_outlier / sd(y_uden_outlier[train])
normalized_rmse_test_lasso <- rmse_lasso_uden_outlier_test / sd(y_uden_outlier[test])

normalized_rmse_train_subset <- rmse_bestsubset_cv_uden_outlier / sd(y_uden_outlier[train])
normalized_rmse_test_subset <- rmse_bestsubset_test_uden_outlier / sd(y_uden_outlier[test])


normalized_rmse_train_ridge <- rmse_ridge_cv_uden_outlier / sd(y_uden_outlier[train])
normalized_rmse_test_ridge <- rmse_ridge_uden_outlier_test / sd(y_uden_outlier[test])

rmse_lasso_cv_uden_outlier 
rmse_lasso_uden_outlier_test
r_squared_lasso_uden_outlier


str(ml_dataset_uden_outlier)

pacman::p_load(readr,Matrix,readxl,forcats,psych,ggcorrplot,GGally,olsrr,leaps,MASS,plyr,tidyverse,fastDummies)

Q01_data <- read_table("exercicios/lista2/Q01_data.txt")
Q02_data <- read_table("exercicios/lista2/Q02_data.txt", col_types = cols(X4 = col_skip()))

Q01_data %>%
  mutate(x1 = as.factor(x1))

modelo1_completo = lm(y ~ x1 + x2 + x3 + x4 + x6 + x7 + x8 + x9 , data = Q01_data)
anova(modelo1_completo)

matriz_cor = cor(Q01_data %>% dplyr::select(-c(1,5,6,11)))

# Converter a matriz de correlação em um data frame
df_cor <- as.data.frame(matriz_cor)

# Criar o gráfico de correlação (corr plot)
ggcorrplot(df_cor,lab = TRUE)

#matriz grafico de dispersao

ggpairs(Q01_data)

#####################################

k <- ols_step_all_possible(modelo1_completo)
subsets = as.data.frame(k)
plot(k)

k <- ols_step_forward_p(modelo1_completo)
plot(k)

k <- ols_step_backward_p(modelo1_completo)
plot(k)

k <- ols_step_both_p(modelo1_completo)
plot(k)

#####################################
features <- Q01_data %>% dplyr::select(-c(1,5,6,11))
reg <- regsubsets(features, Q01_data$y)
reg.summary <- summary(reg)
reg.summary

par(mfrow=c(2,2))

# RSS
plot(reg.summary$rss, xlab="Number of Variables", ylab="Resid. Sum of Squares", type="l")
best.rss <- which.min(reg.summary$rss)
points(best.rss, reg.summary$rss[best.rss], col="red", cex=2,pch=20)

# AdjR2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R2", type="l")
best.adjr2 <- which.max(reg.summary$adjr2)
points(best.adjr2, reg.summary$adjr2[best.adjr2], col="red", cex=2,pch=20)

# Note in this case that AIC and Cp are the same.
# Mallows Cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Mallows Cp", type="l")
best.cp <- which.min(reg.summary$cp)
points(best.cp, reg.summary$cp[best.cp], col="red", cex=2,pch=20)

# BIC
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
best.bic <- which.min(reg.summary$bic)
points(best.bic, reg.summary$bic[best.bic], col="red", cex=2,pch=20)


best.cp_model <- which.min(reg.summary$cp)
coef(reg, best.cp_model)


# Note k=2 uses AIC.  You can use k=log(n) if you want BIC
forward_mdl <- step(modelo1_completo,scope = modelo1_completo, direction="forward", k=2)
summary(forward_mdl)

backward_mdl <- step(modelo1_completo,scope = modelo1_completo, direction="backward", k=2)
summary(backward_mdl)


both_mdl <- step(modelo1_completo,scope = modelo1_completo, direction="both", k=2)
summary(both_mdl)


l<-list(data.frame(t(data.frame(coef(forward_mdl)))),
        data.frame(t(data.frame(coef(backward_mdl)))),
        data.frame(t(data.frame(coef(both_mdl)))))
stepwise_results <- do.call(rbind.fill, l)
row.names(stepwise_results) <- c('Forward','Backward','Both')
stepwise_results

####################
# Ajustar um modelo inicial com todas as variáveis
modelo_inicial <- lm(y ~ ., data = Q01_data)

# Seleção forward
modelo_forward <- step(modelo_inicial, direction = "forward")

# Seleção backward
modelo_backward <- step(modelo_inicial, direction = "backward")

# Seleção stepwise (forward e backward)
modelo_stepwise <- step(modelo_inicial)

# Exibir os resultados dos modelos
summary(modelo_forward)
summary(modelo_backward)
summary(modelo_stepwise)

###############################################################################


Q02_data <- read_table("exercicios/lista2/Q02_data.txt", col_types = cols(X4 = col_skip()))

Q02_data = Q02_data %>%
  mutate(tratamento = as.factor(tratamento))

modelo1_completo = lm(eficacia ~ idade + tratamento, data = Q02_data)
summary(modelo1_completo)
anova(modelo1_completo)

Q02_data_dummies = Q02_data %>%
  dummy_cols(select_columns = "tratamento", remove_selected_columns = TRUE, remove_first_dummy  = TRUE)

modelo1_completo = lm(eficacia ~ idade + tratamento_B + tratamento_C + idade:tratamento_B + idade:tratamento_C,  data = Q02_data_dummies)
summary(modelo1_completo)
anova(modelo1_completo)


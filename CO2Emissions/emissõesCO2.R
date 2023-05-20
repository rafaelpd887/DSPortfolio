#Transformando a base de dados do formato do Excel para o formato do RStudio
FuelConsumption <- read.csv("FuelConsumption.csv")

#Salvando a base de dados transformada para o fromato do RStudio
save(FuelConsumption, file = "FuelConsumption.RData")

#Visualizando o tipo das variáveis
glimpse(FuelConsumption) 

#Mudando o tipo da variável MODELYEAR
FuelConsumption$MODELYEAR <- as.character(FuelConsumption$MODELYEAR)
FuelConsumption$CYLINDERS <- as.numeric(FuelConsumption$CYLINDERS)
FuelConsumption$FUELCONSUMPTION_COMB_MPG <- as.numeric(FuelConsumption$FUELCONSUMPTION_COMB_MPG)
FuelConsumption$CO2EMISSIONS <- as.numeric(FuelConsumption$CO2EMISSIONS)

#Estatísticas das variáveis
summary(FuelConsumption)

#Categorias das variáveis categóricas
levels(factor(FuelConsumption$MAKE))
levels(factor(FuelConsumption$MODEL))
levels(factor(FuelConsumption$VEHICLECLASS))
levels(factor(FuelConsumption$TRANSMISSION))
levels(factor(FuelConsumption$FUELTYPE))

#Tabelas de frequencia das variáveis categóricas
table(FuelConsumption$MAKE)
table(FuelConsumption$MODEL)
table(FuelConsumption$VEHICLECLASS)
table(FuelConsumption$TRANSMISSION)
table(FuelConsumption$FUELTYPE)

#Correlações
chart.Correlation((FuelConsumption[, c(5, 6, 9:13)]), histogram = TRUE)

#Removendo a coluna MODELYEAR para evitar erros na modelagem
dropyear <- FuelConsumption[-c(1)]

#Removendo a coluna MODEL para evitar erros no Stepwise
dropyear2 <- dropyear[-c(2)]


#Dummização das variáveis categóricas
FuelConsumption_dummies <- dummy_columns(.data = dropyear2,
                                    select_columns = "MAKE",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "VEHICLECLASS",
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "TRANSMISSION",
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "FUELTYPE",
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T)

#Modelagem linear com todas as variáveis
modelo_FuelConsumption <- lm(formula = CO2EMISSIONS ~ ., data = FuelConsumption_dummies)

#Parâmetros do modelo_FuelConsumption
summary(modelo_FuelConsumption)

#Stepwise
step_FuelConsumption <- step(modelo_FuelConsumption, k = 3.841459)

summary(step_FuelConsumption2)

#VIF e Tolerance
ols_vif_tol(modelo_FuelConsumption)
vif(step_FuelConsumption)

#Eliminando FUELCONSUMPTION_COMB para evitar multicolinearidade
step_FuelConsumption2 <- update(step_FuelConsumption, CO2EMISSIONS ~ .- FUELCONSUMPTION_COMB, data = FuelConsumption_dummies)

#Stepwise2
step_FuelConsumption3 <- step(step_FuelConsumption2, k = 3.841459)

#Shapiro-Francia
sf.test(step_FuelConsumption3$residuals)

#Plotando os resíduos do modelo step_FuelConsumption2 
FuelConsumption_dummies %>%
  mutate(residuos = step_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
FuelConsumption_dummies %>%
  mutate(residuos = step_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_FuelConsumption2$residuals),
                            sd = sd(step_FuelConsumption2$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Breusch-Pagan
ols_test_breusch_pagan(step_FuelConsumption3)

#Adicionando fitted values e resíduos do modelo 'step_fuelconsumption2'
#no dataset 'fuelconsumption_dummies'
FuelConsumption_dummies$fitted_step <- step_FuelConsumption2$fitted.values
FuelConsumption_dummies$residuos_step <- step_FuelConsumption2$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_fuelconsumption2'
FuelConsumption_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(dropyear2$CO2EMISSIONS)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
FuelConsumption_dummies$bcCO2EMISSIONS <- (((dropyear2$CO2EMISSIONS ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)

#Estimando um novo modelo múltiplo com dummies
modelo_bc_FuelConsumption <- lm(formula = bcCO2EMISSIONS ~ .-CO2EMISSIONS -fitted_step
                           -residuos_step, 
                           data = FuelConsumption_dummies)

#Parâmetros do modelo
summary(modelo_bc_FuelConsumption)

#Aplicando o procedimento Stepwise
step_bc_FuelConsumption <- step(modelo_bc_FuelConsumption, k = 3.841459)

summary(step_bc_FuelConsumption3)

#VIF
vif(step_bc_FuelConsumption2)

#Eliminando FUELCONSUMPTION_COMB e FUELCONSUMPTION_CITY para evitar multicolinearidade
step_bc_FuelConsumption2 <- update(step_bc_FuelConsumption, bcCO2EMISSIONS ~ .- FUELCONSUMPTION_COMB - FUELCONSUMPTION_CITY, data = FuelConsumption_dummies)

#Aplicando novo Stepwise após a eliminação das variáveis acima
step_bc_FuelConsumption3 <- step(step_bc_FuelConsumption2)

#Teste de Shapiro-Francia
sf.test(step_bc_FuelConsumption3$residuals)

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_FuelConsumption3)

#Plotando os resíduos do modelo step_FuelConsumption2 
FuelConsumption_dummies %>%
  mutate(residuos = step_bc_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
FuelConsumption_dummies %>%
  mutate(residuos = step_bc_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_FuelConsumption2$residuals),
                            sd = sd(step_bc_FuelConsumption2$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

summary(step_bc_FuelConsumption3)

#Inserindo fitted.values do modelo "step_bc_FuelConsumption3' no banco de dados "FuelConsumption_dummies"
FuelConsumption_dummies$fitted_step3 <- step_bc_FuelConsumption3$fitted.values

#Comparando a capacidade preditiva do modelos "step_bc_FuelConsumption2" com o modelo "step_FuelConsumption2"
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step)
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step2)
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step3,
     xlab = "Valores estimados de emissoes de CO2(boxcox)",
     ylab = "Valores observados de emissoes de CO2")

#Transformando de volta valores previstos do modelo Box-Cox e os colocando no dateset FuelConsumption_dummies
FuelConsumption_dummies$yhat_step_bc_FuelConsumption3 <- (((step_bc_FuelConsumption3$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$yhat_step_bc_FuelConsumption3,
     xlab = "Valores estimados de emissoes de CO2(boxcox)",
     ylab = "Valores observados de emissoes de CO2")







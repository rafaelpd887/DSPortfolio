# Transforming the Excel format dataset to RStudio format
FuelConsumption <- read.csv("FuelConsumption.csv")

# Saving the transformed dataset in RStudio format
save(FuelConsumption, file = "FuelConsumption.RData")

# Viewing the variable types
glimpse(FuelConsumption)

# Changing the type of the MODELYEAR variable
FuelConsumption$MODELYEAR <- as.character(FuelConsumption$MODELYEAR)
FuelConsumption$CYLINDERS <- as.numeric(FuelConsumption$CYLINDERS)
FuelConsumption$FUELCONSUMPTION_COMB_MPG <- as.numeric(FuelConsumption$FUELCONSUMPTION_COMB_MPG)
FuelConsumption$CO2EMISSIONS <- as.numeric(FuelConsumption$CO2EMISSIONS)

# Variable statistics
summary(FuelConsumption)

# Categories of categorical variables
levels(factor(FuelConsumption$MAKE))
levels(factor(FuelConsumption$MODEL))
levels(factor(FuelConsumption$VEHICLECLASS))
levels(factor(FuelConsumption$TRANSMISSION))
levels(factor(FuelConsumption$FUELTYPE))

# Frequency tables of categorical variables
table(FuelConsumption$MAKE)
table(FuelConsumption$MODEL)
table(FuelConsumption$VEHICLECLASS)
table(FuelConsumption$TRANSMISSION)
table(FuelConsumption$FUELTYPE)

# Correlations
chart.Correlation((FuelConsumption[, c(5, 6, 9:13)]), histogram = TRUE)

# Removing the MODELYEAR column to avoid modeling errors
dropyear <- FuelConsumption[-c(1)]

# Removing the MODEL column to avoid Stepwise errors
dropyear2 <- dropyear[-c(2)]

# Dummy encoding of categorical variables
FuelConsumption_dummies <- dummy_columns(.data = dropyear2,
                                    select_columns = "MAKE",
                                    remove_selected_columns = TRUE,
                                    remove_most_frequent_dummy = TRUE)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "VEHICLECLASS",
                                         remove_selected_columns = TRUE,
                                         remove_most_frequent_dummy = TRUE)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "TRANSMISSION",
                                         remove_selected_columns = TRUE,
                                         remove_most_frequent_dummy = TRUE)

FuelConsumption_dummies <- dummy_columns(.data = FuelConsumption_dummies,
                                         select_columns = "FUELTYPE",
                                         remove_selected_columns = TRUE,
                                         remove_most_frequent_dummy = TRUE)

# Linear modeling with all variables
modelo_FuelConsumption <- lm(formula = CO2EMISSIONS ~ ., data = FuelConsumption_dummies)

# Model parameters
summary(modelo_FuelConsumption)

# Stepwise
step_FuelConsumption <- step(modelo_FuelConsumption, k = 3.841459)

summary(step_FuelConsumption)

# VIF and Tolerance
ols_vif_tol(modelo_FuelConsumption)
vif(step_FuelConsumption)

# Removing FUELCONSUMPTION_COMB to avoid multicollinearity
step_FuelConsumption2 <- update(step_FuelConsumption, CO2EMISSIONS ~ .- FUELCONSUMPTION_COMB, data = FuelConsumption_dummies)

# Stepwise2
step_FuelConsumption3 <- step(step_FuelConsumption2, k = 3.841459)

# Shapiro-Francia
sf.test(step_FuelConsumption3$residuals)

# Plotting the residuals of the step_FuelConsumption2 model
FuelConsumption_dummies %>%
  mutate(residuos = step_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Residuals",
       y = "Frequencies") + 
  theme_bw()

# Adding a theoretical normal curve for distribution comparison
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
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()

# Breusch-Pagan
ols_test_breusch_pagan(step_FuelConsumption3)

# Adding fitted values and residuals of the 'step_fuelconsumption2' model
# to the 'fuelconsumption_dummies' dataset
FuelConsumption_dummies$fitted_step <- step_FuelConsumption2$fitted.values
FuelConsumption_dummies$residuos_step <- step_FuelConsumption2$residuals

# Plot relating residuals and fitted values of the 'step_fuelconsumption2' model
FuelConsumption_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values of Stepwise Model",
       y = "Residuals of Stepwise Model") +
  theme_bw()

# To calculate the Box-Cox lambda
lambda_BC <- powerTransform(dropyear2$CO2EMISSIONS)
lambda_BC

# Inserting the Box-Cox lambda in the new dataset for the estimation of a new model
FuelConsumption_dummies$bcCO2EMISSIONS <- (((dropyear2$CO2EMISSIONS ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)

# Estimating a new multiple model with dummies
modelo_bc_FuelConsumption <- lm(formula = bcCO2EMISSIONS ~ .-CO2EMISSIONS -fitted_step
                           -residuos_step, 
                           data = FuelConsumption_dummies)

# Model parameters
summary(modelo_bc_FuelConsumption)

# Applying the Stepwise procedure
step_bc_FuelConsumption <- step(modelo_bc_FuelConsumption, k = 3.841459)

summary(step_bc_FuelConsumption)

# VIF
vif(step_bc_FuelConsumption2)

# Removing FUELCONSUMPTION_COMB and FUELCONSUMPTION_CITY to avoid multicollinearity
step_bc_FuelConsumption2 <- update(step_bc_FuelConsumption, bcCO2EMISSIONS ~ .- FUELCONSUMPTION_COMB - FUELCONSUMPTION_CITY, data = FuelConsumption_dummies)

# Applying a new Stepwise after removing the above variables
step_bc_FuelConsumption3 <- step(step_bc_FuelConsumption2)

# Shapiro-Francia test
sf.test(step_bc_FuelConsumption3$residuals)

# Heteroscedasticity diagnosis for the Stepwise model with Box-Cox
ols_test_breusch_pagan(step_bc_FuelConsumption3)

# Plotting the residuals of the step_FuelConsumption2 model
FuelConsumption_dummies %>%
  mutate(residuos = step_bc_FuelConsumption2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Residuals",
       y = "Frequencies") + 
  theme_bw()

# Adding a theoretical normal curve for distribution comparison
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
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()

summary(step_bc_FuelConsumption3)

# Inserting fitted.values of the 'step_bc_FuelConsumption3' model in the 'FuelConsumption_dummies' dataset
FuelConsumption_dummies$fitted_step3 <- step_bc_FuelConsumption3$fitted.values

# Comparing the predictive capacity of models "step_bc_FuelConsumption2" with "step_FuelConsumption2"
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step)
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step2)
plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$fitted_step3,
     xlab = "Estimated CO2 Emissions (Box-Cox)",
     ylab = "Observed CO2 Emissions")

# Transforming back predicted values of the Box-Cox model and putting them in the 'FuelConsumption_dummies' dataset
FuelConsumption_dummies$yhat_step_bc_FuelConsumption3 <- (((step_bc_FuelConsumption3$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

plot(FuelConsumption_dummies$CO2EMISSIONS ~ FuelConsumption_dummies$yhat_step_bc_FuelConsumption3,
     xlab = "Estimated CO2 Emissions (Box-Cox)",
     ylab = "Observed CO2 Emissions")







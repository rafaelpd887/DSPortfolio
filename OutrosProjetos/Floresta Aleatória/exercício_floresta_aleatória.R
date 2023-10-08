# Package Installation
packages <- c('tidyverse','rpart','rpart.plot','gtools','Rmisc','scales','viridis','caret','AMR','randomForest','fastDummies','rattle','xgboost','ggpubr','reshape2','mlbench')

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installer <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = TRUE)
    break()
  }
  sapply(packages, require, character = TRUE) 
} else {
  sapply(packages, require, character = TRUE) 
}

# Loading the Housing dataset
data(BostonHousing)

# Viewing the first rows of the dataset
head(BostonHousing)

#######################################
# Data Dictionary:                    #
#######################################

# CRIM: Per capita crime rate by town.
# ZN: Proportion of residential land zoned for large lots (over 25,000 sq. ft.).
# INDUS: Proportion of non-retail business acres per town.
# CHAS: Charles River dummy variable (1 if tract bounds river; 0 otherwise).
# NOX: Nitrogen oxide concentration (parts per 10 million).
# RM: Average number of rooms per dwelling.
# AGE: Proportion of owner-occupied units built before 1940.
# DIS: Weighted distance to employment centers in Boston.
# RAD: Accessibility index to radial highways.
# TAX: Property tax rate (per $10,000).
# PTRATIO: Pupil-teacher ratio by town.
# B: 1000(Bk - 0.63)^2, where Bk is the proportion of residents of African American descent by town.
# LSTAT: Percentage of lower status population.
# MEDV: Median value of owner-occupied homes in thousands of dollars.

#########################################
# 1) Splitting Training and Testing Data #
#########################################
n <- sample(1:2,
            size = nrow(BostonHousing),
            replace = TRUE,
            prob=c(0.8, 0.2))

training_data <- BostonHousing[n==1,]
testing_data <- BostonHousing[n==2,]

#########################################
# 2) Training the Random Forest         #
#########################################
forest <- randomForest::randomForest(
  medv ~ .,
  data = training_data,
  ntree = 500
)

predict(forest, training_data) %>% head
predict(forest, testing_data) %>% head

#########################################
# 3) Model Evaluation                   #
#########################################

# Training dataset
p_train <- predict(forest, training_data) 
  
# Testing dataset
p_test <- predict(forest, testing_data)
  
# Evaluation data frame (Training)
eval_train <- data.frame(obs=training_data$medv, 
                          pred=p_train )

# Evaluation data frame (Testing)
eval_test <- data.frame(obs=testing_data$medv, 
                          pred=p_test)
  
# Evaluation function
evaluate <- function(pred, obs) {
  mse <- mean((pred - obs)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(pred - obs))
  r_squared <- 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))
    
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r_squared, "\n")
}
  
# Using the evaluation function
evaluate(p_train, training_data$medv)
evaluate(p_test, testing_data$medv)

##############################################
# 4) Using Caret for Grid Search            #
##############################################
  
# Cross-Validation
control <- caret::trainControl(
  method = 'repeatedcv', 
  number = 4,
  repeats = 2,
  search = 'grid',
  summaryFunction = defaultSummary, 
  classProbs = FALSE 
)
  
grid <- base::expand.grid(.mtry = 1:10)
  
# Modeling the forest with cross-validation
forest_grid <- caret::train(medv ~ .,  
                                data = training_data,
                                method = 'rf', 
                                metric = 'RMSE', # Choose the best model based on RMSE
                                trControl = control,
                                ntree = 500,
                                tuneGrid = grid)
  
print(forest_grid)
plot(forest_grid)
  
##############################################
# Evaluating the Tuned Model                 #
##############################################
p_train_grid <- predict(forest_grid, training_data)
p_test_grid <- predict(forest_grid, testing_data)

evaluate(p_train_grid, training_data$medv)
evaluate(p_test_grid, testing_data$medv)

evaluate(p_train, training_data$medv)
evaluate(p_test, testing_data$medv)

eval_train_grid <- data.frame(obs=training_data$medv, 
                          pred=p_train_grid )

eval_test_grid <- data.frame(obs=testing_data$medv, 
                         pred=p_test_grid)

##############################################
# Conclusion                                 #
##############################################

## We can conclude that the grid-search and cross-validated tuned forest 
## achieved a slight improvement in its predictive accuracy.

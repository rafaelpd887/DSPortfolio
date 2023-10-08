# Introduction

# The objective of this project is to develop a logistic model to predict whether a person applying
# for credit at a financial institution will be approved or not. Our model will have a binary output
# variable, represented as "1" or "0" (yes or no), indicating whether the loan will be approved or not.
# To do this, we will use various independent variables related to loan applicants.

# Granting loans is an essential activity in financial institutions, but it also involves risks. The ability
# to identify in advance which candidates are more likely to be approved or rejected is fundamental for
# making assertive financial decisions. In this context, the logistic model is a suitable technique to
# solve this problem because it can handle binary dependent variables.


##################################################################################
#                  INSTALLATION AND LOADING OF NECESSARY PACKAGES                #
##################################################################################
#Packages used
packages <- c("plotly", "tidyverse", "knitr", "kableExtra", "fastDummies", "rgl", "car",
             "reshape2", "jtools", "stargazer", "lmtest", "caret", "pROC", "ROCR", "nnet",
             "magick", "cowplot", "globals", "equatiomatic", "PerformanceAnalytics")

options(rgl.debug = TRUE)

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installer <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}

#                EXAMPLE 01 - LOADING THE DATASET                  #
##############################################################################

# Let's load our dataset into R
data <- read.csv("loan.csv")
head(data, 10)

# The Loan_ID variable is useless for our model because it classifies each observation in a unique way
# solely for the purpose of sorting the dataset. Therefore, it will be impossible to extract any
# information or behavior from it that influences the dependent variable.
# Excluding Loan_ID from the dataset
adjusted_data <- subset(data, select = -Loan_ID)

# We can see that some observations have missing/null values. In this case, we can try to fill the
# missing values using some strategy, or we can remove observations with null variables. In this case,
# we choose to exclude observations with null variables since variables like "gender" cannot be estimated
# except by pure arbitrariness.
adjusted_data <- na.omit(adjusted_data)
adjusted_data <- adjusted_data %>%
  filter_all(all_vars(. != ""))
head(adjusted_data, 10)

# Commands were used to remove both "NA" values and empty values. We can see that the observation with
# an empty value that we had in the first position was removed.

# Univariate descriptive statistics of the dataset
summary(adjusted_data)

# Some of the variables are classified in a way that doesn't make much sense. Let's change
# "Dependents" to numeric and "Credit_History" to character. We can see that "Credit_History" is a
# categorical variable since it refers to the positive or negative credit history of the applicant.
# Remember that encoding categorical variables as "character" is only plausible when the order of
# categories is not relevant.

# Convert the Credit_History variable from numeric to character
adjusted_data$Credit_History <- as.character(adjusted_data$Credit_History)

# Convert the Dependents variable from character to numeric
adjusted_data$Dependents <- as.numeric(adjusted_data$Dependents)
##> adjusted_data$Dependents <- as.numeric(adjusted_data$Dependents)
##Warning message:
##  NAs introduced by coercion

# R ended up replacing the "3+" values in the Dependents variable with "NA". This happened because R
# does not interpret "+" as a numeric value. Let's replace "NA" with "3", keeping in mind that "3"
# actually represents 3 or more dependents of the applicant.
adjusted_data$Dependents <- ifelse(is.na(adjusted_data$Dependents), "3", adjusted_data$Dependents)

# Frequency table of the dependent variable
table(adjusted_data$Loan_Status)

# Categories of categorical variables
levels(factor(adjusted_data$Gender))
levels(factor(adjusted_data$Married))
levels(factor(adjusted_data$Education))
levels(factor(adjusted_data$Self_Employed))
levels(factor(adjusted_data$Credit_History))
levels(factor(adjusted_data$Property_Area))

# Frequency tables of categorical variables
table(adjusted_data$Gender)
table(adjusted_data$Married)
table(adjusted_data$Education)
table(adjusted_data$Self_Employed)
table(adjusted_data$Credit_History)
table(adjusted_data$Property_Area)

# Correlations
chart.Correlation((adjusted_data[, c(3, 6:9)]), histogram = TRUE, cex.labels = 10.5)
# Logically, the highest correlations can be identified between the applicant's income (AplicantIncome)
# and the loan amount (LoanAmount). All other correlations are extremely low.

# "One-hot encoding" or "categorical variable binarization" or "dummy encoding"
dummy_data <- dummy_columns(.data = adjusted_data,
                       select_columns = "Gender",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Married",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Education",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Self_Employed",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Credit_History",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Property_Area",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dummy_data <- dummy_columns(.data = dummy_data,
                       select_columns = "Loan_Status",
                       remove_selected_columns = T,
                       remove_first_dummy = T)

# Note that our output variable "Loan_Status" was also dummy-encoded because R does not interpret
# the values "Y" and "N". Unlike the other dummies, I chose to remove the first dummy instead of
# the most frequent one after cleaning the dataset. I believe that at the end of the model, an output
# variable where "1" indicates loan approval and "0" indicates loan denial is more understandable
# for the model's context. Obviously, this was only possible using the "remove_first_dummy = T" command
# because after cleaning the dataset, our first observation contained the value "N" in "Loan_Status".

# Now that our dataset is "clean" and our categorical variables are properly "dummified",
# we can start building our model.

# Let's create a logistic model by maximum likelihood that can predict whether a person
# has their loan application approved or not through an analysis of their personal characteristics
# and their financial history. Remember that our output variable has been renamed to "Loan_Status_Y"
# due to the process of dummification.

Loan_model <- glm(formula = Loan_Status_Y ~ ., 
                      data = dummy_data, 
                      family = "binomial")

# Model parameters
summary(Loan_model)

confint(Loan_model, level = 0.95)

# Extraction of the Log-Likelihood (LL) value
logLik(Loan_model)

# We have a model whose log-likelihood value seems to indicate a well-fitted model. However,
# for a 95% confidence level, many of the independent variables seem not to be significant.
# We can say this because the "summary" function shows many p-values above 0.05. Additionally, the
# "confint" function, which returns 95% confidence intervals for the coefficients of the fitted logistic
# model, returned many intervals that include the value 0, which may indicate that the variables with
# these intervals do not have a significant effect on the response variable.

# Let's try a model without these independent variables, and then compare the models.

step_model <- step(Loan_model, k = 3.841459)

summary(step_model)

confint(step_model, level = 0.95)

# Comparing Log-Likelihood (LL), AIC, and BIC
logLik(step_model)
logLik(Loan_model)
AIC(step_model)
AIC(Loan_model)
BIC(step_model)
BIC(Loan_model)

# With lower LL, AIC, and BIC values, we can say that all our indicators indicate that
# the "step_model" has a better model fit to the data... However, this is not
# enough to determine which model has better predictive ability. Let's test the accuracy of both.

# Confusion matrix for cutoff = 0.5 using the "confusionMatrix" function from the "caret" package
confusionMatrix(table(predict(Loan_model, type = "response") >= 0.5,
                      dummy_data$Loan_Status_Y == 1)[2:1, 2:1])
confusionMatrix(table(predict(step_model, type = "response") >= 0.5,
                      dummy_data$Loan_Status_Y == 1)[2:1, 2:1])
# In the context of a logistic model, the output is the probability of an event occurring.
# Therefore, when we define a "cutoff" to create a confusion matrix, we are setting a
# threshold that will classify predictions as "correct" or "incorrect". In our case, we can see that
# with a confusion matrix for a cutoff of 0.6, both models had very similar metrics, but the original
# model achieved slightly higher accuracy and specificity, while the stepwise model achieved slightly
# better sensitivity. It should be noted that a cutoff of 0.6 was the one that yielded the best results
# for both models.
# Loan_model
data.frame(Sensitivity = confusionMatrix(table(predict(Loan_model,
                                                         type = "response") >= 0.5,
                                                 dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Specificity = confusionMatrix(table(predict(Loan_model,
                                                          type = "response") >= 0.5,
                                                  dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Accuracy = confusionMatrix(table(predict(Loan_model,
                                                    type = "response") >= 0.5,
                                            dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)
# step_model
data.frame(Sensitivity = confusionMatrix(table(predict(step_model,
                                                         type = "response") >= 0.5,
                                                 dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Specificity = confusionMatrix(table(predict(step_model,
                                                          type = "response") >= 0.5,
                                                  dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Accuracy = confusionMatrix(table(predict(step_model,
                                                    type = "response") >= 0.5,
                                            dummy_data$Loan_Status_Y == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

# In summary, accuracy is a general measure of model precision, sensitivity measures the ability
# to correctly identify instances of the positive class, and specificity measures the ability to
# correctly identify instances of the negative class.

# Finally, let's plot the ROC curve for both models. The ROC curve is an important evaluation metric
# in binary classification tasks. It provides valuable information about the performance of a model
# and its ability to distinguish between positive and negative classes.

predictions <- prediction(predictions = Loan_model$fitted.values, 
                        labels = as.factor(dummy_data$Loan_Status_Y))
sensitivity <- (performance(predictions, measure = "sens"))@y.values[[1]] 

specificity <- (performance(predictions, measure = "spec"))@y.values[[1]]

predictions_step <- prediction(predictions = step_model$fitted.values, 
                        labels = as.factor(dummy_data$Loan_Status_Y))
sensitivity_step <- (performance(predictions_step, measure = "sens"))@y.values[[1]] 

specificity_step <- (performance(predictions_step, measure = "spec"))@y.values[[1]]

# roc function from the pROC package
ROC <- roc(response = dummy_data$Loan_Status_Y, 
           predictor = Loan_model$fitted.values)

ROC_step <- roc(response = dummy_data$Loan_Status_Y, 
           predictor = step_model$fitted.values)

# Plot ROC for Loan_model
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", linewidth = 0.2) +
  geom_line(aes(x = 1 - specificity, y = sensitivity),
            color = "darkorchid", linewidth = 2) +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       title = paste("Area under the curve:",
                     round(ROC$auc, 4),
                     "|",
                     "Gini Coefficient:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

# Plot ROC for step_model
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - specificity, y = sensitivity),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       title = paste("Area under the curve:",
                     round(ROC_step$auc, 4),
                     "|",
                     "Gini Coefficient:",
                     round((ROC_step$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )
# In summary, a larger area under the ROC curve represents a greater ability for accuracy
# in a binary logistic model. When analyzing the ROC curves of our models, we can see that
# the original model has a slightly larger ROC curve.

# Overall, we obtained two reliable models with good predictive ability, as both have
# very similar accuracy. The choice of which model to use would depend on the context.

# For example, let's consider two scenarios: One scenario where the bank or financial institution needs
# a predictive model with a greater ability to correctly predict the occurrence of the event,
# in our case, the event being loan approval, the stepwise model would be preferable.

# In a scenario where the bank or financial institution needs a model with greater predictive
# ability for the non-occurrence of the event, i.e., loan disapproval, our original model may be more suitable.

# Additionally, the models also differ in that the stepwise model is more "simple" as it has fewer
# independent variables, while the original model has triple the number of predictor variables, but
# slightly higher accuracy. Therefore, in a real situation, all model characteristics should be considered,
# and the choice of which model to use would depend on the specific situation.

# Here are the equations of our models:

# Loan_model
extract_eq(Loan_model, use_coefs = T, coef_digits = 5,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# step_model
extract_eq(step_model, use_coefs = T, coef_digits = 5,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# PS: "p̂" represents the estimated probability of the event occurring in a logistic model,
# but to obtain the actual probability, the inverse logistic function (or logit function)
# is applied to the estimate."





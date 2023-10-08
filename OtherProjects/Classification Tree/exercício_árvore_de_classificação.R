#########################
# Package Installation  #
#########################

packages <- c('tidyverse', 'rpart', 'rpart.plot', 'gtools', 'Rmisc', 'scales', 'caret', 'plotROC')

if (sum(as.numeric(!packages %in% installed.packages())) != 0) {
  installer <- packages[!packages %in% installed.packages()]
  for (i in 1:length(installer)) {
    install.packages(installer, dependencies = TRUE)
    break()
  }
  sapply(packages, require, character = TRUE) 
} else {
  sapply(packages, require, character = TRUE) 
}

#######################################################
# CREATING THE DATASET TO BE USED IN THE EXERCISE #
#######################################################

set.seed(42)  # Set a random seed for result reproducibility

# Generate simulated variables with correlation
age <- sample(18:70, 10000, replace = TRUE)

# Generate correlated variables using the rmvnorm() function from the mvtnorm package
library(mvtnorm)

mean_values <- c(5000, 2000, 0.5, 5)  # Means of the variables
correlation_matrix <- matrix(c(1, 0.3, 0.2, -0.1, 0.3, 1, -0.1, 0.2, 0.2, -0.1, 1, 0.4, -0.1, 0.2, 0.4, 1), nrow = 4)  # Correlation matrix

simulated_data <- rmvnorm(10000, mean = mean_values, sigma = correlation_matrix)

income <- simulated_data[, 1]
debt <- simulated_data[, 2]
credit_utilization <- pmin(pmax(simulated_data[, 3], 0), 1)  # Limit credit utilization between 0 and 1
recent_queries <- pmax(simulated_data[, 4], 0)  # Ensure that the number of recent queries is non-negative

# Generate a linear function of explanatory variables
linear_predictor <- -7 - 0.01 * age - 0.0002 * income + 0.003 * debt - 3 * credit_utilization + 0.5 * recent_queries

# Calculate default probability (PD) using the logit link function
prob_default <- plogis(linear_predictor)
# Note: plogis(x) is equivalent to 1/(1+exp(-x))

# Generate default as a Bernoulli variable based on the default probability
default <- rbinom(10000, size = 1, prob = prob_default)

# Create a dataframe
data <- data.frame(age, income, debt, credit_utilization, recent_queries, default)
data$default <- ifelse(data$default == 0, "N", "Y")
data$default <- factor(data$default)

head(data)

#####################################################################################

##################################################
# Separating the dataset and creating the first tree #
################################################## 

# Let's split the dataset into training and testing #
set.seed(123)

## Creating a vector to divide the dataset in a 75/25 proportion
training_bool <- stats::runif(dim(data)[1]) > 0.25

## Using the vector to split the dataset into "training" and "testing"
training_data <- data[training_bool, ]
testing_data <- data[!training_bool, ]

data %>% str

## Creating an "unconstrained" tree using the "training" dataset 
tree <- rpart::rpart(default ~ age + income + debt + credit_utilization + recent_queries,
                     data = training_data,
                     method = 'class',
                     xval = 5,
                     control = rpart.control(cp = 0, 
                                             minsplit = 1, 
                                             maxdepth = 30))

# Checking the tree's complexity
tree$frame

##################################################
# Evaluating the tree on training and testing sets #
##################################################

## Using `predict` to store predictions from the tree using the training and testing datasets.
## Then, we transform the predicted values into Y or N with a cutoff of 0.5 and save them in the "c_train" object.
p_train = stats::predict(tree, training_data)
c_train = base::factor(ifelse(p_train[, 2] > 0.5, "Y", "N"))
p_test = stats::predict(tree, testing_data)
c_test = base::factor(ifelse(p_test[, 2] > 0.5, "Y", "N"))

## Looking at the accuracy of the tree on the "training" dataset observations
tab_train <- table(c_train, training_data$default)
acc_train <- (tab_train[1, 1] + tab_train[2, 2]) / nrow(training_data)
sprintf('Accuracy on training set: %s ', percent(acc_train))

## Looking at the accuracy of the tree on the "testing" dataset observations
tab_test <- table(c_test, testing_data$default)
acc_test <- (tab_test[1, 1] + tab_test[2, 2]) / nrow(testing_data)
sprintf('Accuracy on testing set: %s ', percent(acc_test))

###############################
# ROC Curve TRAINING           #
###############################

# Let's calculate the area under the ROC curve using the Caret package's twoClassSummary function.
# The function expects input in the form of a dataframe with this layout:
# obs: a column containing a factor with observed classes
# pred: a factor with predicted classes
# Y: contains the probability of default
# N: contains the probability of non-default
eval_train <- data.frame(obs = training_data$default, 
                          pred = c_train,
                          Y = p_train[, 2],
                          N = 1 - p_train[, 2]
)

caret::twoClassSummary(eval_train, lev = levels(eval_train$obs))

# We can use the same dataframe to create the ROC curve:
ROC_curve_train <- ggplot2::ggplot(eval_train, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin = 0, end = .25) +
  theme(legend.position = "none") +
  ggtitle("ROC Curve - training set")

ROC_curve_train

###############################
# ROC Curve TESTING            #
###############################
eval_test <- data.frame(obs = testing_data$default, 
                         pred = c_test,
                         Y = p_test[, 2],
                         N = 1 - p_test[, 2]
)

twoClassSummary(eval_test, lev = levels(eval_test$obs))

# We can use the same dataframe to create the ROC curve:
ROC_curve_test <- ggplot(eval_test, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin = 0, end = .25) +
  theme(legend.position = "none") +
  ggtitle("ROC Curve - testing set")

ROC_curve_test

## Our ROC curve using the testing data does not have a very good accuracy... This is probably
## due to overfitting of our tree to the training data. Let's limit the "complexity parameter" (cp) of the tree
## to try to address this issue.

###############################
# Pruned Tree (Grid Search)   #
###############################

## Let's define and save the smallest "complexity parameter" in an object called "cp_min"
tab_cp <- rpart::printcp(tree)
tab_cp

plotcp(tree)

tab_cp[which.min(tab_cp[, 'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[, 'xerror']), 'CP']

## Creating a new tree from the previously defined CP
set.seed(1)
pruned_tree <- rpart::rpart(default ~ age + income + debt + credit_utilization + recent_queries,
                            data = training_data,
                            method = 'class',
                            xval = 0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30))

###########################################################
# Evaluating the "pruned" tree on training and testing sets #
###########################################################

p_train_pruned = stats::predict(pruned_tree, training_data)
c_train_pruned = base::factor(ifelse(p_train_pruned[, 2] > 0.5, "Y", "N"))
p_test_pruned = stats::predict(pruned_tree, testing_data)
c_test_pruned = base::factor(ifelse(p_test_pruned[, 2] > 0.5, "Y", "N"))

# Training dataset
eval_train_pruned <- data.frame(obs = training_data$default, 
                          pred = c_train_pruned,
                          Y = p_train_pruned[, 2],
                          N = 1 - p_train_pruned[, 2]
)

caret::twoClassSummary(eval_train_pruned, lev = levels(eval_train_pruned$obs))

# ROC curve for the "pruned" tree on the training dataset
ROC_curve_train_pruned <- ggplot2::ggplot(eval_train_pruned, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin = 0, end = .25) +
  theme(legend.position = "none") +
  ggtitle("ROC Curve_pruned - training set")

ROC_curve_train_pruned

# Testing dataset
eval_test_pruned <- data.frame(obs = testing_data$default, 
                         pred = c_test_pruned,
                         Y = p_test_pruned[, 2],
                         N = 1 - p_test_pruned[, 2]
)

twoClassSummary(eval_test_pruned, lev = levels(eval_test_pruned$obs))

# ROC curve for the "pruned" tree on the testing dataset
ROC_curve_test_pruned <- ggplot(eval_train_pruned, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin = 0, end = .25) +
  theme(legend.position = "none") +
  ggtitle("ROC Curve_pruned - testing set")

ROC_curve_test_pruned

################################
# Visualizing the pruned tree  #
################################

# Defining a color palette
palette = scales::viridis_pal(begin = .75, end = 1)(20)

# Plotting the tree
rpart.plot::rpart.plot(pruned_tree,
                       box.palette = palette) # Color palette

# Alternative tree plot
prp(pruned_tree, box.palette = palette)




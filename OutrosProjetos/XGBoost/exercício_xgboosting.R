## XGBoosting is a machine learning technique based on decision trees that uses a boosting algorithm 
## to enhance predictive performance. It combines multiple weak decision trees into a stronger model, 
## making it one of the most popular approaches for classification and regression problems.

## Let's do a classification exercise using information recorded by the gyroscope of a cellphone.
## This information relates to the acceleration of different types of movements performed by the 
## cellphone, such as rotational and translational movements around the X, Y, and Z axes. 
## Consequently, our output variable will be the identification of the activity being performed 
## by the cellphone user. Namely: 1 - walking; 2 - going upstairs; 3 - going downstairs; 4 - sitting; 
## 5 - standing; 6 - lying down.

#############################################
#Etapa 1 - Data preprocessing
#############################################
##The dataset is allready divided  in "train" and "test". As we can see, we have a lot of variables.

##The only preprocessing step here is converting the response variable to a factor
##and removing the variable 'y'
load("HAR_train.RData")
load("HAR_test.RData")

##Converting the response variable to a factor and adding the "titles" to its levels
HAR_train$V1 <- as.factor(HAR_train$V1)
levels(HAR_train$V1) <- c("walking", "going_up", "going_down", "sitting", "standing", "lying_down")

HAR_test$V1 <- as.factor(HAR_test$V1)
levels(HAR_test$V1) <- c("walking", "going_up", "going_down", "sitting", "standing", "lying_down")

##Evaluating the response variable
HAR_train$V1 %>% table

##Removing the variable 'y'
HAR_train <- subset(HAR_train, select = -y)
HAR_test <- subset(HAR_test, select = -y)

#############################################
#Step 2 - Basic descriptive analysis
#############################################

##Create boxplots
ggplot(HAR_train) +
  geom_boxplot(aes(x = V1, y = HAR_train[,2])) +
  xlab("Activity performed") +
  ylab(colnames(HAR_train)[2]) +
  ggtitle("Average acceleration (x) by activity")

ggplot(HAR_train) +
  geom_boxplot(aes(x = V1, y = HAR_train[,4])) +
  xlab("Activity performed") +
  ylab(colnames(HAR_train)[2]) +
  ggtitle("Average acceleration (x) by activity")

ggplot(HAR_train) +
  geom_boxplot(aes(x = V1, y = HAR_train[,15])) +
  xlab("Activity performed") +
  ylab(colnames(HAR_train)[2]) +
  ggtitle("Average acceleration (x) by activity")

## We can observe from our boxplots that certain variables are more effective in describing the 
## user's activities. For instance, in the examples provided, only the variable in column 15 
## appears to be a significant indicator of the user's activities. 

#############################################
#Step 3 - Train the model
#############################################

## Since not all variables appear to be effective in explaining the response variable, 
## and since we have a very big number of variables let's select only the most relevant ones and 
## train a model using only those variables.

## Let's train an initial model so that we can identify the most relevant variables from it.

## A great method for variable selection is using the 'variable importances' provided by the 
## traditional tree
set.seed(1729)
tree <- rpart::rpart(V1 ~ .,
                     data=HAR_train,
                     control = rpart.control(cp = 0.1,
                                             minsplit = 2,
                                             maxdepth = 6)
)

variables <- tree$variable.importance %>% sort(decreasing = TRUE) %>% names

variables <- variables[1:3]

variables

control <- caret::trainControl(
  "cv",
  number = 2,
  summaryFunction = multiClassSummary, # Performance evaluation function
  classProbs = TRUE # Required for calculating ROC curve
)

search_grid <- expand.grid(
  nrounds = c(300),
  max_depth = c(2, 8),
  gamma = c(0),
  eta = c(0.05, 0.4),
  colsample_bytree = c(.7),
  min_child_weight = c(1),
  subsample = c(.7)
)

##Run the model
set.seed(1729)
model <- caret::train(
  V1 ~ .,
  data = HAR_train[,c(variables, "V1")],
  method = "xgbTree",
  trControl = control,
  tuneGrid = search_grid,
  verbosity = 0
)


##Model output
model

class_HAR_train <- predict(model, HAR_train)
class_HAR_test <- predict(model, HAR_test)

train_accuracy <- sum(class_HAR_train == HAR_train$V1)/nrow(HAR_train)
train_accuracy

test_accuracy <- sum(class_HAR_test == HAR_test$V1)/nrow(HAR_test)
test_accuracy

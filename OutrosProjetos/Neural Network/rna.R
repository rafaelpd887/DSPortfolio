# Loading the data
load(file='EPA_19.RData')

# Checking its structure
df %>% str

# Separating the numeric variables
cols <- c("fuel_economy_combined", 'eng_disp', 'num_cyl', 'num_gears', 'batt_capacity_ah')

# Function for standardization/scaling
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# Standardization/Scaling
df[cols] <- lapply(df[cols], range01)


##################### 
# One Hot Coding
#######################

m <- model.matrix(fuel_economy_combined ~ ., data = df)
m <- as.matrix(data.frame(m, df[, 1]))

# Adding the output variable to "m"
colnames(m)[28] <- "fuel_economy_combined"

###################################################### 
# Training and testing the neural network
#######################################################

## Using the 'for' loop structure, let's create a neural network model and 
## evaluate it through a 'k-fold' method.

# Making an k-fold
k <- 10 #number of folds
m2 = m[sample(1:nrow(m)), ] 
N <- nrow(m2)

stats <- NULL 
pred <- data.frame(matrix(ncol = ncol(m2), nrow = 0))
  
for (i in 0:(k-1)){
  cv_test <- seq(N)>N*(i/k) & seq(N)<=N*((i+1)/k)
  cv_train <- !(seq(N)>N*(i/k) & seq(N)<=N*((i+1)/k))
    
# Training
nn <- neuralnet(fuel_economy_combined ~ ., 
                data=m2[cv_train,], 
                hidden = c(7, 3), 
                # threshold = 0.8,
                linear.output = TRUE)
    
# Evaluating
pred_tmp <- predict(nn, m2[cv_test,])
pred <- rbind(pred, pred_tmp)
    
}

caret::postResample(pred, m2[,28])

## We have r² = ~0.85, which indicates that our model has a good accuracy when used
## to predict from new data.
  
###################################################### 
# Creating the final model (using all rows)
#######################################################
nn_final <- neuralnet(fuel_economy_combined ~ ., 
                data=m, 
                hidden = c(7, 3), 
                linear.output = TRUE)

pred2 <- predict(nn_final, m)
plot(x = pred2, y = df$fuel_economy_combined)
caret::postResample(pred2, df$fuel_economy_combined)

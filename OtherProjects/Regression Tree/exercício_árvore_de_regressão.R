# Package Installation
packages <- c('tidyverse','rpart','rpart.plot','gtools','Rmisc','scales','viridis','caret','AMR','randomForest','fastDummies','rattle','xgboost','ggpubr','reshape2')

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

# Loading the dataset

data(tips)

tips %>% head

############################
# 1.1 Building the Tree #
############################
tree <- rpart(tip~., 
              data=tips,
              control=rpart.control(maxdepth = 4, cp=0))

# Saving predicted values (p) and error (r) in the dataset
tips['p'] = predict(tree, tips)
tips$p %>% tail # viewing the prediction

tips['r'] = tips$tip - tips$p

# Creating a function to plot the tree
plot <- function(tree_){
  palette <- scales::viridis_pal(begin=.75, end=1)(20)
  plot <- rpart.plot::rpart.plot(tree_,
                                 box.palette = palette)
}
# Plotting the tree
plot(tree)

###########################################
# 1.2 Calculating Evaluation Metrics #
# Calculation of SSE, MSE, SST, MST, and R² #
###########################################

# Creating a function for evaluation
metrics <- function(tips_in, p_var, tip_var){
  n <- dim(tips_in)[1]
  SSE <- sum((tips_in[tip_var] - tips[tip_var])^2)
  MSE <- SSE/n
  
  # Calculation of SST (Sum of Squares Total)
  SST <- sum((tips_in[tip_var] - (tips_in[tip_var] %>% sum)/n)^2)
  MST <- SST/n
  
  # Calculation of R-squared
  R_squared <- 1 - SSE/SST
  
  # Printing the results
  cat("Sum of Squares of Errors (SSE): ", SSE, "\n") 
  cat("Mean Square Error (MSE) : ", MSE, "\n")
  cat("Sum of Squares Total (SST): ", SST, "\n") 
  cat("Mean Square Total (MST): ", MST, "\n")
  cat("R-squared (R²): ", R_squared, "\n")
  
}

metrics(tips, "p", "tip")

#################################
# 1.3 Graphical Analysis        #
#################################

# Function to create a scatterplot of predicted values on x and observed values on y with error represented by colors
plot1 <- function(data, x_var, y_var, r_var) {
  ggplot(data) +
    geom_point(aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(r_var))) +
    theme(legend.position="bottom") +
    ggtitle("Scatterplot") +
    scale_color_viridis_c()
}

# Plotting
plot1(tips, "p", "tip", "r")

#######################################
### Part 2: Tuning the Tree #########        
#######################################

#######################################
# 2.1 Training the Tree Without Constraints   #
#######################################

tree_hm <- rpart(tip~.,
                 data=tips[, !(names(tips) %in% c("p", "r"))],
                 xval=10,
                 control = rpart.control(cp = 0, 
                                         minsplit = 2,
                                         maxdepth = 30)
)

tips['p_hm'] = predict(tree_hm, tips)
tips$p %>% tail # investigate the prediction
tips['r_hm'] = tips$tip - tips$p_hm

######################################
# 2.2 Evaluating the hm Tree        ##
######################################

metrics(tips, "p_hm", "tip")
plot1(tips, "p_hm", "tip", "r_hm")

## We won't plot this tree to avoid computer freezes due to its size.

######################################
# 2.3 Complexity of the Paths     ##
######################################

tab_cp <- rpart::printcp(tree_hm)
rpart::plotcp(tree_hm)

#######################################################################
# 2.4 Choosing the Path that Optimizes Impurity in Cross Validation #
#######################################################################

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

tree_tune <- rpart(tip~., 
                   data=tips[, !(names(tips) %in% c("p", "r", "p_hm", "r_hm"))],
                   xval=0,
                   control = rpart.control(cp = cp_min, 
                                           maxdepth = 30)
)
# Predicted values
tips['p_tune'] = predict(tree_tune, tips)
tips$p %>% tail # investigate the prediction
tips['r_tune'] = tips$tip - tips$p_tune

##############################################
## 2.5 Evaluating the Tuned Tree            ##
##############################################
metrics(tips, "p_tune", "tip")
plot1(tips, "p_tune", "tip", "r_tune")

plot(tree_tune)


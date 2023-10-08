## In the contemporary world, where cardiovascular diseases have become a global concern,
## the ability to identify factors contributing to the development of heart problems
## is fundamental. Understanding the aspects and personal characteristics that play a role
## in this condition allows us to develop more effective and personalized preventive strategies.

## Correspondence analysis is a powerful technique that allows us to identify patterns and
## associations between categorical variables. Based on this methodology, we will investigate
## a wide range of factors, such as age, gender, and medical diagnoses. Through this approach,
## we will gain meaningful insights for a more comprehensive understanding of the characteristics
## and profiles of people affected by heart problems.

# Installation and loading of the required packages
packages <- c("plotly", "tidyverse", "ggrepel", "knitr", "kableExtra", "sjPlot", "FactoMineR", "amap", "ade4", "readxl")

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

# Importing the dataset
heart_data <- read_excel("heart_data_acm.xlsx")

## When analyzing the database, we can see that some variables are quantitative, while
## others are qualitative. Since correspondence analysis is a technique exclusive to
## qualitative variables, we need to transform some of the variables. We could simply
## exclude the quantitative variables, but to avoid losing information from these variables,
## we will proceed with the transformation.

# Let's categorize the quantitative variables (by statistical criteria)
heart_data <- heart_data %>% 
  mutate(Age_Category = case_when(Age <= quantile(Age, 0.25, na.rm = TRUE) ~ "young_age",
                                  Age > quantile(Age, 0.25, na.rm = TRUE) & Age <= quantile(Age, 0.75, na.rm = TRUE) ~ "middle_age",
                                  Age > quantile(Age, 0.75, na.rm = TRUE) ~ "old_age"))

heart_data <- heart_data %>% 
  mutate(Resting_BP_Category = case_when(Resting_BP <= quantile(Resting_BP, 0.25, na.rm = TRUE) ~ "low_resting_BP",
                                         Resting_BP > quantile(Resting_BP, 0.25, na.rm = TRUE) & Resting_BP <= quantile(Resting_BP, 0.75, na.rm = TRUE) ~ "medium_resting_BP",
                                         Resting_BP > quantile(Resting_BP, 0.75, na.rm = TRUE) ~ "high_resting_BP"))

heart_data <- heart_data %>% 
  mutate(Cholesterol_Category = case_when(Cholesterol <= quantile(Cholesterol, 0.25, na.rm = TRUE) ~ "low_cholesterol",
                                          Cholesterol > quantile(Cholesterol, 0.25, na.rm = TRUE) & Cholesterol <= quantile(Cholesterol, 0.75, na.rm = TRUE) ~ "medium_cholesterol",
                                          Cholesterol > quantile(Cholesterol, 0.75, na.rm = TRUE) ~ "high_cholesterol"))

heart_data <- heart_data %>% 
  mutate(Max_HeartRate_Category = case_when(Max_HeartRate <= quantile(Max_HeartRate, 0.25, na.rm = TRUE) ~ "low_Max_HeartRate",
                                            Max_HeartRate > quantile(Max_HeartRate, 0.25, na.rm = TRUE) & Max_HeartRate <= quantile(Max_HeartRate, 0.75, na.rm = TRUE) ~ "medium_Max_HeartRate",
                                            Max_HeartRate > quantile(Max_HeartRate, 0.75, na.rm = TRUE) ~ "high_Max_HeartRate"))

## With the variables properly transformed, we can now remove the quantitative variables
## without losing the information contained in them.

# Let's remove the variables we won't use (quantitative)
heart_data <- heart_data %>% 
  select(-Age, -Resting_BP, -Cholesterol, -Max_HeartRate)

# Checking the data types
str(heart_data)

## Let's change our character variables to factors, as the `dudi.acm` function we will use
## only accepts inputs of this type.

# The function for creating the ACM requires "factors"
heart_data <- as.data.frame(unclass(heart_data), stringsAsFactors = TRUE)

## For correspondence analysis to be performed, we need the variables in the database to be
## associated with at least one other variable present in the database. Since, in the context
## of our analysis, the presence or absence of heart disease is the most relevant aspect,
## let's check if the other variables are related to this last one.

# Contingency tables (do all variables show an association with each other?)
sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Gender,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Chest_Pain_Type,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Blood_Sugar,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Resting_ECG,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Exercise_Angina,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Age_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Resting_BP_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Cholesterol_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = heart_data$Heart_Disease,
         var.col = heart_data$Max_HeartRate_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

## All our variables have a p-value less than 0.05. This indicates that, at a confidence level of 95%,
## all our variables are associated with at least one other variable present in the database. Therefore,
## we can proceed with our analysis.

# Applying correspondence analysis using the `dudi.acm` function
ACM <- dudi.acm(heart_data, scannf = FALSE, nf = 3)

## Since we have multiple pairs of variables, we needed to use a function that performs a
## multiple correspondence analysis (MCA). MCA consists of simple correspondence analyses (SCA)
## that are conducted between all pairs of variables. After completing the SCAs for all pairs,
## the eigenvalues are extracted and can be used, among other things, to obtain the proportions
## of inertia explained by the dimensions. In the end, we will have coordinates that we can use
## to plot a perceptual map that demonstrates the association between the variables.

# Analyzing the variances of each dimension
variance_percent <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(variance_percent, 2), "%")
## The number of dimensions is proportional to the number of variables and categories. For our case,
## we have a total of 17 dimensions. A simple formula to know the total dimensions in ACAs is to subtract
## the total categories from the total variables. We know we have 10 variables in total; let's check if
## we indeed have 27 categories.

# Number of categories per variable
num_categories <- apply(heart_data,
                        MARGIN =  2,
                        FUN = function(x) nlevels(as.factor(x)))
num_categories
## We can see that indeed our variables have a total of 27 categories.

# Consolidating the standard coordinates obtained from the binary matrix
df_ACM <- data.frame(ACM$c1, Variable = rep(names(num_categories),
                                            num_categories))

## We will use the obtained coordinates to plot some graphs that allow for easy visualization
## of associations between the variables.

# Plotting the 2D perceptual map
df_ACM %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Category, color = Variable)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(variance_percent[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(variance_percent[2], 2), "%"))) +
  theme_bw()

# 3D perceptual map (first 3 dimensions)
ACM_3D <- plot_ly()

# Adding coordinates
ACM_3D <- add_trace(ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D

## The large number of variables makes the 3D graph difficult to interpret, but in analyses with a
## smaller number of variables, the 3D graph can be an equally useful, or even more useful, analysis
## tool than the two-dimensional one.

# It is possible to obtain coordinates of the observations
df_obs_coords <- ACM$li

# Plotting the perceptual map of observations
df_obs_coords %>%
  ggplot(aes(x = Axis1, y = Axis2, color = heart_data$Heart_Disease)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimension 1:", paste0(round(variance_percent[1], 2), "%")),
       y = paste("Dimension 2:", paste0(round(variance_percent[2], 2), "%")),
       color = "Heart Disease") +
  theme_bw()

## We can conclude that correspondence analysis allows us to condense categorical (qualitative) variables
## into quantitative variables that can be used to plot graphs for a better visualization of the
## relationship between these variables. This transformation allows us to explore and interpret patterns
## of association between categories more efficiently.

## In addition, the quantitative variables obtained through correspondence analysis can be used in other
## data analysis techniques. For example, these variables can be used in factor analyses to identify
## underlying structures in the data or in clustering to group similar observations. They can also be
## used in supervised predictive models, where they can be employed as predictors in statistical or
## machine learning models. This allows us to explore the relationship between the transformed categorical
## variables and an outcome variable, such as a response variable.

## Therefore, correspondence analysis provides us with an effective way to explore and visualize the
## relationship between categorical variables, and the quantitative variables derived from this analysis
## can be applied in various data analysis techniques, expanding the potential for insights and discoveries
## in exploratory and predictive studies.

## e uma variável de interesse, como uma variável de resposta. 

## Portanto, a análise de correspondência nos proporciona uma maneira eficaz de explorar e visualizar 
## a relação entre variáveis categóricas, e  as variáveis quantitativas derivadas dessa análise 
## podem ser aplicadas em várias técnicas de análise de dados, ampliando o potencial de insights 
## e descobertas nos estudos exploratórios e preditivos.

# Principal Component Analysis (PCA) is a powerful statistical technique that allows us to reduce the complexity of a dataset by identifying underlying patterns and summarizing information into latent factors. The main objective of PCA is to identify and describe these latent constructs or factors that underlie the observed variables. These constructs can be thought of as unobservable variables in our database, while the factors can be interpreted as estimates of these unobservable variables obtained through PCA. In summary, this technique helps us understand and interpret the underlying structure of data by simplifying complexity and extracting information hidden within the observed variables.

# In this case, we will apply PCA based on the Kaiser criterion to extract factors representing the main dimensions influencing the quality of life in Brazilian cities.

# We will explore a comprehensive database containing metrics/variables related to longevity, education, income of inhabitants, as well as variables related to the infrastructure of these cities. Our ultimate goal is to create a ranking of cities, ordering them from best to worst, based on the information extracted from the database through PCA.

# Required packages

packages <- c("tidyverse", "ggrepel", "reshape2", "knitr", "kableExtra", "dplyr", "Hmisc", "ltm", "readxl",
             "PerformanceAnalytics", "plotly", "factoextra", "psych", "sp", "tmap")

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

# Loading the dataset
## We will use a dataset provided by IBGE on the webpage
complete_data <- read_excel("A - Base 20 RMs 2000_2010.xlsx")
## Since the original dataset contains a vast number of variables, we will create a new
## database containing only the most relevant variables for our analysis:
data <- complete_data[, c("NOME_RM", "ESPVIDA", "FECTOT", "MORT5", "SOBRE60", "E_ANOSESTUDO", "T_SUPER25M", "RDPC", "T_DES18M", "T_BANAGUA", "T_LIXO", "T_LUZ", "POP", "IDHM", "IDHM_E", "IDHM_L", "IDHM_R")]
## Below is the legend of the selected variables (averaged or rate-based):
## ESPVIDA = life expectancy;
## FECTOT = number of children per woman;
## MORT5 = under-5 mortality rate;
## SOBRE60 = survival rate up to 60 years of age;
## E_ANOSESTUDO = years of education up to 18 years of age;
## T_SUPER25M = percentage of individuals aged 25 or older with completed higher education;
## RDPC = per capita family income;
## T_DES18M = unemployment rate for individuals aged 18 or older;
## T_BANAGUA = percentage of households with piped water and bathroom;
## T_LIXO = percentage of households served by a garbage collection service;
## T_LUZ = percentage of households with electricity;
## POP = number of people residing in fixed dwellings;
## IDHM = municipal human development index;
## IDHM_E = municipal human development index - education dimension;
## IDHM_L = municipal human development index - longevity dimension;
## IDHM_R = municipal human development index - income dimension;

# Univariate Descriptive Statistics

# Observing the dataset
data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Univariate descriptive statistics
summary(data[, 2:17])

# Analyzing some variables that are likely correlated:
# Let's create some plots to analyze the relationship between some variables. We know that years of
# education tend to be related to income and employment of the population. Therefore, we will use
# the variables related to these factors to start our analysis.
# Scatter and linear fit between years of education and per capita income.
data %>%
  ggplot() +
  geom_point(aes(x = E_ANOSESTUDO, y = RDPC),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = E_ANOSESTUDO, y = RDPC),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Years of Education",
       y = "Per Capita Income") +
  theme_bw()

# Scatter and linear fit between years of education and unemployment rate.
data %>%
  ggplot() +
  geom_point(aes(x = E_ANOSESTUDO, y = T_DES18M),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = E_ANOSESTUDO, y = T_DES18M),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Years of Education",
       y = "Unemployment Rate") +
  theme_bw()

# Principal Component Analysis (PCA)

## We will use the 'principal' function to perform our PCA. This technique involves transforming a set
## of correlated variables into a new set of uncorrelated variables called principal components.

## Since our database has 16 variables, to preserve all the variability present in the data, we will request a total of 16 principal components.

factorial <- principal(data[2:17],
                      nfactors = length(data[2:17]),
                      rotate = "none",
                      scores = TRUE)

## Let's look at the factor loadings of our components:
factorial

## The factor loadings indicate, on a scale from -1 to 1, how much each variable contributes
## to the formation of the component. We can see that our first component seems to capture a good
## portion of the data's variability.

# Initial identification of all eigenvalues
## Eigenvalues indicate the variability of each component. Let's display our eigenvalues with 5 decimal places.
eigenvalues <- round(factorial$values, 5)
eigenvalues

## It is important to note that only 2 eigenvalues are greater than 1 because this detail is relevant
## for the continuation of our analysis based on the Kaiser criterion.

## Summing our eigenvalues to check if the sum results in the exact number of variables in our dataset:
sum(eigenvalues)

## As the goal of PCA is to simplify and condense the amount of information
## contained in the original variables, let's do this simplification now based on the Kaiser criterion. The Kaiser criterion states that only factors with eigenvalues greater than 1 should be considered significant. This means that only factors that explain more variance than an individual variable will be retained.

# Defining the number of factors with eigenvalues greater than 1
k <- sum(eigenvalues > 1)
print(k)

## Since only 2 of our eigenvalues are greater than 1, only 2 factors will be retained.

# Reapplying the PCA analysis using the 'principal' function with only the first 2 components.
factorial2 <- principal(data[2:17],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)

factorial2

## It can be seen here that the first component demonstrates a high ability to condense information from
## the original variables.

# Identifying the shared variance in each component
shared_variance <- as.data.frame(factorial2$Vaccounted) %>% 
  slice(1:3)

rownames(shared_variance) <- c("Eigenvalues",
                                "Proportion of Variance",
                                "Cumulative Proportion of Variance")
round(shared_variance, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

## Here we can see that our first 2 components capture approximately 81% of the variance
## in the original data.

# Visualizing factor scores
factor_scores <- as.data.frame(factorial2$weights)

round(factor_scores, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

## Factor scores represent the contribution of each observation to the construction of each
## principal component. They are calculated by multiplying the factor loadings (weights) of the principal components
## by the original variables of each observation and summing these products.

# Visualizing the factors themselves
factors <- as.data.frame(factorial2$scores)

factors

## Factors are calculated by multiplying the factor scores by the standard deviations of the principal components.
## In factor analysis, factors represent the individual scores of observations on the underlying theoretical constructs.

# Pearson correlation coefficients for each pair of orthogonal factors
rho2 <- rcorr(as.matrix(factors), type = "pearson")
round(rho2$r, 4)

## Here we can see that our factors are not correlated with each other. We can say that the factors
## are orthogonal and do not share any common variance. This indicates that they represent
## distinct dimensions of the latent construct, making the interpretation of results easier.

# Visualization of communalities
communalities <- as.data.frame(unclass(factorial2$communality)) %>%
  rename(communalities = 1)

round(communalities, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

## Communality is an important measure. It ranges from 0 to 1, with 0 indicating that the
## variable has no relationship with any of the factors and 1 indicating that the variable is
## perfectly related to at least one of the factors. We can see that, thanks to the Kaiser criterion,
## even though we retained only 2 factors, we are capturing a good portion
## of the variability in the original data.

# City Rankings

# Let's create a database with the factors to create city rankings:
data_factors <- bind_cols(data,
                           "factor_1" = factors$PC1, 
                           "factor_2" = factors$PC2)

# Assuming only the 2 factors as indicators, calculate the "score."
# In this type of experiment, the score is usually considered as
# factor * variance shared by that factor.

rank_data <- data_factors %>% 
  mutate(score = factor_1 * shared_variance$PC1[2] + factor_2 * shared_variance$PC2[2])

# Visualizing the final ranking in descending order
rank_data[, c(1, 20)] %>%
  arrange(desc(score)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

## When analyzing the city ranking based on principal component analysis (PCA),
## we observe that it consistently reflects the Brazilian reality. Cities like
## São Paulo, Campinas, and Curitiba occupy the top positions, while metropolitan regions
## are at the bottom. This highlights the ability of data analysis to summarize complex information
## contained in an extensive database into just two factors.

## Principal Component Analysis not only allows us to create a city ranking,
## but also provides valuable insights into the underlying constructs related to longevity,
## education, income, and infrastructure in Brazilian cities. During the analysis process,
## we identified and understood some of the factors influencing these dimensions in the cities.

## In addition to ranking, PCA can have various other uses in data science projects. It can serve as
## a basis for creating indices or scores that quantify the performance of observations, help identify
## key variables that impact latent constructs, and provide a clearer view of data structure.

## Thus, principal component analysis has proven to be a powerful tool for understanding and interpreting
## complex databases, allowing the extraction of relevant information in a simpler and more concise manner.
## This approach contributes to informed decision-making and promotes a deeper understanding of phenomena
## resulting from the relationships between variables.

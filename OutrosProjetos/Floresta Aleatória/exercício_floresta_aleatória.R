# Instalação de pacotes
pacotes <- c('tidyverse','rpart','rpart.plot','gtools','Rmisc','scales','viridis','caret','AMR','randomForest','fastDummies','rattle','xgboost','ggpubr','reshape2','mlbench')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) }

# Carregando a base de dados Housing
data(BostonHousing)

# Visualizando as primeiras linhas da base de dados
head(BostonHousing)

#######################################
# Dicionário de dados:                #
#######################################

# CRIM: Taxa de criminalidade per capita por região.
# ZN: Proporção de terrenos residenciais divididos em lotes com mais de 25.000 pés quadrados (cerca de 2.322 metros quadrados).
# INDUS: Proporção de acres não comerciais por cidade.
# CHAS: Variável fictícia (dummy) que indica se o imóvel faz fronteira com o rio Charles (1 se faz fronteira, 0 caso contrário).
# NOX: Concentração de óxidos nítricos (partes por 10 milhões).
# RM: Média de número de quartos por habitação.
# AGE: Proporção de unidades ocupadas pelos proprietários construídas antes de 1940.
# DIS: Distância ponderada até cinco centros de emprego em Boston.
# RAD: Índice de acessibilidade a rodovias radiais.
# TAX: Taxa de imposto sobre propriedades de valor total por $10.000.
# PTRATIO: Razão aluno-professor por cidade.
# B: 1000(Bk - 0.63)^2, onde Bk é a proporção de pessoas de origem afro-americana por cidade.
# LSTAT: Porcentagem de status inferior da população.
# MEDV: Valor mediano das residências ocupadas pelos proprietários em milhares de dólares.

#########################################
# 1) Dividindo amostras de treino e teste #
#########################################
n <- sample(1:2,
            size = nrow(BostonHousing),
            replace = TRUE,
            prob=c(0.8, 0.2))

treino <- BostonHousing[n==1,]
teste <- BostonHousing[n==2,]

#########################################
# 2) Treinando a Random Forest            #
#########################################
floresta <- randomForest::randomForest(
  medv ~ .,
  data = treino,
  ntree = 500
)

predict(floresta, treino) %>% head
predict(floresta, teste) %>% head

#########################################
# 3) Avaliando o modelo                 #
#########################################

# Base de treino
p_treino <- predict(floresta, treino) 
  
# Base de teste
p_teste <- predict(floresta, teste)
  
# Data frame de avaliação (Treino)
aval_treino <- data.frame(obs=treino$medv, 
                          pred=p_treino )

# Data frame de avaliação (Teste)
aval_teste <- data.frame(obs=teste$medv, 
                          pred=p_teste)
  
# Função de avaliação
avalia <- function(pred, obs) {
  mse <- mean((pred - obs)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(pred - obs))
  r_squared <- 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))
    
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r_squared, "\n")}
  
# Usando a função de avaliação
avalia(p_treino, treino$medv)
avalia(p_teste, teste$medv)

##############################################
# 4) Usando o Caret para fazer o grid-search #
##############################################
  
# Validação Cruzada
controle <- caret::trainControl(
  method = 'repeatedcv', 
  number = 4,
  repeats = 2,
  search = 'grid',
  summaryFunction = defaultSummary, 
  classProbs = FALSE )
  
grid <- base::expand.grid(.mtry = 1:10)
  
# Modelando a floresta com a validação cruzada
floresta_grid <- caret::train(medv ~ .,  
                                data = treino,
                                method = 'rf', 
                                metric = 'RMSE', # Choose the best model based on RMSE
                                trControl = controle,
                                ntree = 500,
                                tuneGrid = grid)
  
print(floresta_grid)
plot(floresta_grid)
  
##############################################
# Avaliando o modelo tunado                  #
##############################################
p_treino_grid <- predict(floresta_grid, treino)
p_teste_grid <- predict(floresta_grid, teste)

avalia(p_treino_grid, treino$medv)
avalia(p_teste_grid, teste$medv)

avalia(p_treino, treino$medv)
avalia(p_teste, teste$medv)

aval_treino_grid <- data.frame(obs=treino$medv, 
                          pred=p_treino_grid )

aval_teste_grid <- data.frame(obs=teste$medv, 
                         pred=p_teste_grid)

##############################################
# Conclusão                                  #
##############################################

## Podemos concluir que a floresta "tunada" por grid-search e validação cruzada obteve uma 
## leve melhora na sua acurácia preditiva.
  
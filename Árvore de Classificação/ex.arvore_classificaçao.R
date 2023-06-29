#########################
# Instalação de pacotes #
#########################

pacotes <- c('tidyverse', 'rpart', 'rpart.plot', 'gtools', 'Rmisc', 'scales', 'caret', 'plotROC')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#######################################################
# CRIANDO A BASE DE DADOS PARA SER USADA NO EXERCÍCIO #
#######################################################

set.seed(42)  # Define uma semente aleatória para reprodução dos resultados

# Gerar as variáveis simuladas com correlação
idade <- sample(18:70, 10000, replace = TRUE)

# Gerar variáveis correlacionadas usando a função rmvnorm() do pacote mvtnorm
library(mvtnorm)

mean_values <- c(5000, 2000, 0.5, 5)  # Médias das variáveis
correlation_matrix <- matrix(c(1, 0.3, 0.2, -0.1, 0.3, 1, -0.1, 0.2, 0.2, -0.1, 1, 0.4, -0.1, 0.2, 0.4, 1), nrow = 4)  # Matriz de correlação

simulated_data <- rmvnorm(10000, mean = mean_values, sigma = correlation_matrix)

renda <- simulated_data[, 1]
divida <- simulated_data[, 2]
utilizacao_credito <- pmin(pmax(simulated_data[, 3], 0), 1)  # Limita a utilização de crédito entre 0 e 1
consultas_recentes <- pmax(simulated_data[, 4], 0)  # Garante que o número de consultas recentes seja não negativo

# Gerar função linear das variáveis explicativas
preditor_linear <- -7 - 0.01*idade - 0.0002*renda + 0.003*divida - 3*utilizacao_credito + 0.5*consultas_recentes

# Calcular probabilidade de default (PD) usando a função de link logit
prob_default <- plogis(preditor_linear)
#OBS: problogis(x) é equivalente ao 1/(1+exp(-x))

# Gerar inadimplência como variável Bernoulli com base na probabilidade de default


inadimplencia <- rbinom(10000, size = 1, prob = prob_default)

# Criar dataframe
dados <- data.frame(idade, renda, divida, utilizacao_credito, consultas_recentes, inadimplencia)
dados$inadimplencia <- ifelse(dados$inadimplencia == 0, "N", "Y")
dados$inadimplencia <- factor(dados$inadimplencia)

head(dados)

#####################################################################################

##################################################
#Separando o dataset e criando a primeira árvore #
################################################## 

# Vamos separar a base em treinamento e teste #
set.seed(123)


## criando um vetor para dividir o dataset em uma porporção de 75/25
bool_treino <- stats::runif(dim(dados)[1])>.25

## usando o vetor para separar o dataset em "treino" e "teste"
treino <- dados[bool_treino,]
teste  <- dados[!bool_treino,]

dados %>% str

## criando uma arvore "sem restrições" usando o dataset "treino" 
arvore <- rpart::rpart(inadimplencia ~ idade + renda + divida + utilizacao_credito + consultas_recentes,
                       data=treino,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1, 
                                               maxdepth = 30))

# Verificando a complexidade da árvore
arvore$frame

##################################################
# Avaliando a árvore nas bases de treino e teste #
##################################################

## Usando `predict` para armazenar no obj. "p_treino" as previsoes da arvore usando os datasets
## de treino e de teste. Depois disso transformamos os valores preditos em Y ou N tendo como
## corte a probabilidade de 0,5. e salvamos no obj."c_treino".
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

## Olhando a acurácia da árvore em relação as obs. do dataset "treino"
tab <- table(c_treino, treino$inadimplencia)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

## Olhando a acurácia da árvore em relação as obs. do dataset "teste"
tab2 <- table(c_teste, teste$inadimplencia)
acc2 <- (tab2[1,1]+tab2[2,2])/nrow(teste)
sprintf('Acurácia na base de teste: %s ', percent(acc2))

###############################
# Curva ROC TREINO            #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# Y: contém a probabilidade da inadimplencia
# N: contém a probabilidade da não-inadimplencia
aval_treino <- data.frame(obs=treino$inadimplencia, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

###############################
# Curva ROC TESTE             #
###############################
aval_teste <- data.frame(obs=teste$inadimplencia, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC2 <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC2

## A nossa curva ROC usando os dados de teste não possui uma acurácia muito boa... Isso acontece
## provavelmente devio a um "overfitting" da nossa árvore em relação aos dados de treino. Vamos limitar
## o "complexity parameter" (cp) da árvore para tentarmos resolver esse problema.

###############################
# Árvore podada (Grid Search) #
###############################

## Vamos definir e salvar o cp "ótimo" em um obj. chamado "cp_min"
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

## Criando uma nova árvore a partir do CP definido anteriormente
set.seed(1)
arvore_poda <- rpart::rpart(inadimplencia ~ idade + renda + divida + utilizacao_credito + consultas_recentes,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30))

###########################################################
# Avaliando a árvore "podada" nas bases de treino e teste #
###########################################################

p_treino_poda = stats::predict(arvore_poda, treino)
c_treino_poda = base::factor(ifelse(p_treino_poda[,2]>.5, "Y", "N"))
p_teste_poda = stats::predict(arvore_poda, teste)
c_teste_poda = base::factor(ifelse(p_teste_poda[,2]>.5, "Y", "N"))

# Dataset de treino
aval_treino_poda <- data.frame(obs=treino$inadimplencia, 
                          pred=c_treino_poda,
                          Y = p_treino_poda[,2],
                          N = 1-p_treino_poda[,2]
)

caret::twoClassSummary(aval_treino_poda, lev=levels(aval_treino_poda$obs))

# Curva ROC para a árvore "podada" da base de treino
CurvaROC3 <- ggplot2::ggplot(aval_treino_poda, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC_poda - base de treino")

CurvaROC3

#Dataset de teste
aval_teste_poda <- data.frame(obs=teste$inadimplencia, 
                         pred=c_teste_poda,
                         Y = p_teste_poda[,2],
                         N = 1-p_teste_poda[,2]
)

twoClassSummary(aval_teste_poda, lev=levels(aval_teste_poda$obs))

# Curva ROC para a árvore "podada" da base de teste
CurvaROC4 <- ggplot2::ggplot(aval_treino_poda, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC_poda - base de teste")

CurvaROC4

################################
# Visualizando a árvore podada #
################################

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a árvore
rpart.plot::rpart.plot(arvore_poda,
                       box.palette = paleta) # Paleta de cores

# Plot alternativo da árvore
prp(arvore_poda, box.palette = paleta)



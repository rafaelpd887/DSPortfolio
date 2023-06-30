# Instalação de pacotes
pacotes <- c('tidyverse','rpart','rpart.plot','gtools','Rmisc','scales','viridis','caret','AMR','randomForest','fastDummies','rattle','xgboost','ggpubr','reshape2')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando a base de dados

data(tips)

tips %>% head

############################
# 1.1 Construindo a árvore #
############################
tree <- rpart(tip~., 
              data=tips,
              control=rpart.control(maxdepth = 4, cp=0))

# Salvando valores preditos (p) e erro (r) no dataset
tips['p'] = predict(tree, tips)
tips$p %>% tail # olhando a previsão

tips['r'] = tips$tip - tips$p

# Criando uma função para plotar a árvore
plot <- function(arvore_){
  paleta <- scales::viridis_pal(begin=.75, end=1)(20)
  plot <- rpart.plot::rpart.plot(arvore_,
                                 box.palette = paleta)}
# Plotando a árvore
plot(tree)

###########################################
# 1.2 Calculando indicadores de avaliação #
# Cálculo do SQE, QME, SST, QMT e R²      #
###########################################

# Criando um função para avaliação 
metricas <- function(tips_in, p_var, tip_var){
  n <- dim(tips_in)[1]
  SQE <- sum((tips_in[tip_var] - tips[tip_var])^2)
  QME <- SQE/n
  
  # Cálculo do SSE (Sum of Squares Total)
  SST <- sum((tips_in[tip_var] - (tips_in[tip_var] %>% sum)/n)**2)
  QMT <- SST/n
  
  # Cálculo do R-quadrado
  R_squared <- 1 - SQE/SST
  
  # Imprimindo os resultados
  cat("Soma dos Quadrados dos Erros (SQE): ", SQE, "\n") 
  cat("Quadrado Médio dos Erros (QME) : ", QME, "\n")
  cat("Somatório dos Quadrados Totais (SST): ", SST, "\n") 
  cat("Quadrado Médio Total (QMT): ", QMT, "\n")
  cat("R-quadrado (R²): ", R_squared, "\n")
  
}

metricas(tips, "p", "tip")

#################################
# 1.3 Análise gráfica           #
#################################

# Função para fazer plot dos valores previstos em x e observados em y e erro representado por cores
grafico1 <- function(data, x_var, y_var, r_var) {
  ggplot(data) +
    geom_point(aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(r_var))) +
    theme(legend.position="bottom") +
    ggtitle("Scatterplot") +
    scale_color_viridis_c()
}

# Plotando
grafico1(tips, "p", "tip", "r")

#######################################
### Parte 2: Tunando a árvore #########        
#######################################

######################################
# 2.1 treinar a árvore sem travas
######################################

tree_hm <- rpart(tip~.,
                 data=tips[, !(names(tips) %in% c("p", "r"))],
                 xval=10,
                 control = rpart.control(cp = 0, 
                                         minsplit = 2,
                                         maxdepth = 30)
)

tips['p_hm'] = predict(tree_hm, tips)
tips$p %>% tail # investigar a previsão
tips['r_hm'] = tips$tip - tips$p_hm

######################################
# 2.2 avaliar a árvore hm           ##
######################################

metricas(tips, "p_hm", "tip")
grafico1(tips, "p_hm", "tip", "r_hm")

## n plotaremos essa árvore para evitar travamentos no computador devido a extensão da mesma.

######################################
# 2.3 Complexidade dos caminhos     ##
######################################

tab_cp <- rpart::printcp(tree_hm)
rpart::plotcp(tree_hm)

######################################
# 2.4 Escolher o caminho que otimiza a impureza no cross validation
######################################

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

tree_tune <- rpart(tip~., 
                   data=tips[, !(names(tips) %in% c("p", "r", "p_hm", "r_hm"))],
                   xval=0,
                   control = rpart.control(cp = cp_min, 
                                           maxdepth = 30)
)
# Valores preditos
tips['p_tune'] = predict(tree_tune, tips)
tips$p %>% tail # investigar a previsão
tips['r_tune'] = tips$tip - tips$p_tune

##############################################
## 2.5) Avaliar a árvore tunada             ##
##############################################
metricas(tips, "p_tune", "tip")
grafico1(tips, "p_tune", "tip", "r_tune")

plot(tree_tune)

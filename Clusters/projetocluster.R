# A clusterização, também conhecida como análise de agrupamento, é uma técnica de aprendizado não 
# supervisionado utilizada para identificar padrões e estruturas em conjuntos de dados. O objetivo 
# principal da clusterização é agrupar objetos similares em clusters e objetos diferentes em clusters 
# distintos, de acordo com alguma medida de similaridade ou dissimilaridade entre eles. A clusterização 
# tem várias utilidades como a segmentação de clientes, a detecção de anomalias, a exploração de 
# dados no geral, além de outras. Conseguentemente, a análise de cluster possui aplicações em 
# diferentes áreas.

# O conjunto de dados utilizado nesse projeto se refere a clientes de um distribuidor atacadista Portuguesa. 
# Ele inclui os gastos anuais em unidades monetárias (u.m) em diversas categorias de produtos.

# As observaçoes se referem aos clientes, e as variáveis se dividem da seguinte forma:
# FRESH: gastos anuais (u.m.) em produtos frescos (Contínuo);
# MILK: gastos anuais (u.m.) em produtos lácteos (Contínuo);
# GROCERY: gastos anuais (u.m.) em produtos de mercearia (Contínuo);
# FROZEN: gastos anuais (u.m.) em produtos congelados (Contínuo);
# DETERGENTS_PAPER: gastos anuais (u.m.) em produtos de limpeza e papelaria (Contínuo);
# DELICATESSEN: gastos anuais (u.m.) em produtos de delicatessen (Contínuo);
# CHANNEL: canal dos clientes - Horeca (Hotel/Restaurante/Café) ou canal de varejo (Nominal);
# REGION: região dos clientes - Lisboa, Porto ou Outra (Nominal).

# Sendo "CHANNEL" e "REGION" variáveis categóricas e o restante sendo variáveis quantitativas.

# Nesse projeto realizaremos um processo de clusterização hierárquica, e um processo de clusterização
# "k-means". Em resumo, faremos uma análise de clusters onde o número de clusters será definido durante
# o processo (método hierárquico), e uma análise onde a quantidade de clusters será definida a priori.
# Desse modo, poderemos usar uma prática comum entre os cientistas de dados que consiste em usar o 
# "output" do método hierárquico como "input" do método "k-means".


# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", "fastDummies", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# importação da base de dados
clientesdata <- read.csv("Wholesale customers data.csv")
save(clientesdata, file = "clientesdata.RData")

# Visualização da base de dados
View(clientesdata)
##mostrar apenas 10 primeiiras no rmakrdown

# Contagem das categorias por variável

map(clientesdata[, c("Channel", "Region")], ~ summary(as.factor(.)))
# Onde: Channel(1) = Hotel/Restaurante/Café; Channel(2) = Varejo.
# Region(1) = Lisboa; Region(2) = Porto; Region(3) = Outra Região.

# Olhando o "tipo" das variáveis do nosso banco de dados:
glimpse(clientesdata)

# como as variáveis categóricas estão codificadas como numéricas, vamos alterá-las para fatores:
clientesdata2 <- clientesdata
clientesdata2$Channel <- as.factor(clientesdata$Channel)
clientesdata2$Region <- as.factor(clientesdata$Region)

# Como temos variáveis categóricas e numéricas no banco de dados, vamos separar as variáveis em 
# em dois bancos de dados para que possamos criar 2 matrizes de distância. Esse procedimento se faz
# necessário pois usaremos métodos de cálculo de distâncias diferentes para variáveis numéricas e 
# categóricas. Depois combinaremos as matrizes e faremos a clusterização na matriz combinada.

# Separando as variáveis em numéricas e categóricas:
dados_numericos <- clientesdata2[, c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")]
dados_categoricos <- clientesdata2[, c("Channel", "Region")]

# Padronizando as variáveis numéricas
dados_padronizados <- as.data.frame(scale(dados_numericos))
# agora, todas as variáveis numéricas passam a ter média = 0 e desvio padrão = 1.

# dummificando as variáveis categóricas
dados_dummies <- dummy_columns(.data = dados_categoricos,
                                         select_columns = "Channel",
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                               select_columns = "Region",
                               remove_selected_columns = T,
                               remove_most_frequent_dummy = T)

# Criando nossas matrizes
matriz_D_numerica <- dados_padronizados %>% dist(method = "euclidean")
matriz_D_categorica <- dados_dummies %>% dist(method = "binary")

# Unindo as matrizes
dist_total <- matriz_D_categorica + matriz_D_numerica

# Visualizando a matriz de dissimilaridades
data.matrix(dist_total) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)
## mostrar apenas 10 linhas no markdown
# Como nossas distancias são relativamente pequenas, iremos usar o método de encadeamento completo
# durante a clusterização hierárquica.

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = dist_total, method = "complete")

# Construção do dendrograma
dev.off()
dendo1 <- fviz_dend(x = cluster_hier, show_labels = FALSE)
dendo1
## podemos notar a presença de alguns clusters bem definidos e outros nem tanto... Provavelmente
## temos a presença de alguns outliers no banco de dados.

# Após a análise do dendrograma em um processo de clusterização hierárquica, podemos fazer escolha do número de 
# do número de clusters observando a estrutura do dendrograma e identificando os cortes que 
# parecem mais significativos ou relevantes para o nosso objetivo.


# Dendrograma com visualização dos clusters
# Definindo altura 7 para a definição dos clusters do dendograma.
dendo_clusters <- fviz_dend(x = cluster_hier,
          h = 7,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw(),
          show_labels = FALSE)

dendo_clusters
## podem ser visualizados 12 clusters

# Criando um banco de dados com todos os dados usados na criação das matrizes:
dados_completos <- cbind(dados_padronizados, Channel_2=dados_dummies$Channel_2, Region_1=dados_dummies$Region_1, Region_2=dados_dummies$Region_2)

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters.

dados_completos$cluster_hier <- factor(cutree(tree = cluster_hier, k = 12))
## _ps: 12 é o numero de clusters criados pelo corte na altura 7

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_channel2 <- aov(formula = Channel_2 ~ cluster_hier,
                                data = dados_completos))

summary(anova_region1 <- aov(formula = Region_1 ~ cluster_hier,
                              data = dados_completos))

summary(anova_region2 <- aov(formula = Region_2 ~ cluster_hier,
                             data = dados_completos))

summary(anova_fresh <- aov(formula = Fresh ~ cluster_hier,
                             data = dados_completos))

summary(anova_milk <- aov(formula = Milk ~ cluster_hier,
                           data = dados_completos))

summary(anova_grocery <- aov(formula = Grocery ~ cluster_hier,
                           data = dados_completos))

summary(anova_frozen <- aov(formula = Frozen ~ cluster_hier,
                             data = dados_completos))

summary(anova_detergents <- aov(formula = Detergents_Paper ~ cluster_hier,
                            data = dados_completos))

summary(anova_delicassen <- aov(formula = Delicassen ~ cluster_hier,
                            data = dados_completos))

# Para um nível de confiança de 95%, apenas a variável "Region_1" não pode ser considerada significativa
# para a formação de pelo menos 1 cluster.

# Estatísticas descritivas dos clusters por variável

# Estatísticas descritivas da variável 'Fresh'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Fresh),
    sd = sd(Fresh),
    min = min(Fresh),
    max = max(Fresh))

# Estatísticas descritivas da variável 'Milk'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Milk),
    sd = sd(Milk),
    min = min(Milk),
    max = max(Milk))

# Estatísticas descritivas da variável 'Grocery'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Grocery),
    sd = sd(Grocery),
    min = min(Grocery),
    max = max(Grocery))

# Estatísticas descritivas da variável 'Frozen'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Frozen),
    sd = sd(Frozen),
    min = min(Frozen),
    max = max(Frozen))

# Estatísticas descritivas da variável 'Detergents_Paper'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Detergents_Paper),
    sd = sd(Detergents_Paper),
    min = min(Detergents_Paper),
    max = max(Detergents_Paper))

# Estatísticas descritivas da variável 'Delicassen'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Delicassen),
    sd = sd(Delicassen),
    min = min(Delicassen),
    max = max(Delicassen))

# Estatísticas descritivas da variável 'Channel_2'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Channel_2),
    sd = sd(Channel_2),
    min = min(Channel_2),
    max = max(Channel_2))

# Estatísticas descritivas da variável 'Region_1'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Region_1),
    sd = sd(Region_1),
    min = min(Region_1),
    max = max(Region_1))

# Estatísticas descritivas da variável 'Region_2'
group_by(dados_completos, cluster_hier) %>%
  summarise(
    mean = mean(Region_2),
    sd = sd(Region_2),
    min = min(Region_2),
    max = max(Region_2))

## Através das estatísticas, podemos conpreender as características de cada cluster, e conseguentemente
## a rede varejista saberia como melhor adequar o direcionamento de seus recursos para atender
## a demanda de seus clientes. Nesse nosso exemplo, podemos ainda perceber a presença de outliers, 
## pois os clusters 7, 8, 9, 10, 11 e 12 são formados por uma única observação. Poderíamos remover
## as observações outliers e rodar novamente o algorítmo de clusterização hierarquica. Entretanto, 
## como os outliers representam uma fração muito pequena dos dados (6/440), e estão isolados em clusters 
## próprios, podemos considera-los como irrelevantes para a formação dos demais clusters. Portanto,
## vamos seguir em frente com o objetivo incial e executar agora uma clusterização "k-means" com todas
## as observações do banco de dados (inclusive as obs. outliers) e 12 clusters determinados a priori. 
## Ao final, poderemos comparar os resultados de ambos os procedimentos.

# Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(dados_completos2[,1:9], kmeans, method = "wss", k.max = 30)
## o método de elbow parece nos indicar que o numero ótimo de cluster realmente se encontra por
## volta dos números 11 e 12, conforme nossa análise do dendograma no procedimento hierárquico.

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(select(dados_completos, -cluster_hier),
                         centers = 12)

# Criando variável categórica para indicação do cluster no banco de dados
dados_completos2 <- dados_completos
dados_completos2$cluster_K <- factor(cluster_kmeans$cluster)

summary(anova_channel2 <- aov(formula = Channel_2 ~ cluster_K,
                              data = dados_completos2))

summary(anova_region1 <- aov(formula = Region_1 ~ cluster_K,
                             data = dados_completos2))

summary(anova_region2 <- aov(formula = Region_2 ~ cluster_K,
                             data = dados_completos2))

summary(anova_fresh <- aov(formula = Fresh ~ cluster_K,
                           data = dados_completos2))

summary(anova_milk <- aov(formula = Milk ~ cluster_K,
                          data = dados_completos2))

summary(anova_grocery <- aov(formula = Grocery ~ cluster_K,
                             data = dados_completos2))

summary(anova_frozen <- aov(formula = Frozen ~ cluster_K,
                            data = dados_completos2))

summary(anova_detergents <- aov(formula = Detergents_Paper ~ cluster_K,
                                data = dados_completos2))

summary(anova_delicassen <- aov(formula = Delicassen ~ cluster_K,
                                data = dados_completos2))

## diferente do procedimento hierárquico, temso aqui que todas as variáveis são significantes para a
## formação de pelo menos 1 cluster a um nível de confiança de 95%.

# Estatísticas descritivas dos clusters por variável

# Estatísticas descritivas da variável 'Fresh'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Fresh),
    sd = sd(Fresh),
    min = min(Fresh),
    max = max(Fresh))

# Estatísticas descritivas da variável 'Milk'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Milk),
    sd = sd(Milk),
    min = min(Milk),
    max = max(Milk))

# Estatísticas descritivas da variável 'Grocery'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Grocery),
    sd = sd(Grocery),
    min = min(Grocery),
    max = max(Grocery))

# Estatísticas descritivas da variável 'Frozen'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Frozen),
    sd = sd(Frozen),
    min = min(Frozen),
    max = max(Frozen))

# Estatísticas descritivas da variável 'Detergents_Paper'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Detergents_Paper),
    sd = sd(Detergents_Paper),
    min = min(Detergents_Paper),
    max = max(Detergents_Paper))

# Estatísticas descritivas da variável 'Delicassen'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Delicassen),
    sd = sd(Delicassen),
    min = min(Delicassen),
    max = max(Delicassen))

# Estatísticas descritivas da variável 'Channel_2'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Channel_2),
    sd = sd(Channel_2),
    min = min(Channel_2),
    max = max(Channel_2))

# Estatísticas descritivas da variável 'Region_1'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Region_1),
    sd = sd(Region_1),
    min = min(Region_1),
    max = max(Region_1))

# Estatísticas descritivas da variável 'Region_2'
group_by(dados_completos2, cluster_K) %>%
  summarise(
    mean = mean(Region_2),
    sd = sd(Region_2),
    min = min(Region_2),
    max = max(Region_2))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
dados_completos2 %>%
  select(Channel_2, cluster_hier, cluster_K) %>%
  arrange(Channel_2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)
## mostrar apenas 10 primeiras linhas
# podemos reparar uma grande diferença na distribuiçao das observaçoes para cada cluster

# Comparando atraves de uma matriz de confusao
# Criar a tabela de contingência
tabela_contingencia <- table(dados_completos2$cluster_hier, dados_completos2$cluster_K)

# Exibir a matriz de confusão
matriz_confusao <- prop.table(tabela_contingencia, margin = 1)

# Exibir a matriz de confusão formatada
print(matriz_confusao, digits = 2)

# Finalizando os 2 procedimentos e analisando a matriz de confusao, podemos perceber que os 2 procedimentos
# tenderam a agrupar as observações de maneira semelhante. A matriz de confusão nos mostra a
# porcentagem de observaçoes que estavao agrupadas em um mesmo cluster durante a clusterização 
# hierárquica que continuam agrupadas em um mesmo cluster após a clusterização "k-means". É possível
# reparar que não ocorreu nenhuma grande dispersão de observações, e os 2 clusters do procedimento hierárquico
# que sofreram uma maior dispersão de observações, tiveram, aproximadamente, 50% e 70% dessas observações 
# agrupadas juntas novamente.

# Assim, fica demonstrado a eficácia dos algorítmos de clusterização para o agrupamento de observações,
# e como processo de usar o "output" do método hieráquico como "input" do método k-means pode ser
# uma estratégia válida para esse tipo de análise.


## No mundo contemporâneo, em que doenças cardiovasculares têm se tornado uma preocupação global,
## a capacidade de identificar os fatores que contribuem para o desenvolvimento de problemas cardíacos 
## é fundamental. Compreender os aspectos e as características pessoais que desempenham um papel 
## crucial nessa condição nos permite desenvolver estratégias preventivas mais eficazes e personalizadas.

## A análise de correspondência é uma técnica poderosa que permite identificar padrões e 
## associações entre variáveis categóricas. Com base nessa metodologia, iremos investigar uma 
## ampla gama de fatores, como idade, sexo e diagnósticos médicos. Através dessa abordagem, 
## obteremos insights significativos para uma compreensão mais abrangente das características 
## e perfis das pessoas afetadas por problemas cardíacos.


# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", "tidyverse", "ggrepel", "knitr", "kableExtra", "sjPlot", "FactoMineR", "amap", "ade4","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando a base de dados
dados_cor <- read_excel("dados_cor_acm.xlsx")

## Ao analisarmos o banco de dados, podemos ver que algumas das variáveis são quantitativas, enquanto
## outras são qualitativas. Como a análise de correpondência é uma técnica exclusiva para variáveis
## qualitativas, precisamos transformar algumas das variáveis. Poderíamos simplesmente excluir as variáveis
## quantitativas, mas para evitarmos a perca de informação dessas variáveis, seguiremos com a transformação.

# Vamos categorizar as variáveis quanti (por critério estatístico)
dados_cor <- dados_cor %>% 
  mutate(Categ_Idade = case_when(Idade <= quantile(Idade, 0.25, na.rm = T) ~ "menores_idades",
                                 Idade > quantile(Idade, 0.25, na.rm = T) & Idade <= quantile(Idade, 0.75, na.rm = T) ~ "idades_médias",
                                 Idade > quantile(Idade, 0.75, na.rm = T) ~ "maiores_idades"))

dados_cor <- dados_cor %>% 
  mutate(Categ_PS_Desc = case_when(PS_Descanso <= quantile(PS_Descanso, 0.25, na.rm = T) ~ "PS_descanso_baixo",
                                   PS_Descanso > quantile(PS_Descanso, 0.25, na.rm = T) & PS_Descanso <= quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_médio",
                                   PS_Descanso > quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_alto"))

dados_cor <- dados_cor %>% 
  mutate(Categ_Colest = case_when(Colesterol <= quantile(Colesterol, 0.25, na.rm = T) ~ "menor_colesterol",
                                  Colesterol > quantile(Colesterol, 0.25, na.rm = T) & Colesterol <= quantile(Colesterol, 0.75, na.rm = T) ~ "colesterol_médio",
                                  Colesterol > quantile(Colesterol, 0.75, na.rm = T) ~ "maior_colesterol"))

dados_cor <- dados_cor %>% 
  mutate(Categ_BC_Max = case_when(BC_Max <= quantile(BC_Max, 0.25, na.rm = T) ~ "menor_BC_Max",
                                  BC_Max > quantile(BC_Max, 0.25, na.rm = T) & BC_Max <= quantile(BC_Max, 0.75, na.rm = T) ~ "BC_Max_médio",
                                  BC_Max > quantile(BC_Max, 0.75, na.rm = T) ~ "maior_BC_Max"))

## Com as variáveis devidamente transformadas, podemos agora eliminar as variáveis quantitativas
## sem perder a informação contida nas mesmas.

# Vamos remover as variáveis que não utilizaremos (quantitativas)
dados_cor <- dados_cor %>% 
  select(-Idade, -PS_Descanso, -Colesterol, -BC_Max)

# Conferindo o tipo das variáveis
str(dados_cor)

## Vamos mudar nossas variáveis de caracther para fatores, pois a função `dudi.acm` que usaremos so aceita
## inputs desse tipo.

# A função para a criação da ACM pede que sejam utilizados "fatores"
dados_cor <- as.data.frame(unclass(dados_cor), stringsAsFactors=TRUE) 

## Pra que uma análise de correpondência seja realizada, precisamos que as variáveis do banco de dados
## tenham associação com pelo menos uma outra variável presente no banco de dados. Como no contexto
## da nossa análise a presença, ou a não presença, de doença cardíaca é o aspecto mais relevante,
## vamos verificar se as outras variáveis possuem relação com essa última.

# Tabelas de contingência (todas apresentam associação com alguma variável?)
sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Sexo,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Tipo_Dor_Peito,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Açucar_Sangue,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$ECG_Descanso,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Angina_Exerc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Idade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_PS_Desc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Colest,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_BC_Max,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

## Todas as nossas variáveis possuem um valor-p  menor que 0,05. Isso indica que, para um nível de 
## confiança de 95%, todas as nossas variáveis possuem associação com pelo menos uma outra variável
## presente no banco de dados. Logo, podemos saeguir com a nossa análise.

# Aplicando a análise de correpondência através da função `dudi.acm`
ACM <- dudi.acm(dados_cor, scannf = FALSE, nf = 3)

## Como possuímos vários pares de variáveis, foi preciso usar uma função que execute uma análise 
## de correspondência múltipla (ACM). A ACM consiste em análises de correspondência simples (ACS) 
## que são realizadas entre todos os pares de variáveis. Após finalizar as ACS para todos os pares, 
## os autovalores são extraídos e podem ser usados, entre outras coisas, para obter as proporções 
## de inércia explicadas pelas dimensões. Ao final, teremos coordenadas que poderemos usar para 
## plotar um mapa perceptual que demonstra a associação entre as variáveis.

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")
## O número de dimensões é proporcional ao número de variáveis e de categorias. Para o nosso caso
## temos um total de 17 dimensões. Uma fórmula simples para sabermos o total de dimensões em nas ACMs
## é subtrair o total de categorias pelo total de variáveis. Sabemos que temos 10 variáveis no total,
## vamos conferir se realmente temos 27 categorias.

# Quantidade de categorias por variável
quant_categorias <- apply(dados_cor,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))
quant_categorias
## Podemos ver que realmente nossas variáveis possuem um total de 27 categorias.

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

## Vamos usar as coordenadas obtidas para plotar alguns gráfico que permitam a visualização facilitada
## das associações entre as variáveis.

# Plotando o mapa perceptual 2D
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Mapa perceptual em 3D (3 primeiras dimensões)
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D

## O grande número de variáveis torna o gráfico 3D difícil de interpretar, mas em análises com um número
## menor de variáveis o gráfico 3D pode ser uma ferramenta de análise tão útil, ou até mais útil, quanto o bidimensional.

# É possível obter as coordenadas das observações
df_coord_obs <- ACM$li

# Plotando o mapa perceptual das observações
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = dados_cor$Doença_Card)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Doença Cardíaca") +
  theme_bw()


## Podemos concluir que a análise de correspondência nos permite condensar variáveis categóricas 
## (qualitativas) em variáveis quantitativas, que podem ser utilizadas na plotagem de gráficos 
## para uma melhor visualização da relação entre essas variáveis. Essa transformação permite que
## exploremos e interpretemos os padrões de associação entre as categorias de forma mais eficiente.

## Além disso, as variáveis quantitativas obtidas por meio da análise de correspondência podem ser 
## utilizadas em outras técnicas de análise de dados. Por exemplo, essas variáveis podem ser empregadas 
## em análises fatoriais para identificar estruturas subjacentes nos dados ou em clusterizações para 
## agrupar observações semelhantes. Também é possível utilizar essas variáveis em modelos supervisionados 
## preditivos, onde podem ser empregadas como preditores em modelos estatísticos ou de aprendizado 
## de máquina. Isso permite explorar a relação entre as variáveis categóricas transformadas em quantitativas 
## e uma variável de interesse, como uma variável de resposta. 

## Portanto, a análise de correspondência nos proporciona uma maneira eficaz de explorar e visualizar 
## a relação entre variáveis categóricas, e  as variáveis quantitativas derivadas dessa análise 
## podem ser aplicadas em várias técnicas de análise de dados, ampliando o potencial de insights 
## e descobertas nos estudos exploratórios e preditivos.
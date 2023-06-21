# A análise fatorial por componentes principais é uma técnica estatística poderosa que nos 
# permite reduzir a complexidade de um conjunto de dados, identificando padrões subjacentes e 
# resumindo a informação em fatores latentes. Assim, o objetivo principal da análise fatorial 
# por componentes principais é identificar e descrever os construtos latentes e/ou fatores que 
# estão por trás das variáveis observadas. Esses construtos podem ser entendidos como variáveis 
# não observáveis do nosso banco de dados, enquanto os fatores podem ser interpretados como as estimativas 
# dessas variaveis não observaveis que obtidas após a realização de uma análise fatorial. Em resumo, 
# essa técnica nos ajuda a compreender e interpretar a estrutura subjacente dos dados, simplificando 
# a complexidade e extraindo informações que estão por trás das variáveis observadas."

# Neste caso, aplicaremos uma análise fatorial por componentes principais com base no critério de Kaiser
# para extrair fatores que representam as principais dimensões que influenciam a qualidade de 
# vida nas cidades brasileiras.

# Exploraremos um banco de dados abrangente contendo métricas/variáveis relacionadas à 
# longevidade, educação, e renda dos habitantes, além de variáveis relacionadas à infraestrutura dessas cidades. 
# Nosso objetivo final é criar um ranking das cidades, ordenando-as da melhor para a pior, com base nas 
# informações extraídas do banco de dados através da análise fatorial.


# Pacotes necessários

pacotes <- c("tidyverse","ggrepel","reshape2","knitr","kableExtra","dplyr","Hmisc","ltm","readxl", 
             "PerformanceAnalytics","plotly", "factoextra","psych","sp","tmap")

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
## Vamos usar uma base de dados disponibilizada pelo IBGE na página
dados_completos <- read_excel("A - Base 20 RMs 2000_2010.xlsx")
## Como a base de dados original possui uma imensa quantidade de variáveis, vamos criar um novo
## banco de dados que contenha somente as variáveis mais relevantes para a nossa análise:
dados <- dados_completos[, c("NOME_RM", "ESPVIDA", "FECTOT", "MORT5", "SOBRE60", "E_ANOSESTUDO", "T_SUPER25M", "RDPC", "T_DES18M", "T_BANAGUA", "T_LIXO", "T_LUZ", "POP", "IDHM", "IDHM_E", "IDHM_L", "IDHM_R")]
## Segue abaixo a legenda das variáveis escolhidas (descritas em médias ou taxas):
## ESPVIDA = espectativa de vida;
## FECTOT = número de filhos por mulher;
## MORT5 = taxa de mortalidade até os 5 anos de idade;
## SOBRE60 = taxa de sobrevivência até os 60 anos de idade;
## E_ANOSESTUDO = anos de estudo até os 18 anos de idade;
## T_SUPER25M = taxa de pessoas com 25 anos de idade ou mais que possuem ensino superior completo;
## RDPC = renda familiar per capita;
## T_DES18M = taxa de desempregados com 18 anos de idade ou mais;
## T_BANAGUA = taxa de domicílios com banheiro e água encanados;
## T_LIXO = taxa de domicílios atendidos por um serviço de coleta de lixo;
## T_LUZ = taxa de domicílios com energia elétrica;
## POP = número de pessoas que residem em domicílios fixos;
## IDHM = índice de desenvolvimento humano municipal;
## IDHM_E = índice de desenvolvimento humano municipal - dimensão educação;
## IDHM_L = índice de desenvolvimento humano municipal - dimensão longevidade;
## IDHM_R = índice de desenvolvimento humano municipal - dimensão renda;

# Estatisticas Descitivas

# Observando a base de dados
dados %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)
##colocar em bloco interativo no markdown
# Estatísticas descritivas univariadas
summary(dados[,2:17])
##colocar em bloco interativo no markdown

# Analisando algumas das variáveis que provavelmente são correlacionadas:
# Vamos criar alguns gráficos para analisarmos a relação de algumas variáveis. Sabemos que os anos
# de estudo tendem a ter relação com a renda e a empregabilidade da população. Portanto, vamos usar
# as variáveis referentes a esses fatores para inciarmos nossas análises.
# Scatter e ajuste linear entre os anos de estudo e a renda per capita.
dados %>%
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
  labs(x = "Anos de Estudo",
       y = "Renda per Capita") +
  theme_bw()

# Scatter e ajuste linear entre os anos de estudo e a taxa de desemprego.
dados %>%
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
  labs(x = "Anos de Estudo",
       y = "Desemprego") +
  theme_bw()
## Podemos observar duas correlações significativas nos gráficos acima. Eles mostram uma 
## tendência clara de aumento da renda per capita à medida que o nível de educação aumenta. 
## Além disso, é possível notar uma relação inversa entre o desemprego e o aumento dos anos 
## de estudo. 
##:ps:Devido ao grande número de variáveis, não iremos plotar gráficos para todos os pares de variáveis.


### Elaboração da Análise Fatorial Por Componentes Principais ###

# Coeficientes de correlação de Pearson para cada par de variáveis
## Vamos extrair os coeficientes de correlação utilizando a função 'rcorr' e guarda-los
## em um objeto de nome "rho".
rho <- rcorr(as.matrix(dados[,2:17]), type="pearson")

# Podemos agora extrair algumas informações do objeto "rho" que criamos:
corr_coef <- rho$r # Matriz de correlações
corr_coef
## Essa matriz nos mostra os coeficientes de correlação entre os pares de variáveis.

corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes
corr_sig
## Já essa matriz nos mostra os niveis de significancia para cada par. Considerando um nivel de
## confiança de 95%, podemos ver que quase todos os nossos pares possuem correlação significativa.
## As execeções parecem se concetrar na variável "POP", que indica o número de pessoas que residem
## em moradias fixas. Isso indica que a variável "POP" não possui correlação significativa com as
## outras variáveis.

# Podemos ainda elaborar uma "mapa de calor" para melhor visualizarmos as informações vistas acima:
# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  dados[,2:17] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())
##_ps: as cores mais "quentes" indicam maior correlação positiva, enquanto as cores "frias" indicam
## inxistência de correlação ou correlação negativa.

# Também podemos visualizar essas informações através da função 'chart.Correlation':
chart.Correlation(dados[, 2:17], histogram = TRUE, pch = "+")
## essa função nos permite visualizar os gráficos que plotamos no início juntamente as coeficientes 
## de correlação dos pares de variáveis.

# Teste de esfericidade de Bartlett
## O teste de esfericidade de Bartlett é uma análise estatística utilizada para verificar se as 
## variáveis em um conjunto de dados estão correlacionadas entre si. Apesar de ja termos verificado
## o indício de correlações, vamos usar o teste de Bartlett através da função 'cortest.bartlett'
## como um meio de confirmação:

cortest.bartlett(dados[, 2:17])

## Obtivemos um valor-p muito próximo de zero, e isso indica que as variáveis nos dados não são 
## independentes entre si, sugerindo a presença de correlação significativa entre elas.


# Análise Fatorial por Componentes Principais (PCA)

## Utilizaremos a função 'principal' para realizarmos nossa análise fatorial. Essa técnica consiste
## em transformar um conjunto de variáveis correlacionadas em um novo conjunto de variáveis 
## não correlacionadas denominadas de componentes principais.


## Como o nosso banco de dados possui 16 variáveis, para preservarmos toda a variabilidade presente nos dados, iremos 
## requisitar um total de 16 componentes principais.

fatorial <- principal(dados[2:17],
                      nfactors = length(dados[2:17]),
                      rotate = "none",
                      scores = TRUE)

## Vamos olhar as cargas fatoriais dos nossos componentes:
fatorial
## As cargas fatoriais indicam, através de uma escala de -1 até 1, o quanto cada variável contribui
## para a formação do componente. Conseguentemente, podemos ver que o nosso primeiro componente parece
## capturar uma boa porção da variabilidade dos dados.

# Identificação inicial de todos os autovalores
## Os autovalores indicam a variabilidade de cada componente. Vamos exibir nossos autovalores com
## 5 casas decimais.
autovalores <- round(fatorial$values, 5)
autovalores
## É importante reparar que apenas 2 autovalores são maiores que 1, pois esse detalhe é relevante
## para a continuação da nossa análise com base no critério de Kaiser.

## Somando nossos autovalores para conferir que a soma resulta no exato número de variáveis do nosso
## banco:
sum(autovalores)

## Como o objetivo da análise fatorial é simplificar e condensar a quantidade de informação
## contida nas variáveis originais. Vamos fazer essa simplificação agora com base no critério de
## Kaiser. O critério de Kaiser diz que apenas os fatores com autovalores (eigenvalues) maiores 
## do que 1 devem ser considerados significativos. Isso significa que apenas os fatores que 
## explicam uma quantidade de variância maior do que uma variável individual serão retidos.

# Definição da quantidade de fatores com autovalores maiores que 1
k <- sum(autovalores > 1)
print(k)
## Como apenas 2 dos nossos autovalores são maiores do que 1, apenas 2 fatores serão preservados.

# Reaplicando a análise fatorial através da função 'pincipal' com os 2 primeiros componentes apenas.
fatorial2 <- principal(dados[2:17],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)

fatorial2

## Nota-se aqui que o primeiro componente demonstra alta capacidade de condensar as informações das
## variáveis originais.

# Identificação da variância compartilhada em cada 
variancia_compartilhada <- as.data.frame(fatorial2$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)
## Aqui conseguimos ver que o nossos 2 primeiros componente capturam aproximadamente 81% da variância
## dos dados originais.

# Visualização dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial2$weights)

round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

## Os scores fatoriais representam a contribuição de cada observação para a construção de cada
## componente principal. Eles são calculados multiplicando as cargas fatoriais (pesos) dos componentes principais 
## pelas variáveis originais de cada observação e somando esses produtos.

# Visualização dos fatores propriamente ditos
fatores <- as.data.frame(fatorial2$scores)

fatores

## Os fatores são calculados multiplicando os scores fatoriais pelos desvios padrão dos componentes 
## principais. Os fatores na análise fatorial representam as pontuações individuais das observações nos 
## construtos teóricos subjacentes.

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho2 <- rcorr(as.matrix(fatores), type="pearson")
round(rho2$r, 4)
## Aqui podemos notar que nosso fatores não possuem correlação entre si. Podemos dizer que os fatore
## são ortogonais e não compartilham nenhuma variância em comum. Isso indica que os mesmos representam
## dimensões distintas do construto latente, e conseguentemente facilitam a interpretação dos resultados.

# Visualização das comunalidades 
comunalidades <- as.data.frame(unclass(fatorial2$communality)) %>%
  rename(comunalidades = 1)

round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)
## A comunalidade é uma medida importante. Ela varia de 0 a 1, sendo 0 indicativo de que a 
## variável não tem relação com nenhum dos fatores e 1 indicativo de que a variável está 
## perfeitamente relacionada com pelo menos um dos fatores. Podemos ver que, graças ao critério de
## Kaiser, apesar de termos preservados apenas 2 fatores, estamos conseguindo capturar boa parte
## da variabilidade dos dados originais.

# Rank das cidades

# Vamos criar um banco de dados com os fatores para elaborarmos o rank das cidades:
dados_fatores <- bind_cols(dados,
                           "fator_1" = fatores$PC1, 
                           "fator_2" = fatores$PC2)

# Assumindo-se apenas os 2 fatores como indicadores, calcula-se a "pontuação".
# Onde a pontuação nesse esse tipo de experimento é geralmente considerada como 
# sendo fator * variância compartilhada por aquele fator.

dados_rank <- dados_fatores %>% 
  mutate(pontuacao = fator_1 * variancia_compartilhada$PC1[2] + fator_2 * variancia_compartilhada$PC2[2])

# Visualizando o ranking final em ordem decrescente
dados_rank[,c(1, 20)] %>%
  arrange(desc(pontuacao)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

## Ao analisar o rank das cidades com base na análise fatorial por componentes principais, 
## observamos que ele reflete de maneira consistente a realidade brasileira. Cidades como 
## São Paulo, Campinas e Curitiba ocupam as primeiras posições, enquanto as regiões metropolitanas 
## estão nas últimas posições. Isso evidencia a capacidade da análise de dados em resumir informações 
## complexas contidas em um extenso banco de dados em apenas dois fatores.

## A análise fatorial por componentes principais não apenas nos permitiu criar um rank das 
## cidades, mas também nos proporcionou insights valiosos sobre os construtos latentes 
## subjacentes às variáveis observadas. Durante o processo de análise, pudemos identificar e 
## compreender alguns dos fatores que influenciam a longevidade, educação, renda e 
## infraestrutura nas cidades brasileiras.

## Além do rank, a análise fatorial por componentes principais pode ter diversas outras utilidades 
## em projetos de ciência de dados. Ela pode servir como base para a criação de índices ou scores 
## que quantificam a performance das observações, auxiliar na identificação de variáveis-chave 
## que impactam os construtos latentes e fornecer uma visão mais clara da estrutura dos dados.

## Dessa forma, a análise fatorial por componentes principais demonstrou-se uma ferramenta poderosa 
## para a compreensão e interpretação de um banco de dados complexo, permitindo a extração de informações 
## relevantes de maneira mais simples e concisa. Essa abordagem contribui para embasar tomadas 
## de decisão fundamentadas e promover uma compreensão mais profunda de fenômenos derivados das
## relações entre as variáveis.
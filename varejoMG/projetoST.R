# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("readr","readxl","plotly","trend","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate","rugarch")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# O objetivo deste trabalho é prever as futuras alterações no indice do volume de
# vendas no varejo do estado de Minas Gerais. Vamos utilizar o indice do IBGE como
# série temporal, e estimaremos um modelo ARIMA com ajuste de volatilidade condicional(GARCH)
# nas previsões e um modelo ETS. No final iremos comparar e analisar as previsões de ambos.


#varejo Minas Gerais
varejoMG=gbcbd_get_series(1472,first.date='2000-01-01')
TSvarejoMG=ts(varejoMG[2], start = c(2000,1), end = c(2022,12), frequency = 12)
dygraph(TSvarejoMG)

# Decompor a série temporal
decomposicao <- decompose(TSvarejoMG)

# Visualizar os componentes
plot(decomposicao)
# podemos ver claramente que existe tendencia e sazonalidade na serie

# apesar da clara tendencia, vamos confirmar com o teste de Mann-Kendall
install.packages("trend")
library(trend)
result <- Kendall(x = seq_along(TSvarejoMG), y = TSvarejoMG)
result

# Como o valor estimado para tau é 0.778, e o valor p bilateral é menor ou igual a 2.22e-16, 
# temos indicação de uma tendência na série temporal.

#divisão de janelas, sendo uma para treinarmos o algoritmo, e a outra para verificarmos sua eficiencia

#Minas Gerais
TSvarejotreinoMG=window(TSvarejoMG, start=c(2000,1), end=c(2020,12))
TSvarejotesteMG=window(TSvarejoMG,start=c(2021,1), end=c(2022,12))
length(TSvarejotesteMG)

#plotando as duas séries juntas para checagem

#Minas Gerais
autoplot(TSvarejoMG) +
  autolayer(TSvarejotreinoMG, series="Treino") +
  autolayer(TSvarejotesteMG, series="Teste") +
  scale_color_viridis_d() +
  theme_bw()



## Análise das Séries

ggtsdisplay(TSvarejotreinoMG)


# como detectamos a possível presença de tendencia e sazonalidade, provavelmente estamos lidando
# com uma serie não estacionária. Vamos confirmar.

# Teste de Estacionariedade

#Minas Gerais
testevarejoMG=ur.df(TSvarejotreinoMG)
summary(testevarejoMG)

# A série não é estacionária - precisa ser diferenciada ou transformada para 
# aplicarmos um modelo ARIMA nela. O processo de diferenciação é uma técnica comumente 
# utilizada para transformar séries temporais não estacionárias em séries temporais estacionárias. 
# A ideia básica por trás da diferenciação é calcular a diferença entre observações consecutivas da série.

#vamos ver quantas diferenciações são necessárias

ndiffs(TSvarejotreinoMG)

# 1 diferenciação é necessária para estacionar a série


# Agora que realizamos uma breve análise da série, podemos prosseguir com a modelagem. Geralmente,
# os modelos ARIMA são indicados para séries com presença de tendencia e sazonalidade.
# Eles são capazes de capturar tais padrões por meio de componentes autoregressivos (AR), 
# de média móvel (MA) e de diferenciação (I).

# Além do ARIMA, os modelos ETS também são capazes de lidar com esse tipo de série.Eles 
# são baseados em técnicas de suavização exponencial e são úteis quando os padrões da série são 
# aditivos (A) ou multiplicativos (M), e podem incluir componentes adicionais para capturar a 
# sazonalidade, como a suavização exponencial sazonal (S).

# Assim sendo, como estamos diante de uma serie com tais caracteristicas, vamos usar o R
# para estimar um modelo ARIMA a para nossa serie.Depois, estimaremos um modelo ETS e compararemos
# a capacidade preditiva dos dois.

#Estimando modelos arima

#Vamos usar nossa série temporal de treino "TSvarejotreinoMG" para estimar o modelo
arimavarejoMG=auto.arima(TSvarejotreinoMG, trace=T)

summary(arimavarejoMG)

# Podemos ver que o R nos estimou corretamente um modelo ARIMA sazonal(sARIMA) de 12 períodos.
# Resta agora saber se os parametros estimados; 4 termos autorregressivos(AR), 1 difenrenciação não sazonal(I) e 1 termo de média móvel(MA) não sazonal, além
# de 1 diferenciação sazonal e 2 termos de média movel sazonal; serão capazes de fazer
# previsões precisas. Percebe-se que o R realizou a diferenciação que comentamos um pouco acima.

#### validação e diagnóstico

# 1. Teste de Ljung-Box

# Iremos usar o teste de Ljung-Box para testar a autocorrelação dos resíduos. E desejável que
# os resíduos dos modelos preditivos de series temporais não apresentem autocorrelaçao. Quando os resíduos 
# apresentam autocorrelação, significa que as observações subsequentes estão relacionadas entre si e 
# que a estrutura temporal dos dados não foi adequadamente capturada pelo modelo. 
# Isso pode levar a previsões imprecisas e ineficientes, bem como a interpretações errôneas dos resultados.

checkresiduals(arimavarejoMG)

# Basicamente, quanto mais próximo de 1 forem os valores p no teste de Ljung-Box, menor é
# a chance de autocorrelação entre os resíduos de um modelo desse tipo. Nesse tangente,
# podemos dizer que o nosso modelo parece ser razoável. Entretanto, isso não indica que o modelo seja ruim.
# É importante lembrar que a avaliação da qualidade de uma série temporal não se baseia apenas 
# na ausência de correlação nos resíduos. Outros critérios, como o contexto da serie temporal, 
# a estacionariedade dos dados, a precisão das previsões e a normalidade dos resíduos,
# também devem ser considerados. Vamos fazer mais alguns testes para melhor avaliar o modelo. 

# 2. Normalidade dos resíduos

# Vamos usar o teste de Kolmogorov-Smirnov através da função ks.test para verificarmos
# se os resíduos dos nossos modelos seguem uma distribuição normal. O teste de Kolmogorov-Smirnov,
# é um teste de aderência usado para comparar uma distribuição teórica com uma amostra de dados,
# e será usado no nosso caso para verificar a normalidade dos resíduos. Espera-se que os resíduos
# sigam uma distribuição normal, pois um modelo com resíduos aderentes à normalidade tendem a indicar
# que o modelo é capaz de previsões mais precisas.


ks.test(arimavarejoMG$residuals, "pnorm", mean(arimavarejoMG$residuals),
        sd(arimavarejoMG$residuals))

# Podemos ver que para um nível de significancia de 0.5, os resíduos do nosso modelo
# aparentam não seguir uma distribuição normal(gaussiana)...

# Por fim, vamos testar a presença de efeitos ARCH. Os efeitos ARCH são uma forma específica 
# de heterocedasticidade em que a variância dos erros é modelada como uma função autorregressiva 
# dos próprios erros. Tais efeitos podem ocasionar em ineficiência na estimativa dos parâmetros, 
# violação da suposição de homocedasticidade, dificuldade na interpretação dos resultados e 
# possível autocorrelação residual.

ArchTest(arimavarejoMG$residuals)

# Com base nesses resultados, podemos interpretar que há evidências significativas para rejeitar 
# a hipótese nula, sugerindo que estamos diante de um modelo com efeitos ARCH nos resíduos.
# Apesar dos indicadores negativos, vamos testar a capacidade preditiva do modelo usando a
# nossa série de teste "TSvarejotesteMG"

prevvarejo=forecast::forecast(arimavarejoMG, h=24)

autoplot(prevvarejo) +
  theme_bw()

forecast::accuracy(prevvarejo, TSvarejotesteMG)

ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(TSvarejotesteMG,serie="Valores Reais")+
    autolayer(prevvarejo$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# Curiosamente, obtivemos boas previsões. Porém, como o ArchTest indicou a possível presença
# de heterocedasticidade condicional nos resíduos, vamos tentar implementar um modelo GARCH nos 
# residuos do nosso modelo para obtermos a volatidade condicional dos mesmos. Depois vamos usar essa 
# volatilidade para melhorar ainda mais nossas previsões.

# Para tal, vamos "armazenar" os resíduos do nosso modelo em um objeto:

residuos_arimavarejoMG <- residuals(arimavarejoMG)

# Vamos agora criar um série temporal usando os resíduos:

TSresiduos_arimavarejoMG <- ts(residuos_arimavarejoMG, frequency = 12, start=c(2000,1), end=c(2020,12))

# Geralmente precisariamos diferenciar a serie temporal dos residuos, pois os modelos GARCH
# trabalham com séries de retornos, e não séries de valores absolutos. Entretando, como
# a nossa serie temporal original já foi diferenciada, podemos seguir para a modelagem dos resíduos.
# vamos especificcar um modelo básico GARCH(2,2)

GARCH <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                    mean.model = list(armaOrder = c(0, 0)))

# agora vamos fazer um modelo coms as especificações escolhidas usando a nossa série temporal
# dos resíduos do nosso modelo arima

GARCHresiduos_arimavarejoMG <- ugarchfit(GARCH, data = TSresiduos_arimavarejoMG)

# salvando as previsões de volatilidade condicional 

volatilidade <- sigma(GARCHresiduos_arimavarejoMG)

# aplicando a volatilidade às previsões, vamos criar uma serie temporal com a 
# volatilidade para tal

TSvolatilidade <- ts(volatilidade, start = start(prevvarejo$mean), frequency = frequency(prevvarejo$mean))

prevvarejo$mean_garch <- prevvarejo$mean * sqrt(TSvolatilidade)


media1 <- mean(prevvarejo$mean)
media2 <- mean(prevvarejo$mean_garch)
media3 <- media2/media1

prevvarejo$mean_garch <- (prevvarejo$mean * sqrt(TSvolatilidade)) / media3

## a linha 222 representa uma fórmula comumente usada para ajustar as previsões às volatilidades condicionais.
## já o processo da linha 225 à linha 229 se faz necessário para ajustar e normalizar o impacto das volatilidades.

# testando as previsoes usando a volatilidade garch como ajuste

autoplot(prevvarejo) +
  autolayer(prevvarejo$mean_garch,serie="ajuste GARCH")
  theme_bw()

forecast::accuracy(prevvarejo$mean_garch, TSvarejotesteMG)

ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(TSvarejotesteMG,serie="Valores Reais")+
    autolayer(prevvarejo$mean_garch, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# Como podemos ver, o modelo apresenta boa capacidade preditiva. Vamos agora tentar
# um modelo ETS e comparar ambos os modelos.
# Para uma maior praticidade, vamos combinar alguns comandos e comandar de uma so vez
# que o R estime um modelo ETS e faça previsões para o mesmo

ETSvarejoMG = ets(TSvarejotreinoMG)

ETSprev=forecast::forecast(ETSvarejoMG, h=24)

summary(ETSvarejoMG)

## O "M" indica a presença de sazonalidade multiplicativa, enquanto o "Ad" indica 
## a presença de tendência aditiva. Logo, o R estimou um modelo que é adequado para séries com 
## uma tendência que é adicionada a uma componente sazonal multiplicativa.

autoplot(ETSprev) +
  theme_bw()

ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(TSvarejotesteMG,serie="Valores Reais")+
    autolayer(ETSvarejoMG$mean, serie="Previstos por ETS")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

## Verificando a acurácia preditiva do modelo ETS
forecast::accuracy(ETSprev$mean, TSvarejotesteMG)
## Podemos ver que o modelo ETS teve a melhor acurácia até agora...

## Ljung-Box

checkresiduals(ETSvarejoMG)

## Apesar da alta acurácia, e ao contrário do modelo ARIMA,o teste de Ljung Box indica fortemente 
## a presença de autocorrelação nos resíduos. Isso não é necessariamente ruim, mas indica que o modelo 
## pode não estar capturando toda a estrutura de dependência na série temporal.

## Kolmogorov-Smirnov
ks.test(ETSvarejoMG$residuals, "pnorm", mean(ETSvarejoMG$residuals),
        sd(ETSvarejoMG$residuals))

## Diferente do modelo ARIMA, para um nível de significancia de 0.05, os resíduos do
## modelo ETS seguem uma distribuição normal.

## Lagrange Multiplier
ArchTest(ETSvarejoMG$residuals)

## Na presença de indicação de efeitos ARCH nos resíduos de um modelo ETS, é possível explorar a 
## possibilidade de ajustar o modelo às volatilidades condicionais, assim como foi feito no caso do 
## modelo ARIMA. Nesse contexto, foi realizado um procedimento semelhante, onde um modelo 
## sGARCH(0,1) foi ajustado às volatilidades condicionais. No entanto, os resultados mostraram 
## que esses ajustes não melhoraram as previsões do modelo. Essa constatação sugere que, mesmo com 
## a presença de efeitos ARCH, o modelo ETS funciona melhor sem o ajuste das volatilidades 
## condicionais.

# Olhando a acurácia do ETS "ajustado"
forecast::accuracy(ETSprev$mean_garch, TSvarejotesteMG)
#ps: valores de (p, q) diferentes de (0, 1) apresentaram resultados ainda piores...

# Como podemos ver, o MAPE é levemente superior ao modelo ETS inicial e indica que, 
# diferente do nosso modelo ARIMA, o ajuste das previsões às volatilidades condicionais 
# nesse caso não aprimorou o modelo. Por esse motivo, vamos considerar o modelo ETS incial
# como nosso modelo ETS definitivo.


## No geral, obtivemos 3 modelos com boa capacidade preditiva, mas o modelo ETS
## apresentou o menor MAPE, e portanto a melhor capacidade preditiva. É interessante ressaltar que,
## apesar do ARIMA ter apresentado um melhor ajuste aos dados; pois apresentou AIC, AICc e BIC mais baixos
## que o modelo ETS; o modelo ETS apresentou melhores previsões.

real <- ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(TSvarejotesteMG,serie="Valores Reais")+
    scale_color_manual(values = "yellow") +
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

previsao1 <- ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(prevvarejo$mean, serie="ARIMA")+
    scale_color_manual(values = "red") +
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

previsao2 <- ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(prevvarejo$mean_garch, serie="GARCH")+
    scale_color_manual(values = "blue") +
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

previsao3 <- ggplotly(
  autoplot(TSvarejotreinoMG)+
    autolayer(ETSvarejoMG$mean, serie="ETS")+
    scale_color_manual(values = "green") +
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

imagem_combinada <- plotly::subplot(real, previsao1, previsao2, previsao3, nrows = 2)

imagem_combinada

summary(arimavarejoMG)
summary(ETSvarejoMG)

forecast::accuracy(prevvarejo, TSvarejotesteMG)
forecast::accuracy(prevvarejo$mean_garch, TSvarejotesteMG)
forecast::accuracy(ETSprev, TSvarejotesteMG)

## Podemos conluir que:

## Apesar do ARIMA ser uma escolha adequada para séries com tendência e sazonalidade, 
## isso não garante que ele sempre produzirá as melhores previsões 
## em comparação com outros modelos, como o ETS.

## No nosso caso, o modelo ETS parece ter tido melhores previsões devido à sua capacidade de capturar 
## a tendência e a sazonalidade presentes na série temporal. O modelo ARIMA, mesmo 
## com ajuste de volatilidade condicional, pode não ter sido capaz de capturar 
## esses padrões de forma tão eficiente.

## Portanto, embora o ARIMA seja uma escolha comum para séries com tendência e 
## sazonalidade, é importante considerar outras abordagens, como o ETS, para 
## determinar qual modelo oferece o melhor desempenho preditivo em sua série 
## temporal específica.

## Para que tenhamos convicção nas previsões, é sempre uma boa prática testar diferentes modelos
## e compara-los a fim de obtermos o melhor modelo possível para uma determinada série.
## Uma justificativa para o modelo com valores de AIC, AICc e BIC mais altos apresentar 
## um MAPE mais baixo é que a série temporal em questão pode ser relativamente simples, 
## com padrões facilmente capturados por um modelo menos complexo.




















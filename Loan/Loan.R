#Introdução

#O objetivo deste projeto é desenvolver um modelo logístico para prever se uma pessoa que solicita 
#crédito em uma instituição financeira será atendida ou não. Nosso modelo terá uma variável de saída 
#binária, representada como "1" ou "0" (sim ou não), indicando se o empréstimo será aprovado ou não. Para isso, usaremos várias variáveis independentes relacionadas aos candidatos a empréstimo.

#A concessão de empréstimos é uma atividade essencial em instituições financeiras, mas também 
#envolve riscos. A capacidade de identificar previamente quais candidatos têm maior probabilidade 
#de serem aprovados ou reprovados é fundamental para a tomada de decisões financeiras assertivas. 
#Nesse contexto, o modelo logístico é uma técnica adequada para resolver esse problema, pois é 
#capaz de lidar com variáveis dependentes binárias.


##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic","PerformanceAnalytics")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#                EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS                  #
##############################################################################

# vamos carregar o nosso banco de dados no R
dados <- read.csv("loan.csv")
head(dados, 10)

# A variável Loan_ID é inútil para o nosso modelo, pois ela classifica cada observação de uma maneira
# exclusiva simplesmente para fins de ordenamento do banco de dados.Por esse motivo será impossível 
# extrair alguma informação ou comportamento da mesma que influencia na variável dependente. 
# Excluindo Loan_ID do banco de dados
dados_ajustados <- subset(dados, select = -Loan_ID)

# Podemos ver que algumas das observaçoes apresentam valores vazios/nulos. Nesse caso podemos tentar 
# preencher os valores vazios a partir de alguma estratégia, ou podemos excluir as observações com
# variáveis nulas. Nesse caso vamos optar por excluir as observações com variáveis nulas visto que 
# variáveis como "sexo" não podem ser estimadas a não ser por pura arbitrariedade.

dados_ajustados <- na.omit(dados_ajustados)
dados_ajustados <- dados_ajustados %>%
  filter_all(all_vars(. != ""))
head(dados_ajustados, 10)

# Foram usados comando para eliminarmos tantos os valore "NA", quanto os valores vazios. Podemos que 
# a observação com valor vazio que tinhamaos na primeira posição foi excluída.

#Estatísticas descritivas univariadas da base de dados
summary(dados_ajustados)

# Algumas das variáveis estão classificadas de uma maneira que não faz muito sentido. Vamos alterar
# "Dependents" para numérica e "Credit_History" para caractere. Podemos ver que "Credit_History" é uma 
# variável categórica, pois ela se refere ao histórico de crédito positivo ou negativo do solicitante.
# Lembrando que a codificação de variáveis categóriacas como "character" só é plausível quando a ordem
# das categorias não possui relevância.

# Converter a variável Credit_History de numérico para caractere
dados_ajustados$Credit_History <- as.character(dados_ajustados$Credit_History)

# Converter a variável Dependents de caractere para numérico
dados_ajustados$Dependents <- as.numeric(dados_ajustados$Dependents)
##> dados_ajustados$Dependents <- as.numeric(dados_ajustados$Dependents)
##Warning message:
##  NAs introduced by coercion

# O R acabou substituindo os valores "3+" da variável Dependents por "NA". Isso acounteceu porque o R
# não interpreta o "+" como sendo um valor numérico. Vamos substituir os "NA" por "3", tendo em mente que
# o "3" representa na verdade, 3 ou mais pessoas dependentes do solicitante.
dados_ajustados$Dependents <- ifelse(is.na(dados_ajustados$Dependents), "3", dados_ajustados$Dependents)

#Tabela de frequências absolutas da variável dependente
table(dados_ajustados$Loan_Status)

#Categorias das variáveis categóricas
levels(factor(dados_ajustados$Gender))
levels(factor(dados_ajustados$Married))
levels(factor(dados_ajustados$Education))
levels(factor(dados_ajustados$Self_Employed))
levels(factor(dados_ajustados$Credit_History))
levels(factor(dados_ajustados$Property_Area))

#Tabelas de frequencia das variáveis categóricas
table(dados_ajustados$Gender)
table(dados_ajustados$Married)
table(dados_ajustados$Education)
table(dados_ajustados$Self_Employed)
table(dados_ajustados$Credit_History)
table(dados_ajustados$Property_Area)

#Correlações
chart.Correlation((dados_ajustados[, c(3, 6:9)]), histogram = TRUE, cex.labels = 10.5)
# Logicamente, a maior das correlações pode ser identificada entre a renda do solicitante (AplicantIncome)
# e entre o valor do empréstimo(LoanAmount). Todas as outra correlações são extremamente baixas.

# "One-hot encoding" ou "binarização de variáveis categóricas" ou "Dummização"
dados_dummies <- dummy_columns(.data = dados_ajustados,
                       select_columns = "Gender",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Married",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Education",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Self_Employed",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Credit_History",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Property_Area",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

dados_dummies <- dummy_columns(.data = dados_dummies,
                       select_columns = "Loan_Status",
                       remove_selected_columns = T,
                       remove_first_dummy = T)

# Repare que nossa variável de saída "Loan_Status" também foi dummizada pois o R não interpreta
# os valores "Y" e "N". Diferente das outras dummies, optei por remover a primeira dummy ao invés da 
# mais frequente. Acredito que ao finalizarmos o modelo, uma variável de saída onde "1" indica 
# concessão do empréstimo, e "0" indica não concessão do empréstimo seja mais compreensível para
# o contexto do modelo. Obviamente isso so foi possível usando o comando "remove_first_dummy = T" 
# porque, após a "limpeza" do dataset, a nossa primeira observação contia o valor "N" em "Loan_Status".

# Agora que nosso banco de dados está "limpo" e nossa variáveis categóricas devidamente "dummizadas", 
# podemos iniciar a criação do nosso modelo.

# Vamos criar um modelo logístico por máxima verosemelhança que possa prever se uma pessoa 
# tem sua solicitação de crédito atendida ou não através de uma análise de suas características 
# pessoais e através de seu histórico financeiro. Lembrando que nossa variável de saída teve seu nome
# alterado para "Loan_Status_Y" devido ao processo de dummização.

modelo_Loan <- glm(formula = Loan_Status_Y ~ ., 
                      data = dados_dummies, 
                      family = "binomial")

#Parâmetros do modelo_atrasos
summary(modelo_Loan)

confint(modelo_Loan, level = 0.95)

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_Loan)

# Temos um modelo cujo valor da log-verossimilhança parece indicar um modelo com bom ajuste. Entretando,
# para um nível de confiança de 95%, muitas das variáveis independentes parecem não ser significativas.
# Podemos dizer isso porque a função "summary" mostra muitos valores-p acima de 0.05. Além disso, a função "confint",
# que retorna os intervalos de confiança de 95% para os coeficientes do modelo logístico ajustado, retornou muitos intervalos
# que incluem o valor 0, o que pode indicar que as variáveis que possuem esses intervalos não tenham
# um efeito significativo na variável resposta.

# Vamos tentar um modelo sem essas variáveis independentes, e depois vamos comparar os modelos.

step_Loan <- step(modelo_Loan, k = 3.841459)

summary(step_Loan)

confint(step_Loan, level = 0.95)

#Comparando Log-Likelihood (LL), AIC e BIC
logLik(step_Loan)
logLik(modelo_Loan)
AIC(step_Loan)
AIC(modelo_Loan)
BIC(step_Loan)
BIC(modelo_Loan)

# Com valores de LL, AIC, e BIC menores, podemos dizer que todos os nosso indicativos indicam que 
# o modelo "step_Loan" apresenta um melhor ajuste do modelo aos dados... Porém, isso não é 
# suficiente para afirmarmos qual modelo possui melhor capacidade preditiva. Vamos então testar a
# acurácia de ambos.

#Matriz de confusão para cutoff = 0.5 usando a função "confusionMatrix" do pacote "caret"
confusionMatrix(table(predict(modelo_Loan, type = "response") >= 0.5,
                      dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])
confusionMatrix(table(predict(step_Loan, type = "response") >= 0.5,
                      dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])
# No contexto de um modelo logístico, a saída é a probabilidade de um evento ocorrer.
# Logo, ao definirmos um "cutoff" para a elaboração de uma matriz de confusão, estamos definindo um
# ponto de corte que classificará as previsões em "certas" ou "erradas". No nosso caso, podemos ver 
# que em uma matriz de confusão para cutoff 0.6, ambos os modelos apresentaram métricas
# muito parecidas, mas o modelo original obteve acurácia e especificidade um pouco superiores, 
# enquanto o modelo stepwise obteve uma sensitividade um pouco melhor. Resalta-se que o cutoff de 0.6 foi o que 
# apresentou os melhores resultados para ambos os modelos.
# modelo_Loan
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_Loan,
                                                         type = "response") >= 0.5,
                                                 dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_Loan,
                                                          type = "response") >= 0.5,
                                                  dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acuracia = confusionMatrix(table(predict(modelo_Loan,
                                                    type = "response") >= 0.5,
                                            dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)
# step_Loan
data.frame(Sensitividade = confusionMatrix(table(predict(step_Loan,
                                                         type = "response") >= 0.5,
                                                 dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(step_Loan,
                                                          type = "response") >= 0.5,
                                                  dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acuracia = confusionMatrix(table(predict(step_Loan,
                                                    type = "response") >= 0.5,
                                            dados_dummies$Loan_Status_Y == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

# Em resumo, a acurácia é uma medida geral da precisão do modelo, a sensitividade mede a capacidade 
# de identificar corretamente as instâncias da classe positiva e a especificidade mede a capacidade 
# de identificar corretamente as instâncias da classe negativa.

# Vamos por fim plotar a curva ROC de ambos os modelos. A curva ROC é uma métrica 
# de avaliação importante em tarefas de classificação binária. Ela fornece 
# informações valiosas sobre o desempenho de um modelo e sua capacidade de 
# distinguir entre classes positivas e negativas.

predicoes <- prediction(predictions = modelo_Loan$fitted.values, 
                        labels = as.factor(dados_dummies$Loan_Status_Y))
sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

predicoes_step <- prediction(predictions = step_Loan$fitted.values, 
                        labels = as.factor(dados_dummies$Loan_Status_Y))
sensitividade_step <- (performance(predicoes_step, measure = "sens"))@y.values[[1]] 

especificidade_step <- (performance(predicoes_step, measure = "spec"))@y.values[[1]]

#função roc do pacote pROC
ROC <- roc(response = dados_dummies$Loan_Status_Y, 
           predictor = modelo_Loan$fitted.values)

ROC_step <- roc(response = dados_dummies$Loan_Status_Y, 
           predictor = step_Loan$fitted.values)

# plot ROC
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", linewidth = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", linewidth = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

# plot ROC_step
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC_step$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC_step$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )
# De maneira resumida, uma maior área sob a curva ROC representa uma maior capacidade para acurácia
# de um modelo logístico binário. Ao analisarmos as curvas ROC dos nossos modelos, podemos ver que
# o nosso modelo original apresenta uma curva ROC um pouco maior. 

# No geral, obtivemos dois modelos confiáveis com boa capacidade de predição, pois ambos possuem
# uma acurácia muito similar. A escolha de qual modelo é "melhor" dependeria do contexto.

# Por exemplo, vamos considerar 2 cenários: Um cenário onde o banco ou instituição financeira necessite
# de um modelo pretitivo com uma maior capacidade para prever corretamente a ocorrência do evento,
# no nosso caso o evento sendo a concessão de empréstimo, o modelo com stepwise seria preferível.

# Já em um cenário onde o banco ou instituição financeira precise de um modelo com maior capacidade
# pretitiva para a não ocorrência do evento, ou seja a não aprovação do empréstimo, o nosso modelo original
# pode ser mais indicado.

# Além disso, os modelos também se diferenciam pelo modelo stepwise ser mais "simples" por possuir um
# número menor de variáveis indeprendentes, enquanto o modelo original aprensenta o triplo de variáveis
# preditoras, mas possui um acurácia levemente superior. Portanto, em uma situação real, todas as 
# características dos modelos deveriam ser consideradas, e a escolha de qual modelo usar dependeria 
# da situação em si.

# Segue a abaixo as equações dos nossos modelos:

# modelo_Loan
extract_eq(modelo_Loan, use_coefs = T, coef_digits = 5,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)



# step_Loan
extract_eq(step_Loan, use_coefs = T, coef_digits = 5,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# ps: "p̂" representa a estimativa da probabilidade do evento ocorrer em um modelo logístico, 
# mas para obter a probabilidade real, aplica-se função logística inversa (ou função logit) 
# na estimativa.




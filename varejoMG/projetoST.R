# Installation and Loading of All Packages ---------------------------

packages <- c("readr", "readxl", "plotly", "trend", "tidyverse", "gridExtra", "forecast", "TTR",
             "smooth", "tsibble", "fable", "tsibbledata", "fpp3", "lubridate",
             "urca", "dygraphs", "quantmod", "BETS", "tseries", "FinTS", "feasts",
             "gridExtra", "scales", "caret", "xtable", "tsutils", "GetBCBData", 
             "quantmod", "dgof", "seasonal", "devtools", "transformr", "gganimate", "rugarch")

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

# The objective of this work is to predict future changes in the retail sales volume index
# for the state of Minas Gerais. We will use the IBGE index as a time series and estimate
# an ARIMA model with conditional volatility adjustment (GARCH) in the forecasts and an ETS model.
# In the end, we will compare and analyze the forecasts of both.

# Retail sales in Minas Gerais
varejoMG <- gbcbd_get_series(1472, first.date = '2000-01-01')
TSvarejoMG <- ts(varejoMG[2], start = c(2000, 1), end = c(2022, 12), frequency = 12)
dygraph(TSvarejoMG)

# Decompose the time series
decomposition <- decompose(TSvarejoMG)

# Visualize the components
plot(decomposition)
# We can clearly see that there is a trend and seasonality in the series

# Despite the clear trend, let's confirm with the Mann-Kendall test
install.packages("trend")
library(trend)
result <- Kendall(x = seq_along(TSvarejoMG), y = TSvarejoMG)
result

# Since the estimated value for tau is 0.778, and the bilateral p-value is less than or equal to 2.22e-16,
# we have an indication of a trend in the time series.

# Window division, one for training the algorithm, and the other for verifying its efficiency

# Minas Gerais
TSvarejotreinoMG <- window(TSvarejoMG, start = c(2000, 1), end = c(2020, 12))
TSvarejotesteMG <- window(TSvarejoMG, start = c(2021, 1), end = c(2022, 12))
length(TSvarejotesteMG)

# Plotting both series together for checking

# Minas Gerais
autoplot(TSvarejoMG) +
  autolayer(TSvarejotreinoMG, series = "Training") +
  autolayer(TSvarejotesteMG, series = "Test") +
  scale_color_viridis_d() +
  theme_bw()

## Series Analysis

ggtsdisplay(TSvarejotreinoMG)

# Since we detected the possible presence of trend and seasonality, we are likely dealing
# with a non-stationary series. Let's confirm it.

# Stationarity Test

# Minas Gerais
testevarejoMG <- ur.df(TSvarejotreinoMG)
summary(testevarejoMG)

# The series is not stationary - it needs to be differenced or transformed to apply an ARIMA model to it.
# The differencing process is commonly used to transform non-stationary time series into stationary ones.
# The basic idea behind differencing is to calculate the difference between consecutive observations of the series.

# Let's see how many differences are necessary

ndiffs(TSvarejotreinoMG)

# 1 difference is necessary to make the series stationary

# Now that we have performed a brief analysis of the series, we can proceed with modeling.
# Generally, ARIMA models are suitable for series with trend and seasonality.
# They are capable of capturing such patterns through autoregressive (AR), moving average (MA), and differencing (I) components.

# In addition to ARIMA, ETS models are also capable of handling this type of series.
# They are based on exponential smoothing techniques and are useful when the series patterns are
# additive (A) or multiplicative (M), and can include additional components to capture seasonality,
# such as seasonal exponential smoothing (S).

# Therefore, as we are dealing with such a series, we will use R to estimate an ARIMA model for our series.
# Then, we will estimate an ETS model and compare the predictive ability of both.

# Estimating ARIMA models

# Let's use our training time series "TSvarejotreinoMG" to estimate the model
arimavarejoMG <- auto.arima(TSvarejotreinoMG, trace = TRUE)

summary(arimavarejoMG)

# We can see that R has correctly estimated a seasonal ARIMA (sARIMA) model with a 12-period seasonality.
# Now we need to determine if the estimated parameters, which include 4 autoregressive terms (AR), 1 non-seasonal differencing (I),
# 1 non-seasonal moving average term (MA), as well as 1 seasonal differencing and 2 seasonal moving average terms, will be able to make accurate forecasts.
# It is noted that R has already performed the differencing we mentioned earlier.

#### Validation and Diagnosis

# 1. Ljung-Box Test

# We will use the Ljung-Box test to test the autocorrelation of the residuals. It is desirable that
# the residuals of time series predictive models do not exhibit autocorrelation. When residuals
# exhibit autocorrelation, it means that subsequent observations are related to each other and
# that the temporal structure of the data has not been adequately captured by the model.
# This can lead to inaccurate and inefficient forecasts, as well as misinterpretations of results.

checkresiduals(arimavarejoMG)

# Basically, the closer the p-values in the Ljung-Box test are to 1, the lower the chance of autocorrelation
# among the residuals of a model like this. In this regard, we can say that our model seems reasonable.
# However, this does not mean that the model is bad. It is important to remember that the assessment of the quality
# of a time series is not based solely on the absence of correlation in the residuals. Other criteria, such as
# the context of the time series, data stationarity, forecast accuracy, and residual normality, should also be considered.
# Let's perform some more tests to better evaluate the model.

# 2. Normality of Residuals

# We will use the Kolmogorov-Smirnov test through the ks.test function to check
# if the residuals of our models follow a normal distribution. The Kolmogorov-Smirnov test,
# is a goodness-of-fit test used to compare a theoretical distribution with a sample of data,
# and in our case, it will be used to check the normality of the residuals. It is expected that
# the residuals follow a normal distribution because a model with residuals adhering to normality
# tends to indicate that the model is capable of more accurate predictions.

ks.test(arimavarejoMG$residuals, "pnorm", mean(arimavarejoMG$residuals),
        sd(arimavarejoMG$residuals))

# We can see that for a significance level of 0.05, the residuals of our model
# do not appear to follow a normal (Gaussian) distribution...

# Finally, let's test the presence of ARCH effects. ARCH effects are a specific form of heteroskedasticity
# where the variance of errors is modeled as an autoregressive function of the errors themselves.
# Such effects can result in parameter estimation inefficiency, violation of the homoskedasticity assumption,
# difficulty in interpreting results, and possible residual autocorrelation.

ArchTest(arimavarejoMG$residuals)

# Based on these results, we can interpret that there is significant evidence to reject
# the null hypothesis, suggesting that we are dealing with a model with ARCH effects in the residuals.
# Despite the negative indicators, let's test the predictive capacity of the model using
# our test series "TSvarejotesteMG"

prevvarejo <- forecast::forecast(arimavarejoMG, h = 24)

autoplot(prevvarejo) +
  theme_bw()

forecast::accuracy(prevvarejo, TSvarejotesteMG)

ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(TSvarejotesteMG, series = "Real Values") +
    autolayer(prevvarejo$mean, series = "Forecast") +
    scale_colour_viridis_d() +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

# Interestingly, we obtained good forecasts. However, since the ArchTest indicated the possible presence
# of conditional heteroskedasticity in the residuals, let's try implementing a GARCH model on the
# residuals of our ARIMA model to obtain conditional volatility. Then we will use this
# volatility to further improve our forecasts.

# To do this, let's "store" the residuals of our model in an object:

residuals_arimavarejoMG <- residuals(arimavarejoMG)

# Now let's create a time series using the residuals:

TSresiduos_arimavarejoMG <- ts(residuals_arimavarejoMG, frequency = 12, start = c(2000, 1), end = c(2020, 12))

# Generally, we would need to difference the time series of residuals because GARCH models
# work with return series, not absolute value series. However, as
# our original time series has already been differenced, we can proceed to model the residuals.
# Let's specify a basic GARCH(2,2) model:

GARCH <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                    mean.model = list(armaOrder = c(0, 0)))

# Now let's create a model with the chosen specifications using our time series
# of the residuals of our ARIMA model

GARCHresiduos_arimavarejoMG <- ugarchfit(GARCH, data = TSresiduos_arimavarejoMG)

# Saving the conditional volatility forecasts

volatility <- sigma(GARCHresiduos_arimavarejoMG)

# Applying volatility to the forecasts, let's create a time series with the
# volatility for this purpose

TSvolatilidade <- ts(volatility, start = start(prevvarejo$mean), frequency = frequency(prevvarejo$mean))

prevvarejo$mean_garch <- prevvarejo$mean * sqrt(TSvolatilidade)

media1 <- mean(prevvarejo$mean)
media2 <- mean(prevvarejo$mean_garch)
media3 <- media2/media1

prevvarejo$mean_garch <- (prevvarejo$mean * sqrt(TSvolatilidade)) / media3

## Line 222 represents a formula commonly used to adjust forecasts to conditional volatilities.
## The process from line 225 to line 229 is necessary to adjust and normalize the impact of volatilities.

# Testing the forecasts using the GARCH-adjusted volatility

autoplot(prevvarejo) +
  autolayer(prevvarejo$mean_garch, series = "GARCH adjustment") +
  theme_bw()

forecast::accuracy(prevvarejo$mean_garch, TSvarejotesteMG)

ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(TSvarejotesteMG, series = "Real Values") +
    autolayer(prevvarejo$mean_garch, series = "Forecast") +
    scale_colour_viridis_d() +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

# As we can see, the model shows good predictive ability. Now let's try an ETS model and compare both models.
# For greater convenience, let's combine some commands and instruct R to estimate an ETS model and make predictions for it at once.

ETSvarejoMG <- ets(TSvarejotreinoMG)

ETSprev <- forecast::forecast(ETSvarejoMG, h = 24)

summary(ETSvarejoMG)

## The "M" indicates the presence of multiplicative seasonality, while "Ad" indicates
## the presence of additive trend. Therefore, R has estimated a model suitable for series with
## a trend added to a multiplicative seasonal component.

autoplot(ETSprev) +
  theme_bw()

ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(TSvarejotesteMG, series = "Real Values") +
    autolayer(ETSvarejoMG$mean, series = "Forecasted by ETS") +
    scale_colour_viridis_d() +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

## Checking the predictive accuracy of the adjusted ETS model
forecast::accuracy(ETSprev$mean, TSvarejotesteMG)
## We can see that the ETS model had better accuracy...

## Ljung-Box Test

checkresiduals(ETSvarejoMG)

## Despite the high accuracy, and unlike the ARIMA model, the Ljung-Box test strongly indicates
## the presence of autocorrelation in the residuals. This is not necessarily bad, but it suggests that the model
## may not be capturing the entire dependence structure in the time series.

## Kolmogorov-Smirnov
ks.test(ETSvarejoMG$residuals, "pnorm", mean(ETSvarejoMG$residuals),
        sd(ETSvarejoMG$residuals))

## Unlike the ARIMA model, for a significance level of 0.05, the residuals of the
## ETS model follow a normal distribution.

## Lagrange Multiplier
ArchTest(ETSvarejoMG$residuals)

## In the presence of indications of ARCH effects in the residuals of an ETS model, it is possible to explore the possibility of adjusting the model to conditional volatilities, as was done in the case of the ARIMA model. In this context, a similar procedure was carried out, where an sGARCH(0,1) model was adjusted to the conditional volatilities. However, the results showed that these adjustments did not improve the model's forecasts. This finding suggests that, even with the presence of ARCH effects, the ETS model works better without adjusting conditional volatilities.

# Looking at the accuracy of the adjusted ETS
forecast::accuracy(ETSprev$mean_garch, TSvarejotesteMG)
# P.S.: Values of (p, q) different from (0, 1) presented even worse results...

# As we can see, the MAPE is slightly higher than the original ETS model, indicating that, unlike our ARIMA model, adjusting the forecasts to conditional volatilities in this case did not improve the model. For this reason, let's consider the original ETS model as our definitive ETS model.

## Overall, we obtained three models with good predictive capacity, but the ETS model
## had the lowest MAPE, and therefore the best predictive capacity. It is worth noting that,
## although ARIMA had a better fit to the data, as it had lower AIC, AICc, and BIC values
## compared to the ETS model, the ETS model had better forecasts.

real <- ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(TSvarejotesteMG, series = "Real Values") +
    scale_color_manual(values = "yellow") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

forecast1 <- ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(prevvarejo$mean, series = "ARIMA") +
    scale_color_manual(values = "red") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

forecast2 <- ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(prevvarejo$mean_garch, series = "GARCH") +
    scale_color_manual(values = "blue") +
    scale_colour_viridis_d() +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

forecast3 <- ggplotly(
  autoplot(TSvarejotreinoMG) +
    autolayer(ETSvarejoMG$mean, series = "ETS") +
    scale_color_manual(values = "green") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
)

combined_plot <- plotly::subplot(real, forecast1, forecast2, forecast3, nrows = 2)

combined_plot

summary(arimavarejoMG)
summary(ETSvarejoMG)

forecast::accuracy(prevvarejo, TSvarejotesteMG)
forecast::accuracy(prevvarejo$mean_garch, TSvarejotesteMG)
forecast::accuracy(ETSprev, TSvarejotesteMG)

## We can conclude that:

## Although ARIMA is a suitable choice for series with trend and seasonality, this does not guarantee that it will always produce the best forecasts compared to other models, such as ETS.

## In our case, the ETS model seems to have had better forecasts due to its ability to capture the trend and seasonality present in the time series. The ARIMA model, even with conditional volatility adjustment, may not have been able to capture these patterns as efficiently.

## Therefore, while ARIMA is a common choice for series with trend and seasonality, it is important to consider other approaches, such as ETS, to determine which model offers the best predictive performance for your specific time series.

## To have confidence in the forecasts, it is always a good practice to test different models
## and compare them to obtain the best possible model for a given series.
## One justification for the model with higher AIC, AICc, and BIC values to have a lower MAPE is that the time series in question may be relatively simple, with patterns easily captured by a less complex model.





















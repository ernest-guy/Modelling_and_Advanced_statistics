# Time series modelling using fpp3
# Author: Agaba Ernest

library(fpp3)

# Loading data ------------------------------------------------------------
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

recent_production |>
  autoplot(Beer) +
  labs(
    y = "Megalitres",
    title = "Australian quarterly beer production"
  )
# Modeling------------------------------------
##Naive ----------------------------------
fit_naive <- recent_production |>
  model(NAIVE(Beer))
report (fit_naive)

fit_naive |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residue from the naive method")

fit_ets <- recent_production |>
  model(ETS(Beer))
report(fit_ets)

#alpha-seasonality
#gama-trend
fit_ets |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the ets method")

##ARIMA----------------------------------------
# autoregressive and moving average the two components of arima 
fit_arima <- recent_production |>
  model(ARIMA(Beer)) 
report(fit_arima)
#the [4] represents frequency 

fit_arima |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the arima method")

#Forecasting -------------------------------------------
##Naive-----------------------------------
fit_naive |>
  forecast(h=6) |>
  autoplot(recent_production) +
  labs(title = "Naive forecast",
       y ="Beer")

##Exponential ------------------------------------------
fit_ets |>
  forecast(h=6) |>
  autoplot(recent_production) +
  labs(title = "ets forecast",
       y ="Beer")

##ARIMA-------------------------------
fit_arima |>
  forecast(h=6) |>
  autoplot(recent_production) +
  labs(title = "arima forecast",
       y ="Beer")

#Training and test split------------------------------------
training <- recent_production |> filter(year(Quarter) < 2008)
test <- recent_production |> filter(year(Quarter) >= 2008)

##Naive --------------------------------
naive_fit <- training |>
  model(NAIVE(Beer))

naive_fit |>
  forecast(h = 10) |>
  autoplot(recent_production) +
  labs(title = "naive",
       y = "Beer")

##Seasonal naive-----------------------
snaive_fit <- training |>
  model(SNAIVE(Beer))

snaive_fit |>
  forecast(h = 10) |>
  autoplot(recent_production) +
  labs(title = "snaive",
       y = "Beer")

##Exponential-------------------------------
ets_fit <- training |>
  model(ETS(Beer))

ets_fit |>
  forecast(h = 10) |>
  autoplot(recent_production) +
  labs(title = "ets",
       y = "Beer")
# exponential smoothing is slightly better 

##ARIMA-----------------------------------
arima_fit <- training |>
  model(ARIMA(Beer))

arima_fit |>
  forecast(h = 10) |>
  autoplot(recent_production) +
  labs(title = "arima",
       y = "Beer")

#All at once ----------------------------------
beer_fit <- training |>
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    Seasonalnaive = SNAIVE(Beer),
    Arima = ARIMA(Beer),
    ets = ETS(Beer)
  )

## Forecasting------------------------------------
beer_fc <- beer_fit |>
  forecast(h = 10)
## Plot-------------------------------------
beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL  # This removes the confidence intervals for us to easily see the forecasts
  ) +
  labs(
    title = "Forecasts for quarterly beer production"
  ) +
  guides(
    colour = guide_legend(title = "Forecast")
  )

#Forecast evaluation------------------------------------------
accuracy(beer_fc, recent_production)

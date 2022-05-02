library(shiny)
library(shinydashboard)
library(readr)
library(forecast)
# library(fpp2)
# library(TTR)
library(shinyWidgets)
library(shinycssloaders)
library(shinyglide)
library(highcharter)
library(shinyjs)
library(lubridate)
# library(broom)
library(tidyverse)
library(zoo)


mt2data_df<-readxl::read_xlsx(path = 'www/data/Data_Cassio.xlsx')

mt2data_df<- mt2data_df %>% mutate(Date = as_date(Date))

mt2data_df <- mt2data_df %>% mutate(months = month(Date)) %>%
  mutate(Quarter = case_when(months %in% c(1,2,3) ~ '1º - Tri',
                             months %in% c(4,5,6) ~ '2º - Tri',
                             months %in% c(7,8,9) ~ '3º - Tri',
                             months %in% c(10,11,12) ~'4º - Tri'))%>%
  mutate(dias = 1:228)




# dat_train <- mt2data_df[1:218,]
# dat_test <- mt2data_df[217:228,]

dat_ts <- ts(mt2data_df$Trucks, start = c(2003,1), end = c(2021, 12), frequency = 12)

# - - - - - - - - --  - -
holt_model <- holt(dat_ts, h = 12)
holt_summary <- summary(holt_model)
fitted_holt <- holt_summary$fitted
forecast_holt <- holt_summary$mean[1:12]

# - - - - - - - - --  - -
arima_model <- auto.arima(dat_ts)
fitted_autoarima <-  arima_model$fitted

summary_arima <- summary(arima_model)
forecast_autoarima <- forecast(arima_model)$mean[1:12]



# - - - - - - - - --  - -
mt2data_df<-mt2data_df %>%
  mutate(arima_model = fitted_autoarima,
         holt_model = fitted_holt)

# Forecasting Dataframe
date_col <- as_date(mt2data_df$Date + months(12))
forecasting_df <-tibble(Date = as_date(date_col[217:228]),forecast_autoarima,forecast_holt)




mt2data_df_final <- mt2data_df%>% bind_rows(forecasting_df)



SARIMA_Model <- function(univ_data, test_range, c = 0)
{
  
  # C is the univar. cluster to consider
  
  ## ------------------------------ time series data conv. ------------------------
  c <- c + 1
  if (class(univ_data)[1] != "xts") {
    # the dataset is not give as data.frame
    time <- seq(
      from = as.POSIXct(as.character(univ_data[1, 1])),
      to = as.POSIXct(as.character(univ_data[nrow(univ_data), 1])),
      by = "hour"
    )
    univ_data_xts <- xts::xts(univ_data[, -1], order.by = time)
  } else{
    univ_data_xts = univ_data
    time <- time(univ_data_xts)
  }
  attr(univ_data_xts , 'frequency') <-
    192  # set frequency attribute
  
  Correlation <- Hmisc::rcorr(as.matrix(univ_data_xts))
  
  ##- Visualize data and training/testing regions
  train = xts::xts(data.frame(univ_data_xts)[(1:(nrow(univ_data_xts) - test_range)), ],
                   order.by = time[(1:(nrow(univ_data_xts) - test_range))])
  #valid = window(univ_data_xts,start = (end(univ_data_xts)-(test_range-1)*3600)
  #,end = (end(univ_data_xts)-(test_range/2)*3600))
  test = xts::xts(data.frame(univ_data_xts)[((nrow(univ_data_xts) - test_range +
                                                1):nrow(univ_data_xts)),],
                  order.by = time[((nrow(univ_data_xts) - test_range + 1):nrow(univ_data_xts))])
  
  ##- Decompose a time series into seasonal, trend and irregular components
  
  out <- stl(univ_data_xts[, c], s.window = "per")
  time.series <-
    as.data.frame(cbind(univ_data_xts[, c], out$time.series))
  colnames(time.series) <-
    c("Data", "Seasonal", "Trend", "Remainder")
  time.series$Date <-  time
  time.series <- reshape2::melt(time.series, 'Date')
  
  Plot <- ggplot2::ggplot(time.series, aes(x = Date, y = value)) +
    geom_line() + xlab("Time") + theme_bw() +
    facet_free(variable ~ .)
  rm(out, time.series)
  
  ##- Check Stationarity
  print(tseries::adf.test(ts(univ_data_xts[, c], f = 192)))
  acf(train[, c], lag.max = 20)
  ##- Model building
  write.table(Correlation [["r"]],"Correlation.tsv",sep = '\t',col.names = F,row.names = F)
  
  xreg_d = univ_data_xts[, which(as.data.frame(Correlation [["r"]][c, -c]) >=
                                   0.8)]
  # Remove intercept
  #Automatically select best model based on ADMA on smooth package
  
  
  # Best_ARIMA_model_case <-auto.adam(data.frame(train[,c(which(as.data.frame(Correlation [["r"]][c,])>=0.8))]), "NNN", lags=c(1,24,168),
  #                                            orders=list(ar=c(2,2,2),i=c(1,1,1),ma=c(2,2,2),select=TRUE),
  #                                           silent=TRUE,
  #                                            h=test_range, distribution="dnorm",
  #                                            ic = "AICc",bootstrap=TRUE,
  #                                            loss = "MAE",initial="backcasting",outliers = "ignore",regressors  = "use")
  # Best_ARIMA_model_case2 <-smooth::adam(train[,c(which(as.data.frame(Correlation [["r"]][c,])>=0.8))], "NNN",
  #                                    orders=list(ar=c(2,1,2),i=c(1,0,0),ma=c(2,0,0)),
  #                                    lags=c(1,24,168), h=test_range, distribution="dnorm",
  #                                    loss = "MAE",initial="backcasting",regressors  = "use")
  
  if (ncol(xreg_d) == 0) {
    Best_ARIMA_model_case2 <- smooth::msarima(
      train[, c] ,
      orders = list(
        ar = c(2, 2, 2),
        i = c(1, 0, 0),
        ma = c(2, 0, 0)
      ),
      lags = c(1, 24, 168),
      h = test_range,
      silent = "graph",
      initial = "backcasting",
      ic = "AICc",
      loss = "MAE",
      constant = TRUE
    )
  } else {
    Best_ARIMA_model_case2 <- smooth::msarima(
      train[, c] ,
      orders = list(
        ar = c(2, 2, 2),
        i = c(1, 0, 0),
        ma = c(2, 0, 0)
      ),
      lags = c(1, 24, 168),
      h = test_range,
      silent = "graph",
      initial = "backcasting",
      xreg = xreg_d,
      xregDo = "use",
      ic = "AICc",
      loss = "MAE",
      constant = TRUE
    )
  }
  
  summary(Best_ARIMA_model_case2)
  
  # model test
  xreg_F = test[, which(as.data.frame(Correlation [["r"]][c, ]) >= 0.8)]
  # xreg_F = model.matrix( ~ as.factor(lubridate::wday(
  #   lubridate::dmy_hms("12/04/2019 08:00:00") + c(0:((test_range/2) - 1)) * lubridate::hours(1)
  # )))
  forecast_sarima <-
    Best_ARIMA_model_case2 %>% forecast::forecast(
      h = test_range,
      interval = "parametric",
      level = 0.95,
      side = "both"
    )
  
  SARIMA <- dplyr::full_join(
    data.frame(
      Time = time[1:nrow(train)],
      SARIMA_fit = Best_ARIMA_model_case2$fitted,
      actuals = forecast_sarima$model$y[, 1]
    ),
    data.frame(
      Time = time[(nrow(train) + 1):nrow(univ_data_xts)],
      forecast_mean = as.data.frame(forecast_sarima$mean),
      lower_95 = forecast_sarima$lower,
      upper_95 = forecast_sarima$upper,
      Test_data = test[, c]
    ),
    by = "Time"
  )
  colnames(SARIMA) <-
    c(
      "Time",
      "SARIMA_fit",
      "actuals",
      "forecast_mean",
      "lower_95",
      "upper_95",
      "Test_data"
    )
  
  #colnames(SARIMA)<- c("Time","SARIMA_fit","actuals","forecast_mean","Test_data")
  
  SARIMAxts <- xts::xts(SARIMA[, -1], order.by = time)
  Plot2 <- SARIMAxts %>%
    dygraphs::dygraph(
      xlab = "Time",
      ylab = "Data Traffic(TB)",
      main = paste0("Double seasonal ARIMA model fitting for cluster ", (c - 1))
    ) %>%
    dygraphs::dySeries("actuals", color = "black") %>%
    dygraphs::dySeries(c("lower_95", "forecast_mean", "upper_95"),
             label = "forecast_mean",
             color = "red") %>%
    # dySeries(c("forecast_mean"),
    #          label = "forecast_mean",
    #          color = "red") %>%
    dygraphs::dySeries("Test_data", color = "blue") %>%
    dygraphs::dySeries("SARIMA_fit", color = "green")
  sarima_resid <- Best_ARIMA_model_case2$residuals
  rmse <-
    Metrics::rmse(SARIMA$Test_data[(nrow(train) + 1):nrow(univ_data_xts)], SARIMA$forecast_mean[(nrow(train) +
                                                                                                   1):nrow(univ_data_xts)])
  mae <-
    Metrics::mae(SARIMA$Test_data[(nrow(train) + 1):nrow(univ_data_xts)], SARIMA$forecast_mean[(nrow(train) +
                                                                                                  1):nrow(univ_data_xts)])
  output = list(
    train = train,
    model = Best_ARIMA_model_case2,
    decom_plot = Plot,
    sarima_plot = Plot2,
    forcastdata = SARIMA,
    rmse = rmse,
    mae = mae,
    resid = sarima_resid
  )
  
  output
}

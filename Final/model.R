SARIMA_Model <- function(univ_data, test_range, c = 1) 
{# C is the univar. cluster to consider 
  
  ## ------------------------------ time series data conv. ------------------------
  c=c+1
  if (class(univ_data)[1] != "xts"){# the dataset is not give as data.frame
    time <- seq(from = as.POSIXct(as.character(univ_data[1,1])), 
                to = as.POSIXct(as.character(univ_data[nrow(univ_data),1])), by = "hour")
    univ_data_xts <- xts::xts(univ_data[,-1], order.by = time)
  }else{
    univ_data_xts = univ_data
    time <-time(univ_data_xts) 
    }
  attr(univ_data_xts , 'frequency') <- 192  # set frequency attribute
  
  Correlation <- Hmisc::rcorr(as.matrix(univ_data_xts))
  
  ##- Visualize data and training/testing regions
  train = window(univ_data_xts,end = (end(univ_data_xts)-(test_range/2)*3600))#end = (end(univ_data_xts)-test_range*3600)
  #valid = window(univ_data_xts,start = (end(univ_data_xts)-(test_range-1)*3600)
                 #,end = (end(univ_data_xts)-(test_range/2)*3600))
  test = window(univ_data_xts,start = (end(univ_data_xts)-((test_range/2)-1)*3600))
  
  ##- Decompose a time series into seasonal, trend and irregular components
  
  out <- stl(univ_data_xts[,c], s.window = "per")
  time.series <- as.data.frame(cbind(univ_data_xts[,c], out$time.series))
  colnames(time.series) <- c("Data", "Seasonal", "Trend", "Remainder")
  time.series$Date <-  time
  time.series <- reshape2::melt(time.series, 'Date')
  
  Plot<- ggplot(time.series, aes(x = Date, y = value)) +
    geom_line() + xlab("Time") +theme_bw()+
    facet_free(variable ~ .)
  rm(out, time.series)
  
  ##- Check Stationarity 
  print(tseries::adf.test(ts(univ_data_xts[,c],f=192)))
  
  ##- Model building 
  
  xreg_d = univ_data_xts[,which(as.data.frame(Correlation [["r"]][c,])>=0.8)]
  # Remove intercept
  xreg_d <- xreg_d[, -c]
  #Automatically select best arima model
  # Best_ARIMA_model_case2 <- auto.msarima(train[,c], orders=list(ar=c(2,2,2),i=c(1,1,1),ma=c(2,2,2)),
  #                                        lags=c(1,24,168), h=test_range/2, silent="graph", 
  #                                        initial="backcasting",xreg = xreg_d,xregDo = "use",
  #                                        loss = "MAE",constant = TRUE)
  #SARIMAX(2,1,2)[1](2,0,0)[24](2,0,0)[168]
  
  Best_ARIMA_model_case2 <- msarima(train[,c] , orders=list(ar=c(2,2,2),i=c(1,0,0),ma=c(2,0,0)), 
          lags=c(1,24,168), h=test_range/2, silent="graph", initial="backcasting",
          xreg = xreg_d,xregDo = "use",interval = "l",
          loss = "MAE",constant = TRUE)
  
  summary(Best_ARIMA_model_case2)
  
  # model test 
  xreg_F = test[,which(as.data.frame(Correlation [["r"]][c,])>=0.8)]
  # xreg_F = model.matrix( ~ as.factor(lubridate::wday(
  #   lubridate::dmy_hms("12/04/2019 08:00:00") + c(0:((test_range/2) - 1)) * lubridate::hours(1)
  # )))
  xreg_F = xreg_F[,-c]
  
  forecast_sarima <-Best_ARIMA_model_case2 %>% forecast(h = (test_range/2))
  
  SARIMA <-dplyr::full_join(data.frame(Time = time[1:nrow(train)],
    SARIMA_fit= Best_ARIMA_model_case2$fitted,
    actuals = forecast_sarima$model$y),data.frame(Time = time[nrow(train)+1:nrow(univ_data_xts)],
    forecast_mean = as.data.frame(forecast_sarima$mean),
    lower_95 = forecast_sarima$lower,
    upper_95 = forecast_sarima$upper,Test_data = test[,c]),by="Time")
  colnames(SARIMA)<- c("Time","SARIMA_fit","actuals","forecast_mean","lower_95","upper_95","Test_data")
  SARIMAxts<- xts::xts(SARIMA[,-1], order.by = time)
  Plot2 <- SARIMAxts %>%
    dygraph(xlab ="Time",ylab= "Data Traffic(TB)",main="Double seasonal ARIMA model fitting") %>%
    dySeries("actuals", color = "black") %>%
    dySeries(c("lower_95", "forecast_mean", "upper_95"),
             label = "95%",
             color = "red") %>%
    dySeries("Test_data", color = "blue") %>%
    dySeries("SARIMA_fit", color = "green")
  sarima_resid <- Best_ARIMA_model_case2$residuals
  rmse <- Metrics::rmse(SARIMA$Test_data[nrow(train)+1:nrow(univ_data_xts)],SARIMA$forecast_mean[nrow(train)+1:nrow(univ_data_xts)])
  mae<- Metrics::mae(SARIMA$Test_data[nrow(train)+1:nrow(univ_data_xts)],SARIMA$forecast_mean[nrow(train)+1:nrow(univ_data_xts)])
  output = list(model = Best_ARIMA_model_case2,decom_plot=Plot,sarima_plot= Plot2,forcastdata= SARIMA,rmse=rmse,mae=mae,resid = sarima_resid)
  
  output
}


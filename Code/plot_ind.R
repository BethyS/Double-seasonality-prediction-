forecasted <- readxl::read_excel("Forecast_4_BTS in_cluster1.xlsx", 
                                         sheet = "forecasted")
forecasted <- xts::xts(forecasted, order.by = d.time[(2437:2688)])
forecasted_lower <- readxl::read_excel("Forecast_4_BTS in_cluster1.xlsx", 
                                 sheet = "forecasted_lower")
forecasted_lower <- xts::xts(forecasted_lower, order.by = d.time[(2437:2688)])
forecasted_upper <- readxl::read_excel("Forecast_4_BTS in_cluster1.xlsx", 
                                 sheet = "forecasted_upper")
forecasted_upper <- xts::xts(forecasted_upper, order.by = d.time[(2437:2688)])
forecasted_true <- readxl::read_excel("Forecast_4_BTS in_cluster1.xlsx", 
                                 sheet = "forecasted_true")
forecasted_true <- xts::xts(forecasted_true, order.by = d.time[(2437:2688)])


k =3
SARIMA_new <-data.frame(Time = d.time[(2436+1):nrow(cluster_means_xts)],
                        forecast_mean = as.data.frame(forecasted[,k]),
                        lower_95 = as.data.frame(forecasted_lower[,k]),
                        upper_95 = as.data.frame(forecasted_upper[,k]),
                        Test_data = as.data.frame(forecasted_true[,k]))
colnames(SARIMA_new)<- c("Time","forecast_mean","lower_95","upper_95","Test_data")

#colnames(SARIMA)<- c("Time","SARIMA_fit","actuals","forecast_mean","Test_data")

SARIMA_newxts<- xts::xts(SARIMA_new[,-1], order.by = d.time[(2436+1):nrow(cluster_means_xts)])
SARIMA_newxts %>%
  dygraph(xlab ="Time",ylab= "Data Traffic(TB)",main=paste0("Double seasonal ARIMA model fitting for BTS ", bts_c[k]," in cluster ", cluster)) %>%
  dySeries(c("lower_95", "forecast_mean", "upper_95"),
           label = "forecast_mean",
           color = "red") %>%
  # dySeries(c("forecast_mean"),
  #          label = "forecast_mean",
  #          color = "red") %>%
  dySeries("Test_data", color = "blue")
##########################################################################################################################


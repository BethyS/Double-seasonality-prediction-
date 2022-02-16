SARIMA_Forecast <-
  function(model,
           BTS_Names_Cluster = BTS_Names_Cluster,
           Rawdata = Rawdata,
           cluster = cluster,
           test_range = test_range,
           d.time = d.time)
  {
    bts_c <-
      BTS_Names_Cluster[which(BTS_Names_Cluster[, cluster + 2] != 0, arr.ind = T), cluster +
                          2]
    
    BTS_data_cluster = Rawdata[as.character(bts_c)]
    #percluster_corr <- Hmisc::rcorr(as.matrix(BTS_data_cluster))
    BTS_data_cluster <-
      xts::xts(BTS_data_cluster, order.by = d.time)
    #Every BTS Forecast
    forecasted <-
      foreach (
        i = 1:ncol(BTS_data_cluster),
        # BTS_data_cluster = BTS_data_cluster,
        # model = model,
        # test_range = test_range,
        .combine = 'cbind',
        .packages = c("xts", "tidyverse", "smooth")) %dopar% {

        new_model <- model
        new_model$y[1:2436] <- BTS_data_cluster[1:2436, i]
        forecast_sarima_new <- forecast(new_model,
                                        h = test_range,
                                        interval = "parametric",
                                        side = "both")
        list(
          data.frame(forecast_sarima_new$mean),
          data.frame(forecast_sarima_new$lower),
          data.frame(forecast_sarima_new$upper),
          data.frame(BTS_data_cluster[(2437:2688), i])
        )
        
        
      }
    forecasted
  }

SARIMA_Forecast <-
  function(model,
           BTS_Names_Cluster = BTS_Names_Cluster,
           Rawdata = Rawdata,
           cluster = cluster)
  {
    #Every BTS Forcast
    bts_c <-
      BTS_Names_Cluster[which(BTS_Names_Cluster[, cluster + 2] != 0, arr.ind = T), cluster +
                          2]
    
    BTS_data_cluster = Rawdata[as.character(bts_c)]
    percluster_corr <- Hmisc::rcorr(as.matrix(BTS_data_cluster))
    forecast <- data.frame()
    for (i in 1:ncol(BTS_data_cluster))
    {
      xreg_f = BTS_data_cluster[, which(as.data.frame(percluster_corr [["r"]][i, ]) >=
                                          0.75)][2437:nrow(BTS_data_cluster), ]
      forecast_sarima <-
        smooth::reforecast(Linear_model_clu$model,h = (test_range / 2),newdata = xreg_f,level = 0.95)
      if (i == 1) {
        forecast = data.frame(forecast_sarima$forecast)
      } else {
        forecast <- cbind(forecast, data.frame(forecast_sarima$forecast))
      }
    }
    colnames(forecast)<- as.character(bts_c)
    write.table(forecast, paste0("Forecast_4_BTS in_cluster",cluster,".tsv"))
    forecast
  }


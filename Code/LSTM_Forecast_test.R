LSTM_Forecast <-
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
    percluster_corr <- Hmisc::rcorr(as.matrix(BTS_data_cluster))
    BTS_data_cluster <-
      xts::xts(BTS_data_cluster, order.by = d.time)
    #Every BTS Forecast
    forecasted <-
      foreach (
        i = 1:ncol(BTS_data_cluster),
        bts_c = bts_c,
        BTS_data_cluster = BTS_data_cluster,
        model = model,
        test_range = test_range,
        .combine = 'cbind',
        .packages = c("ggplot2", "zoo", "tidyverse", "tensorflow","keras")
      ) %dopar% {
        Lookback <- test_range
        delay <- test_range
        d = series_to_supervised(as.data.frame(BTS_data_cluster[, i]), Lookback, delay)
        x = d[[1]]
        y = d[[2]]

        # 252 days for testing 
        train_x <- as.matrix(x[1:round(nrow(x) * 0.9),])
        print(paste0("Total number of samples in the original training data = ", nrow(train_x)))
        train_y <- as.matrix(y[1:round(nrow(y) * 0.9),])
        dim(train_x) <- c(dim(train_x), n_features)
        dim(train_y) <- c(dim(train_y), n_features)
        
        test_x <- as.matrix(x[(1 + round(nrow(x) * 0.9)):nrow(x),])
        print(paste0("Total number of samples in the original training data = ", nrow(test_x)))
        test_y <- as.matrix(y[(1 + round(nrow(y) * 0.9)):nrow(y),])
        dim(test_x) <- c(dim(test_x), n_features)
        dim(test_y) <- c(dim(test_y), n_features)
        rm(d, x, y)
        # predict
        pred <- model %>%
          predict(test_x, batch_size = 16) %>%
          .[, 1]
        
        print(data.frame(pred[nrow(test_x),]))
        print(data.frame(test_y[nrow(test_x), , 1]))
        list(
          data.frame(pred[nrow(test_x),]),
          data.frame(test_y[nrow(test_x), , 1])
        )

      }
    forecasted
  }

## ##
## ## Bethelhem S.: Recreating Hybrid prediction
## ## (Version 2.1, built: 2021-11-05)
## ## Copyright (C)2021 Bethelhem SEifu


#################################################################################
#   1.0 Loading required packages and importing functions
#################################################################################
#.rs.restartR()


#=================================================================
# .libPaths( c("~/R/x86_64-pc-linux-gnu-library/3.6", "~/Packages/"))
# setwd("~/Datasets/")
#==================================================================   INSTALL_opts = '--no-lock'
rm(list = ls())
graphics.off()
cat("\014")

# setting the current direc.
setwd("F:/Double-seasonality-prediction--main/")

#load("test03.RData")
packages = c(
  # Core Tidyverse
  # "lubridate",
  "tidyverse",
  # mega package that includes includes "purrr","ggplot2","tplyr",  "dplyr"
  #purrr::map() to iterate sheet reading
  "readxl",
  # parallel computing
  "foreach",
  "doParallel",
  "palmerpenguins",
  "ranger",
  "kableExtra",
  # Data processing and manipulations
  "timeSeries",
  # for handling timeseries datasets
  "xts",
  "Hmisc",
  # for evaluating correlation
  "caret",
  # for data splitting
  "tseries",
  #KPSS test
  # Modeling
  "forecast",
  "smooth",
  "keras",
  "tensorflow",
  "tfruns",
  
  # Visualization
  "gridExtra",
  "dygraphs",
  "plotly",
  "ggseas"  # for plots
)
# if (!requireNamespace(packages, quietly = TRUE)){
#   lapply(packages, install.packages, character.only = TRUE,dependencies=T,
#          )}

lapply(packages, require, character.only = TRUE)
rm(packages)
options(stringsAsFactors = FALSE)
cat("\014")
use_condaenv("r-reticulate")

# functions to be used------------------
source("SARIMA_Forecast.R")
source("model.R")
source("series_to_supervised-fun.R")
################################################################################
# Setting up parallel backend
################################################################################
parallel::detectCores()
n <- parallel::detectCores() - 1

#create a cluster
my.cluster <- parallel::makeCluster(n, type = "PSOCK")
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
# check if it is registered
foreach::getDoParRegistered()
#################################################################################
#   1.1 Import the traffic data                              #
#################################################################################
BTS_longtiude_and_latitude <-
  readxl::read_excel("F:/Main Datasets/Base Stations longtiude and latitude.xlsx")
# Rawdataset
Rawdata <- read_csv("RawData.csv")
Rawdata[,-c(1)] = Rawdata[,-c(1)] / 1024
# #DEC or DBA Clustered datasets
# clusters_dat <-
#   read.table(
#     file = 'DEC_Feb21/BTS_Labeled_DEC_Clustered_UnNormalized_Data_Final_TRAIN.tsv',
#     sep = '\t',
#     header = FALSE,
#     fill = TRUE
#   )
# # clusters_dat[,-c(1,2)]=clusters_dat[,-c(1,2)]/1024
# #x=imputeTS::na_kalman(Rawdata[,-c(1,125,355,387,452,511,523,632,651,696,715)])
# BTS_Names_Cluster <-
#   read.csv("DEC_Feb21/BTS_Names_Cluster.csv")
# cluster_means <-
#   read_csv("DEC_Feb21/Normalized_mean_with_custer_names.csv")
# colnames(cluster_means)[2] = "Cluster0"
# # cluster_means[,-c(1)]=cluster_means[,-c(1)]/1024
# 
#Kmeans Clustered dataset

clusters_dat <-
  read.table(
    file = 'K_means_in_terabytes/BTS_Labeled_Kmeans_Clustered_UnNormalized_Data_Final_TRAIN.tsv',
    sep = '\t',
    header = FALSE,
    fill = TRUE
  )
#x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
BTS_Names_Cluster <-
  read.csv("K_means_in_terabytes/BTS_Names_Cluster.csv")
cluster_means <-
  read_csv("K_means_in_terabytes/Normalized_mean_with_custer_names.csv")
# cluster_means[,-c(1)]=cluster_means[,-c(1)]/1024


# selected RNCs
Selected_RNC <-
  BTS_longtiude_and_latitude[BTS_longtiude_and_latitude$`Site ID` %in% clusters_dat$V1, ]

Selected_RNC$Cluster <-
  clusters_dat[Selected_RNC$`Site ID` %in% clusters_dat$V1, 2]

d.time <-
  as.POSIXct("2019-01-09", tz = "Africa/Addis_Ababa") + seq(0, 9676799, 3600)
cluster_means_xts = xts(cluster_means[,-1], order.by = d.time)
###################################################################################
# temp <-
#   rbind(
#     data.frame(
#       time = d.time[1:168],
#       value = cluster_means$Cluster0[1:168],
#       cluster = 0
#     ),
#     data.frame(
#       time = d.time[1:168],
#       value = cluster_means$Cluster1[1:168],
#       cluster = 1
#     ),
#     data.frame(
#       time = d.time[1:168],
#       value = cluster_means$Cluster2[1:168],
#       cluster = 2
#     ),
#     data.frame(
#       time = d.time[1:168],
#       value = cluster_means$Cluster3[1:168],
#       cluster = 3
#     ),
#     data.frame(
#       time = d.time[1:168],
#       value = cluster_means$Cluster4[1:168],
#       cluster = 4
#     )
#   )
# ### Map
# Filename ="F:/demisse/data/map files"
# # reproject onto WGS84
# 
# shp <-
#   rgdal::readOGR(dsn = Filename, 
#                  layer = "ADDIS_ABEBA_HIGHWAY_polyline")
# 
# shp <- sp::spTransform(shp,sp::CRS("+init=epsg:4326"))
# 
# shp2 <-
#   rgdal::readOGR(dsn = Filename, 
#                  layer = "ADDIS_ABEBA_MAIN_ROAD_polyline")
# shp2 <- sp::spTransform(shp2,sp::CRS("+init=epsg:4326"))
# shp3 <-
#   rgdal::readOGR(dsn = Filename, 
#                  layer = "ADDIS_ABEBA_RAILWAY_polyline")
# shp3 <- sp::spTransform(shp3,sp::CRS("+init=epsg:4326"))
# shp4 <-
#   rgdal::readOGR(dsn = Filename, 
#                  layer = "ADDIS_ABEBA_SECONDARY_ROAD_polyline")
# shp4 <- sp::spTransform(shp4,sp::CRS("+init=epsg:4326"))
# shp5 <-
#   rgdal::readOGR(dsn = Filename, 
#                  layer = "ADDIS_ABEBA_STREET_polyline")
# shp5 <- sp::spTransform(shp5,sp::CRS("+init=epsg:4326"))
# 
# # #x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
# 
# # Visualize it as time series
# 
# p1 <-
#   ggplot() + geom_polygon(
#     data = shp,
#     aes(x = long, y = lat, group = group),
#     colour = "gray",
#     fill = NA
#   ) + theme_void()+geom_polygon(
#     data = shp2,
#     aes(x = long, y = lat, group = group),
#     colour = "black",
#     fill = NA
#   )+geom_polygon(
#     data = shp3,
#     aes(x = long, y = lat, group = group),
#     colour = "gray",
#     fill = NA
#   )+ geom_polygon(
#     data = shp4,
#     aes(x = long, y = lat, group = group),
#     colour = "gray",
#     fill = NA
#   )+ geom_polygon(
#     data = shp5,
#     aes(x = long, y = lat, group = group),
#     colour = "gray",
#     fill = NA
#   )+
#   geom_point(data = Selected_RNC, aes(
#     x = Longitude,
#     y = Latitude,
#     colour = factor(Cluster)
#   )) + theme_minimal()
# 
# 
# p2 <- temp %>% ggplot(aes(time, value, color = factor(cluster))) +
#   geom_line() + theme_bw() + labs(x = "Time", y = "Traffic_load` (TB)")# standard plot
# 
# 
# grid.arrange(
#   (
#     p1 + facet_wrap( ~ Cluster, nrow = 1) + ggtitle("DEC Clustered Base Stations located in Addis Ababa")
#   ),
# 
#   (
#     p2 + facet_wrap( ~ cluster , nrow = 1) + ggtitle("7 days of Clustered Mobile data traffic in Addis Ababa")
#   ),
#   ncol = 1,
#   nrow = 2
# )
# 
# 
# 
# 
# rm(temp,p1,p2)

#################################################################################
#   1.2 Data Processing                             #
#################################################################################
# # 1. Compute correlation matrix -----------------------------------------------
# Correlation <- Hmisc::rcorr(as.matrix(cluster_means[,-1]))
# #visualize a correlation matrix using corrplot
# 
# # Insignificant correlations are leaved blank
# # corrplot::corrplot(
# #   Correlation$r,
# #   type = "upper",
# #   order = "hclust",
# #   p.mat = Correlation$P,
# #   sig.level = 0.01,
# #   insig = "blank"
# # )
# 
# col <-
#   colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot::corrplot(
#   Correlation$r,
#   method = "color",
#   col = col(200),
#   type = "upper",
#   order = "hclust",
#   addCoef.col = "black",
#   # Add coefficient of correlation
#   tl.col = "black",
#   tl.srt = 45,
#   #Text label color and rotation
#   # Combine with significance
#   p.mat = Correlation$P,
#   sig.level = 0.01,
#   insig = "blank",
#   # hide correlation coefficient on the principal diagonal
#   diag = FALSE
# )
#################################################################################
#   1.3 Prediction
#################################################################################

# parameters -----------------------------
test_range = 252 # 3 weeks of data for test
e = (end(cluster_means_xts) - (test_range / 2) * 3600)

###----------------------------------------------
#linear model
##-----------------------------------------------
loss <- function(true, pred)
{
  rmse <- Metrics::rmse(true, pred)
  mae <- Metrics::mae(true, pred)
  list(rmse = rmse, mae = mae)
  
}
Linear_model_clu <-
  foreach (cluster = 0:4,
           .packages = c("ggplot2", "xts", "tidyverse")) %dopar% {
             print(cluster)
             SARIMA_Model(univ_data=cluster_means_xts, 
                          test_range, 
                          c = cluster)
           }

for (cluster in 0:4) {
  print(paste0("Cluster ", cluster))
  
  #Every BTS Forecast
  bts_c <-  BTS_Names_Cluster[which(BTS_Names_Cluster[, cluster + 2] != 0, arr.ind = T), cluster +
                                2]
  
  BTS_data_cluster = Rawdata[as.character(bts_c)]
  forecast_sarima_bts <- SARIMA_Forecast(
    model=Linear_model_clu[[(cluster+1)]][[1]]$model,
    BTS_Names_Cluster = BTS_Names_Cluster,
    Rawdata = Rawdata,
    cluster = cluster,
    test_range=test_range,
    d.time=d.time
  )
  colnames(forecast_sarima_bts) <- as.character(bts_c)
  forecasted<- as.data.frame(forecast_sarima_bts[1,])
  colnames(forecasted) <- as.character(bts_c)
  forecasted_lower<- as.data.frame(forecast_sarima_bts[2,])
  colnames(forecasted_lower) <- as.character(bts_c)
  
  forecasted_upper<- as.data.frame(forecast_sarima_bts[3,])
  colnames(forecasted_upper) <- as.character(bts_c)
  
  forecasted_true<- as.data.frame(forecast_sarima_bts[4,])
  colnames(forecasted_true) <- as.character(bts_c)
  rm(forecast_sarima_bts)
  write.table(forecasted,
              paste0("Forecast_4_BTS in_cluster", cluster, ".tsv"))
  
  openxlsx::write.xlsx(
    list(
      forecasted = forecasted,
      forecasted_lower = forecasted_lower,
      forecasted_upper = forecasted_upper,
      forecasted_true = forecasted_true
    ),
    file = paste0("Forecast_4_BTS in_cluster", cluster, ".xlsx")
  )
  
  
  losses <-
    foreach (j = 1:ncol(BTS_data_cluster), .combine = 'rbind') %dopar% {
      cbind(data.frame( Metrics::rmse(
        forecasted_true[[j]],
        forecasted[[j]]
      )),
      data.frame(Metrics::mae(forecasted_true[[j]],
                              forecasted[[j]])))
      
    }
  
  rm(forecasted,forecasted_lower,forecasted_upper,forecasted_true)
  
  #losses <- cbind(data.frame(rmse = rmse), data.frame(mae = mae))
  #rownames(losses) <- as.character(bts_c)
  write.table(losses, paste0("error_4_BTS in_cluster", cluster, ".tsv"))
  rm(losses)
}
parallel::stopCluster(cl = my.cluster)
# ----------------------------------------------------------------------------------------
#K_means
cluster0_forecasted <-read_excel("results/K_means/Mean_forecast/SARIMA_Forecast_for_Cluster0.xlsx")
cluster1_forecasted <-read_excel("results/K_means/Mean_forecast/SARIMA_Forecast_for_Cluster1.xlsx")
cluster2_forecasted <-read_excel("results/K_means/Mean_forecast/SARIMA_Forecast_for_Cluster2.xlsx")
cluster3_forecasted <-read_excel("results/K_means/Mean_forecast/SARIMA_Forecast_for_Cluster3.xlsx")
cluster4_forecasted <-read_excel("results/K_means/Mean_forecast/SARIMA_Forecast_for_Cluster4.xlsx")
sarima_forecast = data.frame(time = cluster0_forecasted$Time[2437:2688],
                       Cluster0=cluster0_forecasted$forecast_mean[2437:2688],
                       Cluster1=cluster1_forecasted$forecast_mean[2437:2688],
                       Cluster2=cluster2_forecasted$forecast_mean[2437:2688],
                       Cluster3=cluster3_forecasted$forecast_mean[2437:2688],
                       Cluster4=cluster4_forecasted$forecast_mean[2437:2688]
)
Test_dataset = data.frame(time = cluster0_forecasted$Time[2437:2688],
                             Cluster0=cluster0_forecasted$Test_data[2437:2688],
                             Cluster1=cluster1_forecasted$Test_data[2437:2688],
                             Cluster2=cluster2_forecasted$Test_data[2437:2688],
                             Cluster3=cluster3_forecasted$Test_data[2437:2688],
                             Cluster4=cluster4_forecasted$Test_data[2437:2688]
)
residuals = data.frame(time = cluster0_forecasted$Time[1:2436],
                  Cluster0=cluster0_forecasted$residuals[1:2436],
                  Cluster1=cluster1_forecasted$residuals[1:2436],
                  Cluster2=cluster2_forecasted$residuals[1:2436],
                  Cluster3=cluster3_forecasted$residuals[1:2436],
                  Cluster4=cluster4_forecasted$residuals[1:2436]
                  )
residuals_xts = xts(residuals[,-1], order.by = residuals$time)

# bts_forecast
btsforecasted <- list(cluster0=read.csv("results/K_means/Forecast_4_BTS in_cluster0.tsv", sep="",check.names = F),
                            cluster1 = read.csv("results/K_means/Forecast_4_BTS in_cluster1.tsv", sep="",check.names = F),
                            cluster2 = read.csv("results/K_means/Forecast_4_BTS in_cluster2.tsv", sep="",check.names = F),
                            cluster3 =read.csv("results/K_means/Forecast_4_BTS in_cluster3.tsv", sep="",check.names = F),
                            cluster4= read.csv("results/K_means/Forecast_4_BTS in_cluster4.tsv", sep="",check.names = F))

btsforecasted_true <- list(cluster0 = read_excel("results/K_means/Forecast_4_BTS in_cluster0.xlsx", 
                                           sheet = "forecasted_true"),
                           cluster1 =read_excel("results/K_means/Forecast_4_BTS in_cluster1.xlsx", 
                                           sheet = "forecasted_true"),
                           cluster2 =read_excel("results/K_means/Forecast_4_BTS in_cluster2.xlsx", 
                                           sheet = "forecasted_true"),
                           cluster3 = read_excel("results/K_means/Forecast_4_BTS in_cluster3.xlsx", 
                                           sheet = "forecasted_true"),
                           cluster4 = read_excel("results/K_means/Forecast_4_BTS in_cluster4.xlsx", 
                                           sheet = "forecasted_true"))
bts_residuals<- list(
  cluster0_btsforecasted_true - cluster0_btsforecasted,
  cluster1_btsforecasted_true - cluster1_btsforecasted,
  cluster2_btsforecasted_true - cluster2_btsforecasted,
  cluster3_btsforecasted_true - cluster3_btsforecasted,
  cluster4_btsforecasted_true - cluster4_btsforecasted)


# ----------------------------------------------------------------------------------------

# =============================================================================

# ###----------------------------------------------
# #LSTM model
# ##-----------------------------------------------
#set_random_seed(100,disable_gpu = TRUE)
for (cluster in 0:4) {
  Lookback <- 252 ## number of steps (Seq_size),504
  n_features <-
    1 ## number of features. This dataset is univariate so it is 1
  delay <- 252
  batch_size <- 32
  
  d = series_to_supervised(as.data.frame(residuals_xts[, (cluster + 1)]), Lookback, delay)
  x = d[[1]]
  y = d[[2]]
  
  # 252 days for testing
  train_x <- as.matrix(x[1:round(nrow(x) * 0.9), ])
  print(paste0(
    "Total number of samples in the original training data = ",
    nrow(train_x)
  ))
  train_y <- as.matrix(y[1:round(nrow(y) * 0.9), ])
  dim(train_x) <- c(dim(train_x), n_features)
  dim(train_y) <- c(dim(train_y), n_features)
  
  test_x <- as.matrix(x[(1 + round(nrow(x) * 0.9)):nrow(x), ])
  print(paste0(
    "Total number of samples in the original testing data = ",
    nrow(test_x)
  ))
  test_y <- as.matrix(y[(1 + round(nrow(y) * 0.9)):nrow(y), ])
  dim(test_x) <- c(dim(test_x), n_features)
  dim(test_y) <- c(dim(test_y), n_features)
  rm(d, x, y)
  # model
  # input layer
  inputs <- layer_input(shape = c(Lookback, n_features))
  bias_init = keras$initializers$Zeros()
  
  predictions <-
    inputs %>%
    layer_lstm(
      units = 128,
      bias_initializer = bias_init,
      #input_shape  = c(Lookback, n_features),
      return_sequences = T,
    ) %>%
    layer_batch_normalization() %>%
    layer_activation_leaky_relu(alpha = 0.2) %>%
    #layer_repeat_vector(delay) %>%
    # layer_lstm(
    #   units = 64,#bias_initializer = bias_init,
    #   return_sequences = T,activation = "relu",
    #   dropout = 0.3
    # ) %>%
    time_distributed(layer_dense(units = n_features)) %>%
    layer_activation_leaky_relu(alpha = 0.2)
  
  # create and compile model
  Lstm_model <- keras_model(inputs = inputs, outputs = predictions)
  summary(Lstm_model)
  #keras$utils$plot_model(model=Lstm_model, show_shapes=T)
  
  
  # optimizer
  # optimizer_d <- switch(FLAGS$optimizer_type,
  #                       adam = optimizer_adam(learning_rate = FLAGS$lr))
  
  Lstm_model %>%
    compile(loss = "mae",  optimizer = optimizer_adam())
  
  #rm(Lstm_model)
  print("Train.....")
  
  model_fit <- Lstm_model %>%
    fit(
      train_x,
      train_y,
      epochs = 50,
      batch_size = 16,
      validation_split = 0.1,
      shuffle = F
      #validation_data = list(X_valid, y_valid)
      ,
      verbose = 1
      # ,callbacks = list(
      #   callback_early_stopping(
      #     monitor = 'val_loss',
      #     min_delta = 0.2,
      #     patience = 0.3,
      #     verbose = 1,
      #     mode = 'auto'
      #   )
      # )
    )
  Lstm_model %>% save_model_hdf5(paste0("residual_model_for cluster_",cluster,".h5"))
  ## the data traffic
  score <- Lstm_model %>% evaluate(test_x, test_y,
                                   verbose = 0)
  
  print(paste0('Test loss:', score))
  
  #parallel::stopCluster(cl = my.cluster)
  
  
  
  ## the data traffic
  pred <- Lstm_model %>%
    predict(test_x, batch_size = batch_size) %>%
    .[, , 1]
  
  openxlsx::write.xlsx(
    list(forecasted = pred, true = test_y[, , 1]),
    file = paste0("LSTM_residual_Forecast_4_cluster", cluster, ".xlsx")
  )
  hybrid_forecast <- 
  
  df_plot = data.frame(
    time = d.time[2437:2688],
    true_data = Test_dataset[,(cluster+1)],
    Sarima_forcast = sarima_forecast[,(cluster+1)],
    hybrid_forecast  = sarima_forecast[,(cluster+1)]+pred[nrow(test_x), ]
  )
  df_plotxts <- xts::xts(df_plot[, -1], order.by = df_plot[, 1])
  df_plotxts %>%
    dygraph(
      xlab = "Time",
      ylab = "Data Traffic(TB)",
      main = paste0("hybrid model fitting for BTS in cluster ", cluster)
    ) %>%
    dySeries(c("forecast_mean"),
             label = "forecast_mean",
             color = "red") %>%
    dySeries("Test_data", color = "blue")
  ##############################################################333
  BTS_data_cluster<- bts_residuals[[(cluster + 1)]]
  for (i in 1:ncol(BTS_data_cluster))
  {
    d = series_to_supervised(as.data.frame(BTS_data_cluster[, i]), Lookback, delay)
    x = d[[1]]
    y = d[[2]]
    
    # 80/20 train_test
    
    test_x <- as.matrix(x[(1 + round(nrow(x) * 0.9)):nrow(x), ])
    test_y <- as.matrix(y[(1 + round(nrow(y) * 0.9)):nrow(y), ])
    dim(test_x) <- c(dim(test_x), 1)
    dim(test_y) <- c(dim(test_y), 1)
    rm(d, x, y)
    
    pred <- Lstm_model %>%
      predict(test_x, batch_size = batch_size) %>%
      .[, , 1]
    print(data.frame(pred[nrow(test_x), ]))
    print(data.frame(test_y[nrow(test_x), , 1]))
    if (i == 1) {
      forecasted = data.frame(pred[nrow(test_x), ])
      forecasted_true <- data.frame(test_y[nrow(test_x), , 1])
    } else {
      forecasted <- cbind(forecasted, data.frame(pred[nrow(test_x), ]))
      forecasted_true <-
        cbind(forecasted_true, data.frame(test_y[nrow(test_x), , 1]))
    }
  }
  colnames(forecasted) <- as.character(bts_c)
  colnames(forecasted_true) <- as.character(bts_c)
  
  openxlsx::write.xlsx(
    list(forecasted = forecasted, forecasted_t = forecasted_true),
    file = paste0("LSTM_Forecast_4_BTS in_cluster", cluster, ".xlsx")
  )
  
  # forecast_lstm_bts <-
  #   list(forecasted = forecasted, forecasted_t = forecasted_true)
  
  for (j in 1:ncol(BTS_data_cluster)) {
    if (j != 1) {
      rmse <-
        rbind(rmse,
              Metrics::rmse(forecasted_true[[j]],
                            forecasted[[j]]))
      mae <-
        rbind(mae,
              Metrics::mae(forecasted_true[[j]],
                           forecasted[[j]]))
    } else {
      rmse <-
        Metrics::rmse(forecasted_true[[j]],
                      forecasted[[j]])
      mae <-
        Metrics::mae(forecasted_true[[j]],
                     forecasted[[j]])
    }
    
  }
  losses <- cbind(data.frame(rmse = rmse), data.frame(mae = mae))
  rownames(losses) <- as.character(bts_c)
  rm(rmse, mae)
  write.table(losses,
              paste0("LSTM_error_4_BTS in_cluster", cluster, ".tsv"))
  rm(Lstm_model)
}
parallel::stopCluster(cl = my.cluster)

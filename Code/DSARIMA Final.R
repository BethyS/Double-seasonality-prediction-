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
#DEC or DBA Clustered datasets
clusters_dat <-
  read.table(
    file = 'DEC_Results_Encoder_Fixed/BTS_Labeled_DEC_Clustered_UnNormalized_Data_Final_TRAIN.tsv',
    sep = '\t',
    header = FALSE,
    fill = TRUE
  )
# clusters_dat[,-c(1,2)]=clusters_dat[,-c(1,2)]/1024
#x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
BTS_Names_Cluster <-
  read.csv("DEC_Results_Encoder_Fixed/BTS_Names_Cluster.csv")
cluster_means <-
  read_csv("DEC_Results_Encoder_Fixed/Normalized_mean_with_custer_names.csv")
colnames(cluster_means)[2] = "Cluster0"
# cluster_means[,-c(1)]=cluster_means[,-c(1)]/1024

# #Kmeans Clustered dataset
# 
# clusters_dat <-
#   read.table(
#     file = 'K_means_in_terabytes/BTS_Labeled_Kmeans_Clustered_UnNormalized_Data_Final_TRAIN.tsv',
#     sep = '\t',
#     header = FALSE,
#     fill = TRUE
#   )
# #x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
# BTS_Names_Cluster <-
#   read.csv("K_means_in_terabytes/BTS_Names_Cluster.csv")
# cluster_means <-
#   read_csv("K_means_in_terabytes/Normalized_mean_with_custer_names.csv")
# # cluster_means[,-c(1)]=cluster_means[,-c(1)]/1024


# selected RNCs
Selected_RNC <-
  BTS_longtiude_and_latitude[BTS_longtiude_and_latitude$`Site ID` %in% clusters_dat$V1, ]

Selected_RNC$Cluster <-
  clusters_dat[Selected_RNC$`Site ID` %in% clusters_dat$V1, 2]


###################################################################################

d.time <-
  as.POSIXct("2019-01-09", tz = "Africa/Addis_Ababa") + seq(0, 9676799, 3600)
cluster_means_xts = xts(cluster_means[,-1], order.by = d.time)

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
# 
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
#     p1 + facet_wrap( ~ Cluster, nrow = 1) + ggtitle("DEC-FE Clustered Base Stations located in Addis Ababa")
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
# 1. Compute correlation matrix -----------------------------------------------
Correlation <- Hmisc::rcorr(as.matrix(cluster_means[,-1]))
#visualize a correlation matrix using corrplot

# Insignificant correlations are leaved blank
# corrplot::corrplot(
#   Correlation$r,
#   type = "upper",
#   order = "hclust",
#   p.mat = Correlation$P,
#   sig.level = 0.01,
#   insig = "blank"
# )

col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(
  Correlation$r,
  method = "color",
  col = col(200),
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 45,
  #Text label color and rotation
  # Combine with significance
  p.mat = Correlation$P,
  sig.level = 0.01,
  insig = "blank",
  # hide correlation coefficient on the principal diagonal
  diag = FALSE
)
#################################################################################
#   1.3 Prediction
#################################################################################

# parameters -----------------------------
test_range = 252 # 3 weeks of data for test
e = (end(cluster_means_xts) - (test_range / 2) * 3600)

###----------------------------------------------
#linear model
##-----------------------------------------------
cluster = 1 # the prediction done for cluster 0
loss <- function(true, pred)
{
  rmse <- Metrics::rmse(true, pred)
  mae <- Metrics::mae(true, pred)
  list(rmse = rmse, mae = mae)

}
Linear_model_clu <-
  foreach (cluster = 0:4,
           .packages = c("ggplot2", "xts", "tidyverse")) %dopar% {
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
# ###----------------------------------------------
# #LSTM model
# ##-----------------------------------------------
# Lookback <- test_range
# delay <- 1
# batch_size <- 32
# 
# d = series_to_supervised(as.data.frame(cluster_means_xts[, cluster]), Lookback, 1)
# x = d[[1]]
# y = d[[2]]
# 
# # 80/20 train_test
# train_x <- as.matrix(x[1:round(nrow(x) * 0.8),])
# train_y <- as.matrix(y[1:round(nrow(y) * 0.8),])
# dim(train_x) <- c(dim(train_x), 1)
# dim(train_y) <- c(dim(train_y), 1)
# 
# test_x <- as.matrix(x[(1 + round(nrow(x) * 0.8)):nrow(x),])
# test_y <- as.matrix(y[(1 + round(nrow(y) * 0.8)):nrow(y),])
# dim(test_x) <- c(dim(test_x), 1)
# dim(test_y) <- c(dim(test_y), 1)
# rm(d, x, y)
# # model
# 
# ##
# 
# # Define the checkpoint directory to store the checkpoints
# checkpoint_dir <- './training_checkpoints'
# # Name of the checkpoint files
# checkpoint_prefix <- file.path(checkpoint_dir, "ckpt_{epoch}")
# 
# decay <- function(epoch, lr) {
#   if (epoch < 3) 1e-3
#   else if (epoch >= 3 && epoch < 7) 1e-4
#   else 1e-5
# }
# 
# 
# FLAGS <- flags(
#   flag_integer("batch_size", batch_size),
#   flag_integer("n_epochs", 50),
#   # fraction of the units to drop for the linear transformation of the inputs
#   #flag_numeric("dropout", 0.2),
#   flag_string("loss", "mae"),
#   flag_string("optimizer_type", "adam")
#   # # size of the LSTM layer
#   # flag_integer("n_units", 128),
#   # # learning rate
#   # flag_numeric("lr", 0.002),
#   # # momentum, an additional parameter to the SGD optimizer
#   # flag_numeric("momentum", 0.9),
#   # # parameter to the early stopping callback
#   # flag_integer("patience", 10)
# )
# 
# 
# # input layer
# inputs <- layer_input(shape = c(Lookback, 1))
# 
# predictions <-
#   inputs %>% 
#   layer_lstm(
#     units = 256,
#     #input_shape  = list(Lookback, 1),
#     dropout = 0.2,
#     return_sequences = T,activation = "relu",
#     stateful = F
#   ) %>%
#   layer_lstm(
#     units = 128,
#     return_sequences = T,activation = "relu",
#     stateful = F
#   ) %>% 
#   layer_lstm(
#     units = 64,
#     dropout = 0.2, activation = "relu",
#     return_sequences = T,
#     stateful = F
#   ) %>%
#   layer_lstm(
#     units = 1,
#     activation = "relu",
#     return_sequences = F,
#     stateful = F
#   )
# 
# # create and compile model
# model <- keras_model(inputs = inputs, outputs = predictions)
# summary(model)
# 
# 
# # optimizer
# optimizer_d <- switch(FLAGS$optimizer_type,
#                       adam = optimizer_adam(learning_rate = FLAGS$lr))
# 
# model %>%
#   compile(loss = FLAGS$loss,  optimizer = optimizer_adam())
# 
# model.fit<- model %>%
#   fit(
#     train_x,
#     train_y,
#     epochs = FLAGS$n_epochs,
#     batch_size = FLAGS$batch_size,
#     validation_split = 0.2
#     #validation_data = list(X_valid, y_valid)
#     ,
#     verbose = 1
#     # ,callbacks = list(
#     #  callback_early_stopping(
#     #    monitor = 'val_loss',
#     #    min_delta = 0.4,
#     #    patience = 0.3,
#     #     verbose = 0,
#     #     mode = 'auto'
#     # )
#     # )
#   )
# 
# ## the data traffic
# pred <- model %>%
#   predict(test_x, batch_size = batch_size) %>%
#   .[, 1]
# 
# score <- model %>% evaluate(
#   test_x, test_y,
#   verbose = 0
# )
# 
# cat('Test loss:', score$loss, '\n')
# cat('Test accuracy:', score$acc, '\n')
# 
# df_plot = data.frame(
#   time = d.time[2202:2688],
#   Test_data = c(test_y[, , 1]),
#   forecast_mean = pred
# )
# df_plotxts <- xts::xts(df_plot[,-1], order.by = d.time[2202:2688])
# df_plotxts %>%
#   dygraph(
#     xlab = "Time",
#     ylab = "Data Traffic(TB)",
#     main = paste0("LSTM model fitting for BTS in cluster ", cluster)
#   ) %>%
#   dySeries(c("forecast_mean"),
#            label = "forecast_mean",
#            color = "red") %>%
#   dySeries("Test_data", color = "blue")
# ##############################################################333
# forecasted <- data.frame()
# forecasted_true <- data.frame()
# BTS_data_cluster = Rawdata[as.character(bts_c)]
# percluster_corr <- Hmisc::rcorr(as.matrix(BTS_data_cluster))
# BTS_data_cluster <- xts::xts(BTS_data_cluster, order.by = d.time)
# for (i in 1:ncol(BTS_data_cluster))
# {
#   d = series_to_supervised(as.data.frame(BTS_data_cluster[, i]), Lookback, 1)
#   x = d[[1]]
#   y = d[[2]]
#   
#   # 80/20 train_test
#   
#   test_x <- as.matrix(x[(1 + round(nrow(x) * 0.8)):nrow(x),])
#   test_y <- as.matrix(y[(1 + round(nrow(y) * 0.8)):nrow(y),])
#   dim(test_x) <- c(dim(test_x), 1)
#   dim(test_y) <- c(dim(test_y), 1)
#   rm(d, x, y)
#   
#   pred <- model %>%
#     predict(test_x, batch_size = batch_size) %>%
#     .[, 1]
#   print(data.frame(pred))
#   print(data.frame(test_y[, , 1]))
#   if (i == 1) {
#     forecasted = data.frame(pred)
#     forecasted_true <- data.frame(test_y[, , 1])
#   } else {
#     forecasted <- cbind(forecasted, data.frame(pred))
#     forecasted_true <-
#       cbind(forecasted_true, data.frame(test_y[, , 1]))
#   }
# }
# colnames(forecasted) <- as.character(bts_c)
# colnames(forecasted_true) <- as.character(bts_c)
# write.table(forecasted,
#             paste0("LSTM_Forecast_4_BTS in_cluster", cluster, ".tsv"))
# 
# openxlsx::write.xlsx(
#   list(forecasted = forecasted, forecasted_t = forecasted_true),
#   file = paste0("LSTM_Forecast_4_BTS in_cluster", cluster, ".xlsx")
# )
# 
# forecast_lstm_bts <-
#   list(forecasted = forecasted, forecasted_t = forecasted_true)
# 
# for (j in 1:ncol(BTS_data_cluster)) {
#   if (j != 1) {
#     rmse <-
#       rbind(
#         rmse,
#         Metrics::rmse(
#           forecast_lstm_bts$forecasted_t[[j]],
#           forecast_lstm_bts$forecasted[[j]]
#         )
#       )
#     mae <-
#       rbind(
#         mae,
#         Metrics::mae(
#           forecast_lstm_bts$forecasted_t[[j]],
#           forecast_lstm_bts$forecasted[[j]]
#         )
#       )
#   } else {
#     rmse <-
#       Metrics::rmse(forecast_lstm_bts$forecasted_t[[j]],
#                     forecast_lstm_bts$forecasted[[j]])
#     mae <-
#       Metrics::mae(forecast_lstm_bts$forecasted_t[[j]],
#                    forecast_lstm_bts$forecasted[[j]])
#   }
# 
# }
#   losses <- cbind(data.frame(rmse = rmse), data.frame(mae = mae))
#   rownames(losses) <- as.character(bts_c)
#   rm(rmse, mae)
#   write.table(losses, paste0("LSTM_error_4_BTS in_cluster", cluster, ".tsv"))
#   
#   parallel::stopCluster(cl = my.cluster)

## ##
## ## Bethelhem S.: Recreating Hybrid prediction
## ## (Version 2.1, built: 2021-12-30)
## ## Copyright (C)2021 Bethelhem SEifu


#################################################################################
#   1.0 Loading required packages and importing fuctions
#################################################################################
#.rs.restartR()
# setting the current direc.
setwd("F:/Double-seasonality-prediction--main/")
use_condaenv("r-reticulate",required=T)
#=================================================================
# .libPaths( c("~/R/x86_64-pc-linux-gnu-library/3.6","~/Packages/"))
# setwd("~/Double-seasonality-prediction--main/")
# install.packages("httr",character.only = TRUE,dependencies=TRUE,  respo= "https://CRAN.R-project.org/")
#==================================================================
rm(list = ls())
graphics.off()
cat("\014")

packages = c(
  # Core Tidyverse
  # "lubridate",
  "dplyr",
  # mega package that includes includes "purrr","ggplot2","tplyr",  "dplyr"
  #purrr::map() to iterate sheet reading
  "readxl","readr",
  # parallel computing
  "foreach",
  "doParallel",
  "palmerpenguins",
  "ranger",
  # Data processing and manipulations
  "timeSeries",
  # for handling timeseries datasets
  "xts",
  # for data splitting
  "tseries",
  #KPSS test
  # Modeling
  "keras",
  "tensorflow",
  "tfruns")
# if (!requireNamespace(packages, quietly = TRUE)){
#   lapply(packages, install.packages, character.only = TRUE,dependencies=T,
#          )}

lapply(packages, require, character.only = TRUE)
rm(packages)
options(stringsAsFactors = FALSE)
cat("\014")


# functions to be used------------------
source("series_to_supervised-fun.R")
################################################################################
# Setting up parallel back-end
################################################################################
# parallel::detectCores()
# n <- parallel::detectCores() - 1
# 
# #create a cluster
# my.cluster <- parallel::makeCluster(n, type = "PSOCK")
# print(my.cluster)
# 
# #register it to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)
# # parallel::clusterEvalQ(cl = my.cluster,
# # .libPaths(c("~/R/x86_64-pc-linux-gnu-library/3.6","~/Packages/")))
# # check if it is registered
# foreach::getDoParRegistered()
#-------------------------------------------------------
#################################################################################
#   1.1 Import the traffic data                              #
#################################################################################
BTS_longtiude_and_latitude <-
  readxl::read_excel("F:/Main Datasets/Base Stations longtiude and latitude.xlsx")
# Rawdataset
Rawdata <- read_csv("RawData.csv")
Rawdata[,-c(1)] = Rawdata[,-c(1)] / 1024
#DEC Clustered datasets
clusters_dat <-
  read.table(
    file = 'DBA Results/BTS_Full_clusterd_data_Withname_Train.tsv',
    sep = '\t',
    header = FALSE,
    fill = TRUE
  )
# clusters_dat[,-c(1,2)]=clusters_dat[,-c(1,2)]/1024
#x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
BTS_Names_Cluster <-
  read.csv("DBA Results/BTS_Names_Cluster.csv")
cluster_means <-
  read_csv("DBA Results/Normalized_mean_with_custer_names.csv")
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
#############################################################################
# Data Pre-processing 
#############################################################################

#set_random_seed(100,disable_gpu = TRUE)
cluster = 0
Lookback <- 504 ## number of steps (Seq_size)
n_features <- 1 ## number of features. This dataset is univariate so it is 1
delay <- 168
batch_size <- 32

d = series_to_supervised(as.data.frame(cluster_means_xts[, (cluster+1)]), Lookback, delay)
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
# model

FLAGS <- flags(
  flag_integer("batch_size", batch_size),
  flag_integer("n_epochs", 50),
  
  # fraction of the units to drop for the linear transformation of the inputs
  #flag_numeric("dropout", 0.2),
  # flag_string("loss", "mae"),
  flag_string("optimizer_type", "adam"),
  # # size of the LSTM layer
  # flag_integer("n_units", 128),
  # # learning rate
  flag_numeric("learning_rate", 0.002)
  # # momentum, an additional parameter to the SGD optimizer
  # flag_numeric("momentum", 0.9),
  # # parameter to the early stopping callback
  # flag_integer("patience", 10)
)


# input layer
inputs <- layer_input(shape = c(Lookback, n_features))
bias_init = keras$initializers$Zeros()


predictions <-
  inputs %>% 
  layer_lstm(
    units = 256,bias_initializer = bias_init,
    #input_shape  = c(Lookback, n_features),
    activation = "relu"
  ) %>%
  layer_repeat_vector(delay) %>% 
  layer_lstm(
    units = 128,bias_initializer = bias_init,
    return_sequences = T,activation = "relu",
    dropout = 0.3
  ) %>% 
  time_distributed(layer_dense(units = n_features,activation = "relu"))

  

# create and compile model
Lstm_model <- keras_model(inputs = inputs, outputs = predictions)
summary(Lstm_model)
#keras$utils$plot_model(model=Lstm_model, show_shapes=T)


# optimizer
optimizer_d <- switch(FLAGS$optimizer_type,
                      adam = optimizer_adam(learning_rate = FLAGS$lr))

Lstm_model %>%
  compile(loss = "mse",  optimizer = optimizer_adam())


print("Train.....")

model_fit<- Lstm_model %>%
  fit(
    train_x,
    train_y,
    epochs = 10,
    batch_size = 8,
    validation_split = 0.2
    #validation_data = list(X_valid, y_valid)
    ,
    verbose = 1
    ,callbacks = list(
     callback_early_stopping(
       monitor = 'val_loss',
       min_delta = 0.2,
       patience = 0.3,
        verbose = 0,
        mode = 'auto'
    )
    )
  )

## the data traffic
score <- Lstm_model %>% evaluate(
  test_x, test_y,
  verbose = 0
)

print(paste0('Test loss:', score$loss))

#parallel::stopCluster(cl = my.cluster)


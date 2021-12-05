## ##
## ## Bethelhem S.: Recreating Hybrid prediction
## ## (Version 2.1, built: 2021-11-05)
## ## Copyright (C)2021 Bethelhem SEifu


#################################################################################
#   1.0 Loading required packages and importng fuctions
#################################################################################
#.rs.restartR()
rm(list = ls())
graphics.off()
cat("\014")
packages = c(
  # Core Tidyverse
  # "lubridate",
  "tidyverse",# mega package that includes includes "purrr","ggplot2","tplyr",  "dplyr"
  #purrr::map() to iterate sheet reading
  "readxl","raster",
  # Data processing and manipulations
  "timeSeries", # for handling timeseries datasets
  "xts",
  "Hmisc", # for evaluating correlation 
  "caret", # for data splitting
  "tseries", #KPSS test
  # Modeling
  "forecast","smooth",
  #"keras",
  #"tensorflow",
  #"tfruns",
  
  # Visualization
  "gridExtra","dygraphs","plotly",
  "ggseas"  # for plots
)

#lapply(packages, install.packages, character.only = TRUE,dependencies=T)
lapply(packages, require, character.only = TRUE)
rm(packages)
options(stringsAsFactors = FALSE)
cat("\014")
# setting the current direc.
setwd("F:/Double-seasonality-prediction--main/")

# functions to be used------------------
source("SARIMA_Forecast.R")
source("model.R")
#################################################################################
#   1.1 Import the traffic data                              #
#################################################################################
BTS_longtiude_and_latitude <-
  readxl::read_excel("F:/Main Datasets/Base Stations longtiude and latitude.xlsx")
# Rawdataset
Rawdata <- read_csv("RawData.csv")
Rawdata[,-c(1)]=Rawdata[,-c(1)]/1024
#DEC Clustered datasets
clusters_dat <- read.table(file='DEC_Results_Unormalized/BTS_Labeled_DEC_Clustered_UnNormalized_Data_Final_TRAIN.tsv', sep = '\t', header = FALSE, fill = TRUE)
clusters_dat[,-c(1,2)]=clusters_dat[,-c(1,2)]/1024
#x=imputeTS::na_kalman(clusters_dat[,-c(1,2)])
BTS_Names_Cluster<- read.csv("DEC_Results_Unormalized/BTS_Names_Cluster.csv")
cluster_means <- read_csv("DEC_Results_Unormalized/Normalized_mean_with_custer_names.csv")
cluster_means[,-c(1)]=cluster_means[,-c(1)]/1024

# selected RNCs
Selected_RNC <-BTS_longtiude_and_latitude[BTS_longtiude_and_latitude$`Site ID` %in% clusters_dat$V1, ]

Selected_RNC$Cluster <- clusters_dat[Selected_RNC$`Site ID` %in% clusters_dat$V1,2]


###################################################################################

d.time <- as.POSIXct("2019-01-09",tz ="Africa/Addis_Ababa") + seq(0,9676799,3600)
cluster_means_xts = xts(cluster_means[,-1], order.by = d.time)

temp <- rbind(data.frame(time = d.time[1:168], value = cluster_means$Cluster0[1:168],cluster = 0),
              data.frame(time = d.time[1:168], value = cluster_means$Cluster1[1:168],cluster = 1),
              data.frame(time = d.time[1:168], value = cluster_means$Cluster2[1:168],cluster = 2),
              data.frame(time = d.time[1:168], value = cluster_means$Cluster3[1:168],cluster = 3),
              data.frame(time = d.time[1:168], value = cluster_means$Cluster4[1:168],cluster = 4))


# Visualize it as time series


p1 <- ggplot(data = Selected_RNC, aes(x = Longitude, y = Latitude,colour = factor(Cluster))) +
  geom_point() + theme_bw()+
  ggtitle("Base Stations located in Addis Ababa")

p2 <- temp %>% ggplot(aes(time,value,color = factor(cluster))) + 
           geom_line()+theme_bw() + labs( x = "Time", y = "Traffic_load` (TB)")# standard plot         
           
grid.arrange(
(p1 + facet_wrap( ~ Cluster, nrow = 1) + ggtitle("Clustered Base Stations located in Addis Ababa")),

(p2 + facet_wrap( ~ cluster , nrow = 1) + ggtitle("7 days of Clustered Mobile data traffic in Addis Ababa")),
ncol =1, nrow = 2)


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

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(Correlation$r, method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col="black", tl.srt=45, #Text label color and rotation
                   # Combine with significance
                   p.mat = Correlation$P, sig.level = 0.01, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE 
)
#################################################################################
#   1.3 Prediction                            
#################################################################################

# parameters -----------------------------
test_range = 504 # 3 weeks of data for test 
e =(end(cluster_means_xts)-(test_range/2)*3600)

###--------------------------------------------------
#linear model
##------------------------------------------------
cluster = 0 # the prediction done for cluster o

Linear_model_clu = SARIMA_Model(cluster_means_xts,test_range,c=cluster)
print("Model:");Linear_model_clu$model$model
Linear_model_clu$decom_plot+theme_bw() 
Linear_model_clu$sarima_plot

#Every BTS Forcast 
bts_c <- BTS_Names_Cluster[which(BTS_Names_Cluster[,cluster+2]!=0,arr.ind = T),cluster+2]
as.character(bts_c)
BTS_data_cluster= Rawdata[as.character(bts_c)]
loss <- function(da)
{
  
  print(da)
  rmse <- Metrics::rmse(da,forecast[,1])
  mae <- Metrics::mae(da,forecast[,1])
  list(rmse=rmse, mae=mae)   
  
}
losses <- mapply(loss,(BTS_data_cluster[2437:nrow(BTS_data_cluster),]))
write.table(forecast, paste0("error_4_BTS in_cluster",cluster,".tsv"))

## --------------------------------------
## Ensemble prediction: Model building
## --------------------------------------


## Input: Data for modelling, as R data frames
## Output 1: The fitted models, as list objects
## Output 2: The weights of the models for ensemble prediction

## In the future, may also need to add historical models
## And this may be a user input?
Historical = FALSE


## Load the R packages
library(lubridate)
library(stringr)
library(mgcv)


## Load the input data 
load("Modelling_data.RData")


## ---------------------
## (I) Fitting models
## ---------------------

## Functions for fitting models
## In the following, data_y are sap flux data, data_x are weather data

## Model 1: the temperature driven model
gam_temp = function(data_y, data_x) {
  ntree = ncol(data_y) - 1
  
  gam_avg_temp = lapply(1:ntree, FUN=function(i) {
    data_all = data.frame(data_y[-1, c(1, i+1)], data_x[-1, -1])
    names(data_all) = c("time", "sf", "airtemp", "humidity", "vpd", "solar", 
                        "meanTemp", "maxTemp", "meanHumid", "DoY")
    data_all$sf1 = data_y[-nrow(data_y), i+1]
    
    gam_fit = gam(sf ~ s(vpd, by=solar, bs="bs", k=15) + 
                    s(solar, bs="bs", k=10) + 
                    s(vpd, by=maxTemp, bs="bs", k=10) + 
                    humidity + sf1,
                  data=data_all, method="REML", select=TRUE)
    return(gam_fit)
  })
  
  return(gam_avg_temp)
}


## Model 2: the humidity driven model
gam_humid = function(data_y, data_x) {
  ntree = ncol(data_y) - 1
  
  gam_avg_humid = lapply(1:ntree, FUN=function(i) {
    data_all = data.frame(data_y[-1, c(1, i+1)], data_x[-1, -1])
    names(data_all) = c("time", "sf", "airtemp", "humidity", "vpd", "solar", 
                        "meanTemp", "maxTemp", "meanHumid", "DoY")
    data_all$sf1 = data_y[-nrow(data_y), i+1]
    
    gam_fit = gam(sf ~ s(vpd, by=solar, bs="bs", k=15) + 
                    s(solar, bs="bs", k=10) + 
                    s(vpd, by=meanHumid, bs="bs", k=10) + 
                    airtemp + sf1,
                  data=data_all, method="REML", select=TRUE)
    return(gam_fit)
  })
  
  return(gam_avg_humid)
}


## Model 3: the seasonal driven model (for future impelmentation)
## This model uses historical data, must completes a full growing season
gam_doy = function(data_y, data_x) {
  ntree = ncol(data_y) - 1

  gam_avg_doy = lapply(1:ntree, FUN=function(i) {
    data_all = data.frame(data_y[-1, c(1, i+1)], data_x[-1, -1])
    names(data_all) = c("time", "sf", "airtemp", "humidity", "vpd", "solar",
                        "meanTemp", "maxTemp", "meanHumid", "DoY")
    data_all$sf1 = data_y[-nrow(data_y), i+1]

    gam_fit = gam(sf ~ s(vpd, by=solar, bs="bs", k=15) +
                    s(solar, bs="bs", k=10) +
                    s(vpd, by=DoY, bs="bs", k=10) +
                    airtemp + humidity + sf1,
                  data=data_all, method="REML", select=TRUE)
    return(gam_fit)
  })

  return(gam_avg_doy)
}


## Fitting GAMs using current year data
gam_list_airtemp <- gam_temp(data_y=Sapflux_modelling, data_x=Weather_modelling)
names(gam_list_airtemp) <- paste("gam_airtemp", 1:length(gam_list_airtemp), sep="_")

gam_list_humidity <- gam_humid(data_y=Sapflux_modelling, data_x=Weather_modelling)
names(gam_list_humidity) <- paste("gam_humidity", 1:length(gam_list_humidity), sep="_")


## Fitting GAMs using historical data
if (Historical) {
  load("Data/Historical_data.RData")
  gam_list_doy <- gam_doy(data_y=Sapflux_modelling, data_x=Weather_modelling)
  names(gam_list_doy) <- paste("gam_doy", 1:length(gam_list_doy), sep="_")
} else {
  gam_list_doy <- NULL
}


## Merge the lists and save the result
gam_list <- c(gam_list_airtemp, gam_list_humidity, gam_list_doy)
save(gam_list, file="Fitted_models.RData")



## ---------------------------------
## (II) Training ensemble weights
## ---------------------------------

## Training the weights of the models
## Use last two-weeks data to train the weights
nt = nrow(Sapflux_modelling)
Sapflux_training <- Sapflux_modelling[nt:(nt-24*14), ]
Weather_training <- Weather_modelling[nt:(nt-24*14), ]


## A function for doing the training
ensemble_train = function(model_list, train_y, train_x) { 
  
  ## calculate the prediction on training data
  ntree = ncol(train_y) - 1
  
  train_list = lapply(1:ntree, FUN=function(i) {
    train_all = data.frame(train_y[-1, c(1, i+1)], train_x[-1, -1])
    names(train_all) = c("time", "sf", "airtemp", "humidity", "vpd", "solar",
                         "meanTemp", "maxTemp", "meanHumid", "DoY")
    train_all$sf1 = train_y[-nrow(train_y), i+1]
    
    train_mod = sapply(1:length(model_list), 
                       FUN=function(m) { predict(model_list[[m]], newdata=train_all, se.fit=FALSE) } )
    return(train_mod)
  })
  
  ## ensemble weights
  y_long <- unlist(train_y[-1, 1+(1:ntree)])
  fit_long <- numeric(0)
  for (i in 1:ntree) {
    fit_long <- rbind(fit_long, train_list[[i]])
  }
  res_long <- fit_long - y_long
  
  rev_mse <- 1 / sqrt(apply(res_long^2, MARGIN=2, FUN=mean, na.rm=TRUE))
  mse_wt <- rev_mse / sum(rev_mse)
  equ_wt <- 1 / length(model_list)
  weight <- list(equ=equ_wt, mse=mse_wt)
  
  ## ensemble spread and errors
  equ_pred <- apply(fit_long, MARGIN=1, FUN=mean, na.rm=TRUE)
  mse_pred <- apply(sapply(1:length(mse_wt), 
                           FUN=function(x) { fit_long[, x] * mse_wt[x] } ),
                    MARGIN=1, FUN=sum)
  
  # spread
  equ_dif <- fit_long - equ_pred
  spread_equ <- sqrt(apply(equ_dif^2, MARGIN=1, FUN=mean, na.rm=TRUE))
  mse_dif <- fit_long - mse_pred
  spread_mse <- sqrt(apply(sapply(1:length(mse_wt), 
                                  FUN=function(x) { mse_dif[, x]^2 * mse_wt[x] } ),
                           MARGIN=1, FUN=sum))
  
  # errors
  err_equ <- abs(y_long - equ_pred)
  err_mse <- abs(y_long - mse_pred)
  err_equ_scale <- sqrt(sum(err_equ^2, na.rm=TRUE) / sum(spread_equ^2, na.rm=TRUE))
  err_mse_scale <- sqrt(sum(err_mse^2, na.rm=TRUE) / sum(spread_mse^2, na.rm=TRUE))
  sfactor <- list(equ=err_equ_scale, mse=err_mse_scale)
  
  ## Return the result as a list
  return(list(weight=weight, sfactor=sfactor))
}


## Train the ensemble weights
gam_weight <- ensemble_train(model_list=gam_list, train_y=Sapflux_training,
                             train_x=Weather_training)
save(gam_weight, file="Weights.RData")



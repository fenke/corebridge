## ------------------------------------------
## Ensemble prediction: Predicting sap flux 
## ------------------------------------------


## Input 1: The fitted models and their weights as R list objects
## Input 2: Weather prediction data, as R data frames
## Input 3: The historical data (used in model fitting), as R data frames
## Output: The predicted sap flux density of a standard tree


## Load the R packages
library(lubridate)
library(stringr)
library(mgcv)


## Load input data
load("Fitted_models.RData")
load("Weights.RData")
load("Prediction_data.RData")
load("Modelling_data.RData")


## Functions for predicting sap flux density and water usage
ensemble_predict = function(model_list, pred_x, ini_y, weight, sfactor) {
  ## First create the prediction function
  forward_pred <- function(gam_mod, dat_pred, lag1) {
    # t=1
    dat_temp <- dat_pred[1, ]
    dat_temp$sf1 <- lag1
    sv_hat <- predict(gam_mod, newdata=dat_temp, se.fit=FALSE)
    pred_gam <- sv_hat
    
    # t=2, 3, ...
    ny = nrow(dat_pred)
    for(j in 2:ny) {
      dat_temp <- dat_pred[j, ]
      dat_temp$sf1 <- sv_hat
      sv_hat <- predict(gam_mod, newdata=dat_temp, se.fit=FALSE)
      pred_gam <- c(pred_gam, sv_hat)
    }
    # return the prediction 
    return(pred_gam)
  }
  
  ## Prediction using different models and data
  names(pred_x) = c("time", "airtemp", "humidity", "vpd", "solar",
                    "meanTemp", "maxTemp", "meanHumid", "DoY")
  ntree = length(ini_y)
  
  pred_avg_all <- list()
  ensemble_avg_all <- error_avg_all <- list()
  for (i in 1:ntree) { 
    ## prediction using different models
    sf1 <- as.numeric(ini_y[i])
    gam_avg_pred = sapply(1:length(model_list), FUN=function(m) {
      forward_pred(model_list[[m]], dat_pred=pred_x, lag1=sf1)
    })
    pred_avg_all[[i]] <- gam_avg_pred
    
    ## Ensemble spread and prediction uncertainty
    equ_wt <- weight$equ
    mse_wt <- weight$mse
    
    # equal weights
    pred_equ <- apply(gam_avg_pred, MARGIN=1, FUN=mean, na.rm=TRUE)
    dif_equ <- gam_avg_pred - pred_equ
    spread_equ <- sqrt(apply(dif_equ^2, MARGIN=1, FUN=mean, na.rm=TRUE))
    
    # using mse weight
    pred_mse <- apply(sapply(1:length(mse_wt), 
                             FUN=function(x) { gam_avg_pred[, x] * mse_wt[x] } ),
                      MARGIN=1, FUN=sum, na.rm=TRUE)
    dif_mse <- gam_avg_pred - pred_mse
    spread_mse <- sqrt(apply(sapply(1:length(mse_wt), 
                                    FUN=function(x) { dif_mse[, x]^2 * mse_wt[x] } ),
                             MARGIN=1, FUN=sum))
    
    error_equ <- spread_equ * sfactor$equ
    error_mse <- spread_mse * sfactor$mse
    
    ensemble_avg_all[[i]] <- list(equ=pred_equ, mse=pred_mse)
    error_avg_all[[i]] <- list(equ=error_equ, mse=error_mse) 
  }
  
  ## Predict a typical tree
  # equal weights
  equ_pred_all <- sapply(1:ntree, FUN=function(x) { ensemble_avg_all[[x]]$equ } )
  equ_typical <- apply(equ_pred_all, MARGIN=1, FUN=mean, na.rm=TRUE)
  
  equ_err_all <- sapply(1:ntree, FUN=function(x) { error_avg_all[[x]]$equ } )
  equ_typical_se <- sqrt(apply(equ_err_all^2, MARGIN=1, FUN=sum, na.rm=TRUE)) / ncol(equ_err_all)
  
  # mse weights
  mse_pred_all <- sapply(1:ntree, FUN=function(x) { ensemble_avg_all[[x]]$mse } )
  mse_typical <- apply(mse_pred_all, MARGIN=1, FUN=mean, na.rm=TRUE)
  
  mse_err_all <- sapply(1:ntree, FUN=function(x) { error_avg_all[[x]]$mse } )
  mse_typical_se <- sqrt(apply(mse_err_all^2, MARGIN=1, FUN=sum, na.rm=TRUE)) / ncol(mse_err_all)
  
  typical_equ <- list(pred=equ_typical, se=equ_typical_se)
  typical_mse <- list(pred=mse_typical, se=mse_typical_se)
  typical_tree <- list(equ=typical_equ, mse=typical_mse)
  
  ## return the result as a list
  return(list(ensemble=ensemble_avg_all, error=error_avg_all, 
              typical=typical_tree, time=pred_x$time))
}



## Prediction
nt = nrow(Sapflux_modelling)
yt = Sapflux_modelling[nt, -1]
ensemble_pred <- ensemble_predict(model_list=gam_list, pred_x=Weather_pred, 
                                  ini_y=yt, weight=gam_weight$weight, 
                                  sfactor=gam_weight$sfactor)

save(ensemble_pred, file="Predicted_sapflux.RData")





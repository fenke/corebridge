## ----------------------------------------------
## Ensemble prediction: Calculating water usage
## ----------------------------------------------


## Input 1: The predicted sap flux density of a standard tree
## Input 2: Total sapwood area (user input?)
## Output: The predicted daily water usage

## Load the R packages
library(lubridate)
library(stringr)


## Load input data
load("Predicted_sapflux.RData")


## Also need the user input data: total sapwood area
total_sapwood = pi*3.5^2


## A function for calcuation water usage with uncertainty
water_usage_predict = function(ensemble, error, timestamp, size) {
  # amplitude for reverting back the normalised time series
  # size is total size for scaling up the water usage
  
  ntree <- length(ensemble)
  temp_day <- ymd(substr(timestamp, start=1, stop=10), tz="UTC")
  AS <- size
  
  # equal weights
  equ_pred_all <- sapply(1:ntree, FUN=function(x) { ensemble[[x]]$equ } )
  equ_typical <- apply(equ_pred_all, MARGIN=1, FUN=mean, na.rm=TRUE)
  water_equ <- equ_typical * AS  
  
  equ_err_all <- sapply(1:ntree, FUN=function(x) { error[[x]]$equ } )
  equ_typical_se <- sqrt(apply(equ_err_all^2, MARGIN=1, FUN=sum, na.rm=TRUE)) / ncol(equ_err_all)
  water_equ_upper <- water_equ + 1.96 * equ_typical_se * AS
  water_equ_lower <- water_equ - 1.96 * equ_typical_se * AS
  
  water_equ_day <- tapply(water_equ, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000    # devide 1000 to get usage in litre
  water_equ_day_upper <- tapply(water_equ_upper, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000
  water_equ_day_lower <- tapply(water_equ_lower, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000
  
  # mse weights
  mse_pred_all <- sapply(1:ntree, FUN=function(x) { ensemble[[x]]$mse } )
  mse_typical <- apply(mse_pred_all, MARGIN=1, FUN=mean, na.rm=TRUE)
  water_mse <- mse_typical * AS    # this is hourly data in litre
  
  mse_err_all <- sapply(1:ntree, FUN=function(x) { error[[x]]$mse } )
  mse_typical_se <- sqrt(apply(mse_err_all^2, MARGIN=1, FUN=sum, na.rm=TRUE)) / ncol(mse_err_all)
  water_mse_upper <- water_mse + 1.96 * mse_typical_se * AS
  water_mse_lower <- water_mse - 1.96 * mse_typical_se * AS
  
  water_mse_day <- tapply(water_mse, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000
  water_mse_day_upper <- tapply(water_mse_upper, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000
  water_mse_day_lower <- tapply(water_mse_lower, INDEX=temp_day, FUN=sum, na.rm=TRUE) / 1000
  
  # return the result
  pred_equ_obj <- list(pred=water_equ_day, 
                       pred_upper=water_equ_day_upper, 
                       pred_lower=water_equ_day_lower)
  pred_mse_obj <- list(pred=water_mse_day, 
                       pred_upper=water_mse_day_upper, 
                       pred_lower=water_mse_day_lower)
  return(list(equ=pred_equ_obj, mse=pred_mse_obj, time=unique(temp_day)))
}


## Calculating water usage
water_usage = water_usage_predict(ensemble=ensemble_pred$ensemble, 
                                  error=ensemble_pred$error, 
                                  timestamp=ensemble_pred$time, 
                                  size=total_sapwood)

save(water_usage, file="Predicted_water_usage.RData")


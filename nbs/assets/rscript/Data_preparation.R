## -----------------------------------------
## Data formatting for ensemble prediction
## -----------------------------------------


library(lubridate)
library(stringr)
library(zoo)


## Assuming all sap flux and weather data are stored in a folder called "Data".
## In this example, folder "Data" is inside the current folder.
## In practice, this folder will be somewhere else. 
## So need to change the directory accordingly.

## Assuming all the processed data (models, predictions) are saved locally.


## -----------------------
## (I) The training data
## -----------------------

## If data are saved as csv document
Meta_data <- read.table("Data/Meta_data.csv", header=TRUE, sep=",")
Sapflux_train <- read.table("Data/Sapflux_Tilia_train.csv", header=TRUE, sep=",")
Weather_train <- read.table("Data/Weather_Tilia_train.csv", header=TRUE, sep=",")


## If data are saved as RData files
# load("Data/Meta_data.RData")
# load("Data/Sapflux_Tilia_train.RData")
# load("Data/Weather_Tilia_train.RData")


## (1) First average the data within an hour

time0 <- Sapflux_train$Time
hour0 <- ymd_h(substr(time0, start=1, stop=13), tz="UTC") + 1
# plus 1 second to avoid the change in format when converting back and forth
# between string and datetime object
y0 <- sapply(Sapflux_train[, -(1:3)], FUN=function(x) {
  tapply(x, INDEX=hour0, FUN=mean, na.rm=TRUE)
}) 
Sapflux_hour <- data.frame(Time=unique(hour0), y0)
rownames(Sapflux_hour) <- NULL

time1 <- Weather_train$Time
hour1 <- ymd_h(substr(time1, start=1, stop=13), tz="UTC") + 1
y1 <- sapply(Weather_train[, -(1:3)], FUN=function(x) {
  tapply(x, INDEX=hour1, FUN=mean, na.rm=TRUE)
})
Weather_hour <- data.frame(Time=unique(hour1), y1)
rownames(Weather_hour) <- NULL


## (2) Then get the mean sap flux density by 
## (a) averaging the inner and outer sap flux density of large trees or 
## (b) taking the outer sap flux density of small trees
## The criterion of large trees are R >= 3.5 cm.

ntree <- (length(Sapflux_hour) - 1) / 2

treeID0 <- strsplit(names(Sapflux_hour)[1+(1:ntree)], split="_")
treeID <- sapply(treeID0, FUN=function(x) { 
  split <- unlist(strsplit(x, split="_"))
  tree_name <- str_flatten(split[2:3], collapse = "_")
  return(tree_name)
})

radius <- Meta_data$Radius[Meta_data$TreeID %in% treeID]

ynew <- list()
for (i in 1:ntree) {
  if (radius[i] >= 3.5) {
    ytemp <- 0.5 * (Sapflux_hour[[i+1]] + Sapflux_hour[[i+ntree+1]])
  } else {
    ytemp <- Sapflux_hour[[i+ntree+1]]   # outer only
  }
  ynew[[i]] <- ytemp
}
names(ynew) <- treeID
ydat <- as.data.frame(ynew)

Sapflux_hour_avg <- data.frame(Time=Sapflux_hour$Time, ydat)


## (3) Selecting the period for modelling 
## The main purpose is to remove the period before the leaves are fully expanded
## For example, we exclude the first 20 days

timeID <- Sapflux_hour_avg$Time > as.POSIXct("2023-05-20", tz="UTC")
Sapflux_modelling <- Sapflux_hour_avg[timeID, ]

timeID <- Weather_hour$Time > as.POSIXct("2023-05-20", tz="UTC")
Weather_modelling <- Weather_hour[timeID, ]

# Check the time labels 
match1 <- Sapflux_modelling$Time %in% Weather_modelling$Time
match2 <- Weather_modelling$Time %in% Sapflux_modelling$Time
if (!(all(match1) & all(match2))) {
  Sapflux_modelling <- Sapflux_modelling[match1, ]
  Weather_modelling <- Weather_modelling[match2, ]
}


## (4) Adding additional covariates
## They are: daily mean air temperature, daily maximum temperature,
## daily mean humidity and day-of-year (for the seasonal pattern)

day0 <- ymd(substr(Weather_modelling$Time, start=1, stop=10), tz="UTC")
meanAirTemp <- tapply(Weather_modelling$AirTemp, INDEX=day0, 
                      FUN=mean, na.rm=TRUE)
maxAirTemp <- tapply(Weather_modelling$AirTemp, INDEX=day0, 
                     FUN=max, na.rm=TRUE)
meanHumidity <- tapply(Weather_modelling$Humidity, INDEX=day0, 
                       FUN=mean, na.rm=TRUE)

repl <- tapply(day0, INDEX=day0, FUN=length)
avgTemp <- rep(meanAirTemp, times=repl)
maxTemp <- rep(maxAirTemp, times=repl)
avgHumid <- rep(meanHumidity, times=repl)

Weather_modelling$avgTemp <- avgTemp
Weather_modelling$maxTemp <- maxTemp
Weather_modelling$avgHumid <- avgHumid

DoY <- yday(Weather_modelling$Time)
Weather_modelling$DoY <- DoY


## Save the two data frames for modelling
save(Sapflux_modelling, Weather_modelling, 
     file="Modelling_data.RData")


## If we want to fit models using data from previous years,
## then we will also need to format the historical data,
## and save them in a file called "Historical_data.RData".
## This is most likely done prior to the current year.
## The file is probably stored in the "Data" folder



## ---------------------------
## (II) The prediction data
## ---------------------------

## Here we assuming we have the hourly data
## So we don't need to average within an hour

## If data are saved as csv document
Weather_pred <- read.csv("Data/Weather_Tilia_pred.csv", header=TRUE, sep=",")

## If data are saved as RData file
# load("Data/Weather_Tilia_pred.RData")


## Add covariates (mean, max, day-of-year)
day0 <- ymd(substr(Weather_pred$Time, start=1, stop=10), tz="UTC")
meanAirTemp <- tapply(Weather_pred$AirTemp, INDEX=day0, 
                      FUN=mean, na.rm=TRUE)
maxAirTemp <- tapply(Weather_pred$AirTemp, INDEX=day0, 
                     FUN=max, na.rm=TRUE)
meanHumidity <- tapply(Weather_pred$Humidity, INDEX=day0, 
                       FUN=mean, na.rm=TRUE)

repl <- tapply(day0, INDEX=day0, FUN=length)
avgTemp <- rep(meanAirTemp, times=repl)
maxTemp <- rep(maxAirTemp, times=repl)
avgHumid <- rep(meanHumidity, times=repl)

Weather_pred$avgTemp <- avgTemp
Weather_pred$maxTemp <- maxTemp
Weather_pred$avgHumid <- avgHumid

DoY <- yday(Weather_pred$Time)
Weather_pred$DoY <- DoY

save(Weather_pred, file="Prediction_data.RData")


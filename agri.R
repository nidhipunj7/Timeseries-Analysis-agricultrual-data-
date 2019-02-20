rm(list=ls(all=T))

setwd("Z:/Data Science/Projects/Agriculture/")
data <- read.csv("Monthly_data_cmo.csv")
data <- na.omit(data)

class(data)

dim(data)
n <- rnorm(data$arrivals_in_qtl)
qqnorm(n) 
qqline(n)

d <- as.data.frame(unique(data$Commodity))

# no. of APMCs
length(unique(data$APMC))

#no. of Commodities
length(unique(data$Commodity))

data1 <- data
data <- data1

#Split into Commodities 

commodity <- split(data, data$Commodity)


# bp <- boxplot(commodity$`ambat chuka`[,5], plot = TRUE)
#commodity$`ambat chuka`[which(commodity$`ambat chuka`[,5] %in% bp$out),5] <- NA
# 
for(i in 5:8){
  bp <- boxplot(commodity$`ambat chuka`[,i])
  boxplot(commodity$`ambat chuka`[which(commodity$`ambat chuka`[,i] %in% bp$out),i])
  commodity$`ambat chuka`[which(commodity$`ambat chuka`[,i] %in% bp$out),i] <- NA   # to remove the outliers
}

boxplot(commodity$`amba koy`)
boxplot(commodity$`ambat chuka`)
boxplot(commodity$amla)
boxplot(commodity$apple)
boxplot(commodity$arvi)
boxplot(commodity$aster)
boxplot(commodity$awala)
boxplot(commodity$bajri)
boxplot(commodity$banana)
boxplot(commodity$`banana(raw)`)

##### Filtering Outliers ####

for(j in 2:205){
for(i in 5:8){
  bp <- boxplot(commodity[[j]][,i], plot = FALSE)
  commodity[[j]][which(commodity[[j]][,i] %in% bp$out),i] <- NA   # to remove the outliers
}
}


#number of outliers in each commodity AND removing Outliers

for(i in 2:205){
  print(names(commodity[i]))
  print(sum(is.na(commodity[[i]])))
  n <- sum(is.na(commodity[[i]]))
  if(n!=0)
  commodity[[i]] <- na.omit(commodity[[i]])
}

sum(is.na(commodity[[24]]))

### Detect Seasonality Type 

# library(plotly)
# library(forecast)
# 
# plot_ly(da,x = da$`commodity$apple$Month`,y=da$arrival,type = 'scatter', mode = 'lines')
# 
# plot(da, x = da$`commodity$apple$Month`, y = da$arrival)

##1. amba koy

da <- as.data.frame(commodity$`amba koy`$date)
da$price <- commodity$`amba koy`$modal_price

amba.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(amba.ts[,2]) #additive trend for apple 

# trend

trend_amba = ma(amba.ts, order = 12, centre = T)
plot(amba.ts[,2])
lines(trend_amba[,2])
plot(trend_amba[,2])

##Detrend the Time Series

detrend_amba = amba.ts - trend_amba
plot(as.ts(detrend_amba[,2]))

decomp <- stl(log(amba.ts[,2]), s.window="periodic")
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(amba.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Number of passengers (thousands)")


##2. ambat chuka
rm(da)

da <- as.data.frame(commodity$`ambat chuka`$date)
da$price <- commodity$`ambat chuka`$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

# trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(ts(comm.ts[,2],freq=12), t.window=15, s.window="per", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")+ scale_y_continuous(breaks = NULL)


ts_comm = ts(comm.ts[,2], frequency = 4)
decompose_amba = decompose(ts_comm, "additive")

plot(as.ts(decompose_amba$seasonal))
plot(as.ts(decompose_amba$trend))
plot(as.ts(decompose_amba$random))
plot(decompose_amba)

##3. amla
rm(da)

da <- as.data.frame(commodity$amla$date)
da$price <- commodity$amla$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")


ts_comm = ts(comm.ts[,2], frequency = 4)
decompose_amba = decompose(ts_comm, "additive")

plot(as.ts(decompose_amba$seasonal))
plot(as.ts(decompose_amba$trend))
plot(as.ts(decompose_amba$random))
plot(decompose_amba)


##4. apple
rm(da)

da <- as.data.frame(commodity$apple$date)
da$price <- commodity$apple$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

# trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")


##4. arvi
rm(da)

da <- as.data.frame(commodity$arvi$date)
da$price <- commodity$arvi$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##6. aster
rm(da)

da <- as.data.frame(commodity$aster$date)
da$price <- commodity$aster$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend 

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##7. awala
rm(da)

da <- as.data.frame(commodity$awala$date)
da$price <- commodity$awala$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")




##8. Bajri
rm(da)

da <- as.data.frame(commodity$bajri$date)
da$price <- commodity$bajri$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##9. Banana
rm(da)

da <- as.data.frame(commodity$banana$date)
da$price <- commodity$banana$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")

##10. Banana(raw)
rm(da)

da <- as.data.frame(commodity$`banana(raw)`$date)
da$price <- commodity$`banana(raw)`$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")





# ts_comm = ts(comm.ts[,2], frequency = 4)
# decompose_amba = decompose(ts_comm, "additive")
# 
# plot(as.ts(decompose_amba$seasonal))
# plot(as.ts(decompose_amba$trend))
# plot(as.ts(decompose_amba$random))
# plot(decompose_amba)
# 
# comm.ts.qtr <- aggregate(comm.ts, nfrequency=4)
# comm.ts.yr <- aggregate(comm.ts, nfrequency=1)
# plot.ts(comm.ts[,2], main = "Monthly Apple Production", xlab = "Year", ylab = "ML")
# plot.ts(comm.ts.qtr[,2], main = "Quarterly Beer Production in Australia", xlab = "Year", ylab = "ML")
# plot.ts(comm.ts.yr[,2], main = "Yearly arrival of Apple", xlab = "Year", ylab = "ML")
# rm(da1)
# 
# da1 <- as.data.frame(commodity$arvi$date)
# da1$Price <- commodity$arvi$modal_price
# comm.ts <- ts(da1, frequency = 12, start = c(2014,1), end = c(2016,12))
# plot(comm.ts[,2])
# 
# comm.ts <- ts(da1, frequency = 12, start = c(2014,1), end = c(2016,12))
# comm.ts.qtr <- aggregate(comm.ts, nfrequency=4)


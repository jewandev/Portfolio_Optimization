
## dtwclust----
library(dtwclust)
data("uciCT")
glimpse(CharTraj)
pc <- tsclust(CharTraj, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

hc <- tsclust(CharTraj, type = "hierarchical", k = 20L, 
              distance = "sbd", trace = TRUE,
              control = hierarchical_control(method = "average"))
plot(hc)

# Calculate autocorrelation up to 50th lag, considering a list of time series as input
acf_fun <- function(series, ...) {
  lapply(series, function(x) { as.numeric(acf(x, lag.max = 50L, plot = FALSE)$acf) })
}
# Autocorrelation-based fuzzy c-means
fc <- tsclust(CharTraj[1L:25L], type = "fuzzy", k = 5L,
              preproc = acf_fun, distance = "L2",
              seed = 123L)
fc

# Multivariate series provided as a list of matrices, using GAK distance
mvc <- tsclust(CharTrajMV[1L:20L], k = 4L, distance = "gak", seed = 390L)
# Note how the variables of each series are appended one after the other in the plot
plot(mvc, labels = list(nudge_x = -10, nudge_y = 1))



# kospi, kosdaq, usd_krw 종가만 추출 ----
getwd()
setwd('/Users/wan/GitHub/Portfolio_Optimization/time_series_clustering/')

### 코스피
kospi <- read.csv('data/kospi.csv')
library(dplyr)
glimpse(kospi)
library(timetk)
kospi_price <- kospi[c(1, 2)]
colnames(kospi_price) <- (c('Date', 'Price'))
kospi_price <- na.omit(kospi_price)
library(readr)
kospi_price$Price <- parse_number(kospi_price$Price) # 열에서 숫자만 추출
kospi_price$Date <- ymd(kospi_price$Date) # ymd()로 형태를 yyyy-mm-dd로 변경
kospi_price <- tk_xts(kospi_price, date_var = Date) # tk_xts()로 시계열로 변경, 인덱스: kospi_price$Date
print(tail(kospi_price))
ksp_ep = endpoints(kospi_price, on = 'months')
kospi_p <- kospi_price[c(ksp_ep[2]:ksp_ep[length(ksp_ep)-1]), 1]
kospi_p <- kospi_p[-1, 1]
write.csv(data.frame(kospi_p), 'data/kospi_close.csv')
# data.frame으로 설정해서 시계열 인덱스 안 사라지게 함.

### 코스닥
kosdaq <- read.csv('data/kosdaq.csv')
kodaq_price <- kosdaq[c(1, 2)]
colnames(kodaq_price) <- (c('Date', 'Price'))
kodaq_price <- na.omit(kodaq_price)
kodaq_price$Price <- parse_number(kodaq_price$Price)
kodaq_price$Date <- ymd(kodaq_price$Date)
kodaq_price <- tk_xts(kodaq_price, date_var = Date)
print(tail(kodaq_price))
ksdq_ep = endpoints(kodaq_price, on = 'months')
kosdaq_p <- kodaq_price[c(ksdq_ep[2]:ksdq_ep[length(ksdq_ep)-1]), 1]
kosdaq_p <- kosdaq_p[-1, 1]
write.csv(data.frame(kosdaq_p), 'data/kosdaq_close.csv')

### 달러환율
usd <- read.csv('data/USD_KRW.csv')
usd_price <- usd[c(1, 2)]
colnames(usd_price) <- (c('Date', 'Price'))
usd_price <- na.omit(usd_price)
usd_price$Price <- parse_number(usd_price$Price)
usd_price$Date <- ymd(usd_price$Date)
usd_price <- tk_xts(usd_price, date_var = Date)
print(tail(usd_price))
usd_ep = endpoints(usd_price, on = 'months')
usd_p <- usd_price[c(usd_ep[2]:usd_ep[length(usd_ep)-1]), 1]
usd_p <- usd_p[-1, 1]
write.csv(data.frame(usd_p), 'data/usd_close.csv')

# 분할해보기 ----
library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)
last(ksp)

### 코스피
ksp = read.csv('data/kospi_close.csv', row.names = 1,
               stringsAsFactors = FALSE) %>% as.xts()
ksp_ep = endpoints(ksp, on = 'months')
head(ksp)
ksp_part <- list()
lookback <- 1
for (i in (lookback+1) : length(ksp_ep)) {
  ksp_part[[i-1]] <- as.vector(ksp[ksp_ep[i-lookback] : ksp_ep[i] , 1])
}
ksp_part

### 코스닥
ksdq = read.csv('data/kosdaq_close.csv', row.names = 1,
                stringsAsFactors = FALSE) %>% as.xts()
ksdq_ep = endpoints(ksdq, on = 'months')
head(ksdq)
ksdq_part <- list()
lookback <- 1

for (i in (lookback+1) : length(ksdq_ep)) {
  ksdq_part[[i-1]] <- as.vector(ksdq[ksdq_ep[i-lookback] : ksdq_ep[i] , 1])
}

### 달러
usd = read.csv('data/usd_close.csv', row.names = 1,
               stringsAsFactors = FALSE) %>% as.xts()
usd_ep = endpoints(usd, on = 'months')
head(usd)
usd_part <- list()
lookback <- 1

for (i in (lookback+1) : length(usd_ep)) {
  usd_part[[i-1]] <- as.vector(usd[usd_ep[i-lookback] : usd_ep[i] , 1])
}

length(ksp_part)
length(ksdq_part)
length(usd_part)

### 분할한 구간들 각각 표준화해주기 ----
# 표준화: (값 - 평균)/표준편차
mean_ex <- mean(c(ksp_part[[1]][1:length(ksp_part[[1]])]))
(ksp_part[[1]] - mean_ex)/sd(ksp_part[[1]])

## 반복문으로 전체 ㄱㄱ
# 코스피
ksp_part_Norm <- list()
for (i in c(1: length(ksp_part))){
  part_mean <- mean(c(ksp_part[[i]][1:length(ksp_part[[i]])]))
  ksp_part_Norm[[i]] <- (ksp_part[[i]] - part_mean) / sd(ksp_part[[i]])
}
ksp_part_Norm

# 코스닥
ksdq_part_Norm <- list()
for (i in c(1: length(ksdq_part))){
  part_mean <- mean(c(ksdq_part[[i]][1:length(ksdq_part[[i]])]))
  ksdq_part_Norm[[i]] <- (ksdq_part[[i]] - part_mean) / sd(ksdq_part[[i]])
}
ksdq_part_Norm

# 달러환율
usd_part_Norm <- list()
for (i in c(1: length(usd_part))){
  part_mean <- mean(c(usd_part[[i]][1:length(usd_part[[i]])]))
  usd_part_Norm[[i]] <- (usd_part[[i]] - part_mean) / sd(usd_part[[i]])
}
usd_part_Norm

### 분할한 데이터 클러스터링 해보기 ----
ksp_part_Norm # 코스피
ksdq_part_Norm # 코스닥
usd_part_Norm # 달러환율

library(dtwclust)
data("uciCT")
### 코스피
glimpse(ksp_part_Norm)
pc <- tsclust(ksp_part_Norm, type = "partitional", k = 7L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 7L)))
plot(pc)


### 코스닥
glimpse(ksdq_part_Norm)
pc <- tsclust(ksdq_part_Norm, type = "partitional", k = 7L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 7L)))
plot(pc)

### 달러환율
glimpse(usd_part_Norm)
pc <- tsclust(usd_part_Norm, type = "partitional", k = 9L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 9L)))
plot(pc)

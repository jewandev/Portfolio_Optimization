library(dtwclust)
library(dtw)
setwd('/Users/wan/GitHub/Portfolio_Optimization/time_series_clustering')
## dtwclust----
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

# 분할해보기(비교 데이터 & 다음달데이터) ----
library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

### 코스피
ksp = read.csv('data/kospi_close.csv', row.names = 1,
               stringsAsFactors = FALSE) %>% as.xts()
ksp_ep = endpoints(ksp, on = 'months')
head(ksp)
ksp_part_now <- list()
lookback <- 1

# 군집 분류 리스트
for (i in (lookback+1) : (length(ksp_ep)-1)) {
  idx <- substr(as.character(index(ksp[ksp_ep[i]])), 1, 7)
  ksp_part_now[[idx]] <- as.vector(ksp[ksp_ep[i-lookback] : ksp_ep[i] , 1])
}
ksp_part_now

# 다음달 수익률 확인 리스트
ksp_part_next <- list()
for (i in (lookback+2) : length(ksp_ep)) {
  idx <- substr(as.character(index(ksp[ksp_ep[i]])), 1, 7)
  ksp_part_next[[idx]] <- as.vector(ksp[ksp_ep[i-lookback] : ksp_ep[i], 1])
}
ksp_part_next

### 코스닥
ksdq <- read.csv('data/kosdaq_close.csv', row.names = 1,
                stringsAsFactors = FALSE) %>% as.xts()
ksdq_ep <- endpoints(ksdq, on = 'months')
head(ksdq)
ksdq_part_now <- list()
lookback <- 1

# 군집 분류 리스트
for (i in (lookback+1) : (length(ksdq_ep)-1)) {
  idx <- substr(as.character(index(ksdq[ksdq_ep[i]])), 1, 7)
  ksdq_part_now[[idx]] <- as.vector(ksdq[ksdq_ep[i-lookback] : ksdq_ep[i] , 1])
}
ksdq_part_now

# 다음달 수익률 확인 리스트
ksdq_part_next <- list()
for (i in (lookback+2) : length(ksdq_ep)) {
  idx <- substr(as.character(index(ksdq[ksdq_ep[i]])), 1, 7)
  ksdq_part_next[[idx]] <- as.vector(ksdq[ksdq_ep[i-lookback] : ksdq_ep[i], 1])
}
ksdq_part_next

### 달러
usd <- read.csv('data/usd_close.csv', row.names = 1,
               stringsAsFactors = FALSE) %>% as.xts()
usd_ep <- endpoints(usd, on = 'months')
head(usd)
usd_part_now <- list()
lookback <- 1
# 군집 분류 리스트
for (i in (lookback+1) : (length(usd_ep)-1)) {
  idx <- substr(as.character(index(usd[usd_ep[i]])), 1, 7)
  usd_part_now[[idx]] <- as.vector(usd[usd_ep[i-lookback] : usd_ep[i] , 1])
}
usd_part_now
# 다음달 수익률 확인 리스트
usd_part_next <- list()
for (i in (lookback+2) : length(usd_ep)) {
  idx <- substr(as.character(index(usd[usd_ep[i]])), 1, 7)
  usd_part_next[[idx]] <- as.vector(usd[usd_ep[i-lookback] : usd_ep[i] , 1])
}
usd_part_next

length(ksp_part_now)
length(ksdq_part_now)
length(usd_part_now)

### 분할한 구간들 각각 표준화해주기 ----
# 표준화: (값 - 평균)/표준편차
mean_ex <- mean(c(ksp_part_now[[1]][1:length(ksp_part_now[[1]])]))
(ksp_part_now[[1]] - mean_ex)/sd(ksp_part_now[[1]])

## 반복문으로 전체 표준화
# 코스피(군집분석)
ksp_part_Norm <- list()
for (i in c(1: length(ksp_part_now))){
  part_mean <- mean(c(ksp_part_now[[i]][1:length(ksp_part_now[[i]])]))
  idx <- names(ksp_part_now)[i]
  ksp_part_Norm[[idx]] <- (ksp_part_now[[i]] - part_mean) / sd(ksp_part_now[[i]])
}
ksp_part_Norm

# 코스피(다음 달)
ksp_part_Norm_next <- list()
for (i in c(1: length(ksp_part_next))){
  part_mean <- mean(c(ksp_part_next[[i]][1:length(ksp_part_next[[i]])]))
  idx <- names(ksp_part_next)[i]
  ksp_part_Norm_next[[idx]] <- (ksp_part_next[[i]] - part_mean) / sd(ksp_part_next[[i]])
}
ksp_part_Norm_next

# 코스닥(이번달)
ksdq_part_Norm <- list()
for (i in c(1: length(ksdq_part_now))){
  part_mean <- mean(c(ksdq_part_now[[i]][1:length(ksdq_part_now[[i]])]))
  idx <- names(ksdq_part_now)[i]
  ksdq_part_Norm[[idx]] <- (ksdq_part_now[[i]] - part_mean) / sd(ksdq_part_now[[i]])
}
ksdq_part_Norm

# 코스닥(다음달)
ksdq_part_Norm_next <- list()
for (i in c(1: length(ksdq_part_next))){
  part_mean <- mean(c(ksdq_part_next[[i]][1:length(ksdq_part_next[[i]])]))
  idx <- names(ksdq_part_next)[i]
  ksdq_part_Norm_next[[idx]] <- (ksdq_part_next[[i]] - part_mean) / sd(ksdq_part_next[[i]])
}
ksdq_part_Norm_next

# 달러환율(이번달)
usd_part_Norm <- list()
for (i in c(1: length(usd_part_now))){
  part_mean <- mean(c(usd_part_now[[i]][1:length(usd_part_now[[i]])]))
  idx <- names(usd_part_now)[i]
  usd_part_Norm[[idx]] <- (usd_part_now[[i]] - part_mean) / sd(usd_part_now[[i]])
}
usd_part_Norm

# 달러환율(다음달)
usd_part_Norm_next <- list()
for (i in c(1: length(usd_part_next))){
  part_mean <- mean(c(usd_part_next[[i]][1:length(usd_part_next[[i]])]))
  idx <- names(usd_part_next)[i]
  usd_part_Norm_next[[idx]] <- (usd_part_next[[i]] - part_mean) / sd(usd_part_next[[i]])
}
usd_part_Norm_next

### 분할한 데이터 클러스터링 해보기 ----
ksp_part_Norm # 코스피
ksdq_part_Norm # 코스닥
usd_part_Norm # 달러환율
data("uciCT")
### 코스피
glimpse(ksp_part_Norm)

ksp.dtw <- tsclust(ksp_part_Norm,
                  type="partitional",
                  centroid = "pam",
                  k = 2L:14L,
                  distance ="dtw_basic",
                  seed=1234,
                  trace=T,
                  args = tsclust_args(dist = list(window.size = 30L))
                  )

ksp_eval_clust<-sapply(ksp.dtw, cvi) # 지표 값 데이터 저장

plot(ksp_eval_clust[1,], type = 'l', main="Silhouette index(to be maximized)")

#plot(ksp_eval_clust[2,], type = 'l', main="Score function(to be maximized)")

plot(ksp_eval_clust[3,], type = 'l', main="Calinski-Harabasz index(to be maximized)")

plot(ksp_eval_clust[4,], type = 'l', main="Davies–Bouldin index(to be minimized)")

plot(ksp_eval_clust[5,], type = 'l', main="Modified Davies–Bouldin index(to be minimized)")

plot(ksp_eval_clust[6,], type = 'l', main="Dunn index(to be maximized)")

plot(ksp_eval_clust[7,], type = 'l', main="COP index(to be minimized)")

plot(ksp_eval_clust[1,],type="l", main="sil index(to be maximized)")

## 5개 군집이 최적
# 군집 그래프
ksp_pc <- tsclust(ksp_part_Norm, type = "partitional", k = 5L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 30L)))
plot(ksp_pc)
# 각 군집 그래프
plot(ksp_pc, type = "sc", clus = 1L)

# 군집 별 센트로이드 그래프(중위)
plot(ksp_pc, type = "centroids", clus = 1L)
plot(ksp_pc, type = "centroids", clus = 2L)
plot(ksp_pc, type = "centroids", clus = 3L)
plot(ksp_pc, type = "centroids", clus = 4L)
plot(ksp_pc, type = "centroids", clus = 5L)

ksp_pc@control
ksp_pc@cluster
ex <- cbind(names(ksp_part_Norm), cluster = ksp_pc@cluster)

# 군집 별 구간 리스트
ksp_clust <- list()
for (i in 1:5){
  ksp_clust[i] <- list(ex[,1][as.numeric(ex[,2])==i])
}

# 군집 별 다음 구간
ksp_next_period <- ksp_clust
for (i in 1:5){
  for (j in 1:length(ksp_clust[[i]])){
    ksp_next_period[[i]][j] <- names(ksp_part_next[names(ksp_part_now)==ksp_clust[[i]][j]])
  }
}
ksp_next_period
ksp_part_now[ksp_clust[[1]][1]]
ksp_part_next[names(ksp_part_next) == ksp_next_period[[1]][1]]
ksp_next_period


# 다음달 수익률 확인 리스트
next_period_ts <- list()
for (i in 1:5){
  next_period_ts[[i]] <- ksp_part_next[ksp_next_period[[i]]]
  for (j in 1:length(next_period_ts[[i]])){
    next_period_ts[[i]][[j]] <- ts(next_period_ts[[i]][[j]], start = 1, end = length(next_period_ts[[i]][[j]]))
  }
}
next_period_ts

Return.calculate(next_period_ts[[1]][[1]]) %>% xts::last(length(next_period_ts[[1]][[1]])-1) %>%
  sapply(., function(x) {prod(1+x) - 1})

'''
for (i in (lookback+2) : length(ksp_ep)) {
  idx <- substr(as.character(index(ksp[ksp_ep[i]])), 1, 7)
  if (idx %in% ksp_next_period[[1]]){
    next_period_ts[[1]] <- append(next_period_ts[[1]], ksp_part_next[[idx]])
  }
  ksp_part_next[[idx]] <- ksp[ksp_ep[i-lookback] : ksp_ep[i], 1]
}
ksp_part_next
'''





''' 계층적 방법(이건 나중에 다뤄보자)
ksp_hier <- tsclust(ksp_part_Norm, type = "h", k = 5L, distance = "dtw")
plot(ksp_hier)
plot(ksp_hier, type="sc")
cutree(ksp_hier, k=6L)

'''




### 코스닥 : 9개 군집이 최적
glimpse(ksdq_part_Norm)

pc.dtw <- tsclust(ksdq_part_Norm,
                  type="partitional",
                  centroid = "pam",
                  k = 2L:14L,
                  distance ="dtw_basic",
                  seed=1234,
                  trace=T,
                  args = tsclust_args(dist = list(window.size = 30L))
)

eval_clust<-sapply(pc.dtw, cvi) # 지표 값 데이터 저장

plot(eval_clust[1,], type = 'l')
plot(eval_clust[4,], type = 'l')

pc <- tsclust(ksdq_part_Norm, type = "partitional", k = 9L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 30L)))
plot(pc)

### 달러환율: 10개 군집 최적
glimpse(usd_part_Norm)

pc.dtw <- tsclust(usd_part_Norm,
                  type="partitional",
                  centroid = "pam",
                  k = 2L:14L,
                  distance ="dtw_basic",
                  seed=1234,
                  trace=T,
                  args = tsclust_args(dist = list(window.size = 30L))
)

eval_clust<-sapply(pc.dtw, cvi) # 지표 값 데이터 저장

plot(eval_clust[1,], type = 'l')
plot(eval_clust[4,], type = 'l')

pc <- tsclust(usd_part_Norm, type = "partitional", k = 10L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 30L)))
plot(pc)


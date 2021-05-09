library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

length(ksp)
length(ksdq)
length(krw)
na.locf(cbind(ksp, ksdq, krw))
tail(cbind(ksp, ksdq, krw))
  #setNames(c(ksp, ksdq, krw))

prices <- na.locf(cbind(ksp, ksdq, krw)) %>%
  setNames(c('KOSPI', 'KOSDAQ', 'KRW'))

idx <- index(prices)%in%index(ksp)
prices <- prices[idx]
idx <- index(prices) < "2021-03-01"
prices <- prices[idx]
rets = na.omit(Return.calculate(prices))
tail(rets)
ep = endpoints(rets, on = 'months')
print(ep)
index(prices[ep])
wts = list()
lookback = 1
i = lookback + 1

sub_price = prices[ep[i-lookback] : ep[i] , 1]
head(sub_price, 3)
tail(sub_price, 3)
sma = mean(sub_price)

substr(as.Date(index(prices[ep])), 1, 7)
#---
# 군집 다음 수익률 +인지 -인지 알아보고 그거에 맞는 비율 설정하는 코드 짤 것
# 그리고 아래 행렬에 넣으면 됨
glimpse(ksp_clust_data)
ksp_clust_data[,2][ksp_clust_data[,1] %in% substr(as.Date(index(prices[ep])), 1, 7)[1]]
ksdq_clust_data[,2][ksdq_clust_data[,1] %in% substr(as.Date(index(prices[ep])), 1, 7)[1]]
usd_clust_data[,2][usd_clust_data[,1] %in% substr(as.Date(index(prices[ep])), 1, 7)[1]]
substr(as.Date(index(prices[ep])), 1, 7)[1]

ksp_clust_data <- as.data.frame(ksp_clust_data)
ksdq_clust_data <- as.data.frame(ksdq_clust_data)
usd_clust_data <- as.data.frame(usd_clust_data)

ksp_clust_data$sign <- ifelse(ksp_rets_mean[as.numeric(ksp_clust_data$cluster)]>0, 1, 0)
ksdq_clust_data$sign <- ifelse(ksdq_rets_mean[as.numeric(ksdq_clust_data$cluster)]>0, 1, 0)
usd_clust_data$sign <- ifelse(usd_rets_mean[as.numeric(usd_clust_data$cluster)]<0, 1, 0)

clust_data <- ksp_clust_data[,c(1,3)] %>% setNames(c("Date", "KOSPI"))
clust_data$KOSDAQ <- ksdq_clust_data[,3]
clust_data$USD <- usd_clust_data[,3]
clust_data$Score <- clust_data$KOSPI+clust_data$KOSDAQ+clust_data$USD
clust_data

#usd_rets_data
wt <- rep(0, 3)
wt[1] <- ifelse(clust_data$KOSPI[1] > 0, 0.5, 0.45)
#wt[1] = ifelse(last(sub_price) > sma, 1, 0)
wt[2] <- ifelse(clust_data$KOSDAQ[1] > 0, 0.5, 0.45)
if (clust_data$USD[1] < 1){
  wt[1] = wt[1] - 0.025
  wt[2] = wt[2] - 0.025
}
#wt[2] = 1 - wt[1]
wt[3] <- 1 - wt[1] - wt[2]
#wt[3] <- ifelse(clust_data$USD[1] == 0, wt[1] - 0.025 & wt[2] - 0.025 & , )
wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
wts[[i]]

rets[ep[119]]
ep = endpoints(rets, on = 'months')
head(rets)
tail(rets)
wts = list()
#lookback = 10
#tail(clust_data)
#length(clust_data$Date)
for (i in 2:(length(ep)-1)) {
  #sub_price = prices[ep[i-lookback] : ep[i] , 1]
  #sma = mean(sub_price)
  wt = rep(0, 3)
  wt[1] <- ifelse(clust_data$KOSPI[i] > 0, 0.5, 0.45)
  #wt[1] = ifelse(last(sub_price) > sma, 1, 0)
  wt[2] <- ifelse(clust_data$KOSDAQ[i] > 0, 0.5, 0.45)
  if (clust_data$USD[i] < 1){
    wt[1] = wt[1] - 0.025
    wt[2] = wt[2] - 0.025
  }
  #wt[2] = 1 - wt[1]
  wt[3] = 1 - wt[1] - wt[2]
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}
wts[1]
length(clust_data$Date)
wts = do.call(rbind, wts)
tail(rets)
sum(is.na(rets))
tail(wts)
length(ep)
wts[,1] + wts[,2] + wts[,3]==1
Tactical = Return.portfolio(R = rets,weights = wts, verbose = T)
tail(rets)
tail(Tactical$EOP.Value)
tail(is.na(Tactical$BOP.Weight))
tail(Tactical$EOP.Value)
portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>%
  setNames(c('매수 후 보유', '국면분석 전략'))

portfolios = cbind(rets[,1], Tactical$returns) %>%
  setNames(c('매수 후 보유', '국면분석 전략'))
tail(portfolios)

tail(Tactical, 10)

par(family = 'AppleGothic')
tail(portfolios)
charts.PerformanceSummary(portfolios,
                          main = "Buy & Hold vs Tactical")

turnover = xts(rowSums(abs(Tactical$BOP.Weight -
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(turnover)

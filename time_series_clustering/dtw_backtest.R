library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

#ksp <- kospi_p
#ksdq <- kosdaq_p
#usd <- usd_p
#krw <- krw_p
length(ksp)
length(ksdq)
length(krw)
na.locf(cbind(ksp, ksdq, krw))
tail(cbind(ksp, ksdq, krw))
  #setNames(c(ksp, ksdq, krw))

prices <- na.locf(cbind(ksp, ksdq, krw)) %>%
  setNames(c('KOSPI', 'KOSDAQ', 'KRW'))

#idx <- index(prices)%in%index(ksp)
#prices <- prices[idx]
#idx <- index(prices) > "2014-12-31"
#prices <- prices[idx]
rets = na.omit(Return.calculate(prices))
sum(is.infinite(rets))
sum(is.na(rets))
sum(is.nan(rets))
rets <- replace(rets, is.infinite(rets), 0)
sum(is.infinite(rets))
head(rets)
tail(rets)
cor(rets)
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

rets[ep[2]]
ep = endpoints(rets, on = 'months')
head(rets)
tail(rets)
lookback = 1
clust_data[1,]
nrow(rets[ep])

wts = list()
for (i in lookback:(length(ep)-2)) {
  wt <- rep(0,3)
  if(clust_data$KOSPI[i] == 1 & clust_data$KOSDAQ[i] == 1 & clust_data$USD[i] == 1){
    wt[1] <- 0.5
    wt[2] <- 0.5
    wt[3] <- 0.0
  }
  else if (clust_data$KOSPI[i] == 1 & clust_data$KOSDAQ[i] == 1 & clust_data$USD[i] == 0){
    wt[1] <- 0.475
    wt[2] <- 0.475
    wt[3] <- 0.05
  }
  else if (clust_data$KOSPI[i] == 1 & clust_data$KOSDAQ[i] == 0 & clust_data$USD[i] == 1){
    wt[1] <- 0.50
    wt[2] <- 0.45
    wt[3] <- 0.05
  }
  else if (clust_data$KOSPI[i] == 0 & clust_data$KOSDAQ[i] == 1 & clust_data$USD[i] == 1){
    wt[1] <- 0.45
    wt[2] <- 0.50
    wt[3] <- 0.05
  }
  else if (clust_data$KOSPI[i] == 1 & clust_data$KOSDAQ[i] == 0 & clust_data$USD[i] == 0){
    wt[1] <- 0.475
    wt[2] <- 0.425
    wt[3] <- 0.1
  }
  else if (clust_data$KOSPI[i] == 0 & clust_data$KOSDAQ[i] == 0 & clust_data$USD[i] == 1){
    wt[1] <- 0.45
    wt[2] <- 0.45
    wt[3] <- 0.1
  }
  else if (clust_data$KOSPI[i] == 0 & clust_data$KOSDAQ[i] == 1 & clust_data$USD[i] == 0){
    wt[1] <- 0.425
    wt[2] <- 0.475
    wt[3] <- 0.1
  }
  else if (clust_data$KOSPI[i] == 0 & clust_data$KOSDAQ[i] == 0 & clust_data$USD[i] == 0){
    wt[1] <- 0.425
    wt[2] <- 0.425
    wt[3] <- 0.15
  }
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i+1]]))
}

wts = do.call(rbind, wts)
glimpse(wts)

Tactical = Return.portfolio(R = rets, weights = wts, verbose = T)

head(Tactical$EOP.Value)
tail(Tactical$EOP.Value)
head(Tactical$returns)
tail(Tactical$returns)

Tactical$returns <- Tactical$returns[-length(Tactical$returns)]
tail(Tactical$returns)
portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>%
  setNames(c('KOSPI', 'Portfolio'))
head(rets[,1])
head(portfolios)

par(family = 'AppleGothic')
tail(portfolios)
charts.PerformanceSummary(portfolios,
                          main = "KOSPI vs Portfolio")
charts.PerformanceSummary(portfolios$Portfolio)
turnover = xts(rowSums(abs(Tactical$BOP.Weight -
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(turnover)

# 누적수익률 그림
library(PerformanceAnalytics)
chart.CumReturns(portfolios$KOSPI)
chart.CumReturns(portfolios$Portfolio)
# 누적수익률 수치
Return.cumulative(portfolios$KOSPI)
Return.cumulative(portfolios$Portfolio)

# 연율화 수익률(산술)
Return.annualized(portfolios$KOSPI, geometric = F)
Return.annualized(portfolios$Portfolio, geometric = F)

# 연율화 수익률(기하)
Return.annualized(portfolios$KOSPI)
Return.annualized(portfolios$Portfolio)

# 연율화 변동성
sd(portfolios$KOSPI) * sqrt(12)
sd(portfolios$Portfolio) * sqrt(12)

# 최대낙폭
maxDrawdown(portfolios$KOSPI)
maxDrawdown(portfolios$Portfolio)

# 연도별 수익률
apply.yearly(portfolios$KOSPI, Return.cumulative)
apply.yearly(portfolios$Portfolio, Return.cumulative)
# 연도별 수익률 그래프
library(lubridate)
library(tidyr)
library(ggplot2)

R.yr = apply.yearly(portfolios$Portfolio, Return.cumulative) %>%
#R.yr = apply.yearly(portfolios$KOSPI, Return.cumulative) %>%
  fortify.zoo() %>%
  mutate(Index = year(Index)) %>%
  gather(key, value, -Index) %>%
  mutate(key = factor(key, levels = unique(key)))

ggplot(R.yr, aes(x = Index, y = value, fill = key)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle('Yearly Return') +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  scale_y_continuous(expand = c(0.03, 0.03)) +
  scale_x_continuous(breaks = R.yr$Index,
                     expand = c(0.01, 0.01)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank() ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  geom_text(aes(label = paste(round(value * 100, 2), "%"),
                vjust = ifelse(value >= 0, -0.5, 1.5)),
            position = position_dodge(width = 1),
            size = 3)

# 승률
UpsideFrequency(portfolios$Portfolio, MAR = portfolios$KOSPI)
UpsideFrequency(portfolios$KOSPI, MAR = portfolios$Portfolio)

# 롤링 윈도우 값
roll_12 = portfolios$Portfolio %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  UpsideFrequency(.,MAR = portfolios$KOSPI)

roll_24 = portfolios$Portfolio %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 24, Return.annualized) %>% na.omit() %>%
  UpsideFrequency(., MAR = portfolios$KOSPI)

roll_36 = portfolios$Portfolio %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 36, Return.annualized) %>% na.omit() %>%
  UpsideFrequency(., MAR = portfolios$KOSPI)

roll_win = cbind(roll_12, roll_24, roll_36)
print(roll_win)


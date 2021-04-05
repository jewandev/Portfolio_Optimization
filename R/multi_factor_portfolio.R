setwd('/Users/wan/GitHub/Portfolio_Optimization/R')
#
### ------
KOR_ticker[invest_qvm, ]
price
tail(KOR_price[,invest_qvm])
rets = Return.calculate(prices) %>% na.omit()
length(KOR_price[,invest_qvm])
####
dt_start = "2018-03-26"
dt_end = "2021-03-24"
getSymbols('DEXKOUS', src='FRED', from = "2018-03-26", to = "2021-03-24", auto.assign = FALSE)
tail(DEXKOUS)
prices = do.call(cbind,
                 lapply('DEXKOUS', function(x) Ad(get(x))))
SPY
library(quantmod)
library(Quandl)
DEXKOUS = Quandl("FRED/DEXKOUS", start_date=dt_start,end_date=dt_end,type="xts")
getSymbols('DEXKOUS', src = 'FRED')
DEXKOUS['2018-03-26']
prices <- do.call(cbind,
                  lapply(DEXKOUS, function(x) index(DEXKOUS[x])>"2018-03-25" & index(DEXKOUS[x]) < "2021-03-24"))

index(DEXKOUS["2018-03-26"]) > "2018-03-25"
xts::last(DEXKOUS["2018-03"],2)

DEXKOUS["2018-03-26::2021-03-24"]


#### 멀티팩터 포트폴리오-----
# 앞에서 배운 팩터 이론들과 결합 방법을 응용해서 진행

# 사용되는 지표
# - 퀄리티: 자기자본이익률, 매출 총이익, 영업활동현금흐름
# - 밸류: PER, PBR, PSR, PCR
# - 모멘텀: 3개월 수익률, 6개월 수익률, 12개월 수익률
library(dplyr)
library(xts)
library(stringr)
library(tidyr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

### 퀄리티 지표 계산
if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}
# 자기자본이익률
quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]
#매출총이익
quality_gpa = (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]
# 영업활동현금흐름
quality_cfo =
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))

factor_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()
# mutate_all()로 랭킹 구하고 표준화
# desc()로  내림차순으로 정리
# rowSums()로 Z-Score를 종목별로 합치기

# Z-Score의 히스토그램
factor_quality %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()

### 밸류 지표 계산
factor_value = KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_value %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()

### 모멘텀 지표 계산
library(PerformanceAnalytics)
library(dplyr)

ret_3m = Return.calculate(KOR_price) %>% xts::last(60) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_6m = Return.calculate(KOR_price) %>% xts::last(120) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_12m = Return.calculate(KOR_price) %>% xts::last(252) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_bind = cbind(ret_3m, ret_6m, ret_12m) %>% data.frame()

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_mom %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()

### 팩터 간 상관관계 확인
library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))
# 상관관계 매우 낮은 것으로 확인 -> 분산효과를 기대

### 계산된 팩터들을 토대로 최종 포트폴리오 구성

factor_qvm =
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>% 
  # 분포가 다르기 때문에 다시 한 번 scale()로 정규화
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%
  # 각 팩터에 동일한 비중(0.33) 곱하기
  rowSums() # 최종 합

# 최종적으로 기준 상위 20종목 선택
invest_qvm = rank(factor_qvm) <= 20

data.frame(cbind("ticker" = KOR_ticker[,1][invest_qvm],
                 "corp" = KOR_ticker[,2][invest_qvm])) 
### 선택된 종목의 퀄리티 지표 별 분포
library(tidyr)

quality_profit[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)
# 대부분 종목의 수익성이 높음이 확인됨

### 선택된 종목의 가치지표 별 분포
KOR_value[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)
# 대부분 종목의 값이 낮아 밸류 종목임이 확인됨

### 선택된 종목의 기간 별 수익률 분포
ret_bind[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)
# 대부분 수익률 높음

### 최종 포트폴리오
KOR_ticker[invest_qvm, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm, ], 2)) %>%
  cbind(round(KOR_value$PBR[invest_qvm], 2)) %>%
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M'))
# 전반적으로 ROE는 높고, PBR은 낮으며 12개월 수익률은 높은 모습
# 물론 특정 팩터의 강도가 약하더라도 나머지 팩터의 강도가 충분히 강하다면
# 포트폴리오에 편입 됨.
cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()

####
Sys.setenv(TZ="Asia/Seoul")
Sys.getenv('TZ')
kospi <- getSymbols('069500.KS')
kospi
Sys.getenv(TZ="")
kospi <- do.call(cbind, lapply(kospi, function(x)Ad(get(x))))
kospi <- kospi["2018-07-13::2021-03-24"]
rets_ks <- Return.calculate(kospi) %>% na.omit()
head(rets_ks)
tail(rets_ks)
# 멀티
rets <- Return.calculate(KOR_price)[-1,]
multi <- rets[, invest_qvm] %>% na.omit()
tail(multi)
head(multi)
#index(multi) = as.POSIXlt(as.Date(index(multi)), "UTC")
#index(rets_ks) = as.POSIXlt(index(rets_ks))
#index(rets_ks) <- date(index(rets_ks))
index(multi) <- date(index(multi))
#index(multi) <- date(index(multi)+1)
glimpse(index(rets_ks))
glimpse(index(multi))
index(rets_ks)
index(multi)
nrow(rets_ks)
nrow(multi)
index(multi)[1] == index(rets_ks)[1]
index(multi)[662] == index(rets_ks)[646]

portfolio_ks <- Return.portfolio(rets_ks, verbose = TRUE)
portfolio <- Return.portfolio(multi, verbose = TRUE)
#index(rets_ks) = as.POSIXlt(index(rets_ks), "UTC")
#index(multi) = as.POSIXlt(as.Date(index(multi)), "UTC")
#index(rets_ks)
#nrow(rets_ks)
#nrow(multi)
portfolios <- cbind(portfolio_ks$returns, portfolio$returns) %>%
  setNames(c("KODEX200", "멀티팩터 포트폴리오"))
charts.PerformanceSummary(portfolios, main = "멀티팩터 포트폴리오")

cbind(portfolio_ks$returns) %>%
 # setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')
cbind( portfolio$returns) %>%
  # setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')



# 최종 포트폴리오 내 종목들의 지표 별 평균 값

cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()
cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()

#Return.portfolio(R, weights = NULL, wealth.index = FALSE,
#                 contribution = FALSE, geometric = TRUE,
#                 rebalance_on = c(NA, "years", "quarters", 
#                                  "months", "weeks", "days"),
#                 value = 1, verbose = FALSE, ...)


rets <- Return.calculate(KOR_price)[-1,]
glimpse(rets)

names(rets) <- gsub("X", "", colnames(rets))

R <- rets[, invest_qvm] %>% na.omit()
portfolio <- Return.portfolio(R, verbose = TRUE)

portfolios <- na.omit(cbind(portfolio$returns))
charts.PerformanceSummary(portfolios, main = "멀티팩터 포트폴리오")

ep = endpoints(rets, on = 'months')
index(rets[ep[2],])
index(rets[5])
index(portfolio$returns)




library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('SPY', # 미국 주식
            'IEV', # 유럽 주식 
            'EWJ', # 일본 주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC'  # 상품
)
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>%
  # 수정주가만 선택
  setNames(symbols)

# 수익률 계산
rets = Return.calculate(prices) %>% na.omit()
rets <- rets["2018-07-16::2021-03-24"]
ymd(index(rets[1])) %>%
  with_tz('UTC')
library(tidyr)
library(dplyr)
library(corrplot)

# 각 ETF 수익률 간 상관관계
cor(rets) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 0.7,
           tl.cex = 0.6, tl.srt=45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar = c(0,0,0.5,0))


## 백테스트
ep = endpoints(rets, on = 'months')
wts = list()
lookback = 12
wt_zero = rep(0, 10) %>% setNames(colnames(rets))

for (i in (lookback+1) : length(ep)) {
  sub_ret = rets[ep[i-lookback] : ep[i] , ]
  cum = Return.cumulative(sub_ret)
  
  K = rank(-cum) <= 5
  covmat = cov(sub_ret[, K])
  
  wt = wt_zero
  wt[K] = optimalPortfolio(covmat,
                           control = list(type = 'minvol',
                                          constraint = 'user',
                                          LB = rep(0.10, 5),
                                          UB = rep(0.30, 5)))
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)

GDAA = Return.portfolio(rets, wts, verbose = TRUE)
charts.PerformanceSummary(GDAA$returns, main = '동적자산배분')

wts %>% fortify.zoo() %>%
  gather(key, value, -Index) %>%
  mutate(Index = as.Date(Index)) %>%
  mutate(key = factor(key, levels = unique(key))) %>%
  ggplot(aes(x = Index, y = value)) +
  geom_area(aes(color = key, fill = key),
            position = 'stack') +
  xlab(NULL) + ylab(NULL) +  theme_bw() +
  scale_x_date(date_breaks="years", date_labels="%Y",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(byrow = TRUE))

GDAA$turnover = xts(
  rowSums(abs(GDAA$BOP.Weight -
                timeSeries::lag(GDAA$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(GDAA$BOP.Weight))

chart.TimeSeries(GDAA$turnover)

fee = 0.0030
GDAA$net = GDAA$returns - GDAA$turnover*fee

cbind(GDAA$returns, GDAA$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')


#### 기존 동적자산배분 + 멀티팩터 포트폴리오-----------------------

port <- portfolio$returns
Sys.timezone()
Sys.timezone(index(rets[1]))
index(port) <- date(gsub(" KST", "", index(port)))
function(x) {ymd(index(port)[x], tz='Asia/Seoul') %>%
  with_tz('UTC') %>%
  as.Date(.)}
a = index(rets)
glimpse(a)
tail(a)
tail(index(port))
glimpse(index(port))
names(unclass(a[1]))
lapply( )
index(port)
index(port) <- date(index(port)+1)
rets_up <- cbind(rets, port) %>% na.omit()
rets_up <- rets_up[-520,]
nrow(rets_up)
tail(rets_up)

ep = endpoints(rets_up, on = 'months')
wts = list()
lookback = 12
wt_zero = rep(0, 11) %>% setNames(colnames(rets_up))

for (i in (lookback+1) : length(ep)) {
  sub_ret = rets_up[ep[i-lookback] : ep[i] , ]
  cum = Return.cumulative(sub_ret)
  
  K = rank(-cum) <= 5
  covmat = cov(sub_ret[, K])
  
  wt = wt_zero
  wt[K] = optimalPortfolio(covmat,
                           control = list(type = 'minvol',
                                          constraint = 'user',
                                          LB = rep(0.10, 5),
                                          UB = rep(0.30, 5)))
  
  wts[[i]] = xts(t(wt), order.by = index(rets_up[ep[i]]))
}

wts = do.call(rbind, wts)

GDAA_up = Return.portfolio(rets_up, wts, verbose = TRUE)
charts.PerformanceSummary(GDAA_up$returns, main = '동적자산배분')

wts %>% fortify.zoo() %>%
  gather(key, value, -Index) %>%
  mutate(Index = as.Date(Index)) %>%
  mutate(key = factor(key, levels = unique(key))) %>%
  ggplot(aes(x = Index, y = value)) +
  geom_area(aes(color = key, fill = key),
            position = 'stack') +
  xlab(NULL) + ylab(NULL) +  theme_bw() +
  scale_x_date(date_breaks="months", date_labels="%Y-%m",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(byrow = TRUE))

GDAA_up$turnover = xts(
  rowSums(abs(GDAA_up$BOP.Weight -
                timeSeries::lag(GDAA_up$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(GDAA_up$BOP.Weight))

chart.TimeSeries(GDAA_up$turnover)

fee = 0.0030
GDAA_up$net = GDAA_up$returns - GDAA_up$turnover*fee

cbind(GDAA_up$returns, GDAA_up$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA+port')

cbind(GDAA_up$returns, GDAA$returns) %>%
  setNames(c('GDAA+멀티팩터', '기존 GDAA')) %>%
  charts.PerformanceSummary(main = '수익률 비교')

library(PerformanceAnalytics)
chart.CumReturns(GDAA_up$returns)

table.Drawdowns(GDAA_up$returns)
maxDrawdown(GDAA_up$returns)
chart.Drawdown(GDAA_up$returns)

library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

symbols = c('SPY', 'SHY')
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x))))
rets = na.omit(Return.calculate(prices))

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x))))
rets = na.omit(Return.calculate(prices))
ep = endpoints(rets, on = 'months')

print(ep)
wts = list()
lookback = 10

i = lookback + 1
sub_price = prices[ep[i-lookback] : ep[i] , 1]

head(sub_price, 3)

tail(sub_price, 3)

sma = mean(sub_price)

wt = rep(0, 2)
wt[1] = ifelse(last(sub_price) > sma, 1, 0)
wt[2] = 1 - wt[1]

wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))

ep = endpoints(rets, on = 'months')
wts = list()
lookback = 10

for (i in (lookback+1) : length(ep)) {
  sub_price = prices[ep[i-lookback] : ep[i] , 1]
  sma = mean(sub_price)
  wt = rep(0, 2)
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)
  wt[2] = 1 - wt[1]
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)

Tactical = Return.portfolio(rets, wts, verbose = TRUE)
portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>%
  setNames(c('매수 후 보유', '시점 선택 전략'))

charts.PerformanceSummary(portfolios,
                          main = "Buy & Hold vs Tactical")

turnover = xts(rowSums(abs(Tactical$BOP.Weight -
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(turnover)

######GDAA----------
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

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

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

portfolios = na.omit(cbind(GDAA$returns, Tactical$returns)) %>%
  setNames(c('동적 자산 배분', '시점 선택 전략'))

charts.PerformanceSummary(portfolios, main = "시점선택 VS 동적자산배분")

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

######Tactical vs GDAA ---------
Tactical$turnover = xts(
  rowSums(abs(Tactical$BOP.Weight -
                timeSeries::lag(Tactical$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(Tactical$turnover)

fee = 0.0030
Tactical$net = Tactical$returns - Tactical$turnover*fee

cbind(Tactical$returns, Tactical$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'Tactical')

cbind(Tactical$net, GDAA$net) %>%
  setNames(c('Tactical Fee', 'GDAA Fee')) %>%
  charts.PerformanceSummary(main = ' Tactical vs GDAA')

### 성과분석--------
library(PerformanceAnalytics)
## 함수 없이 구해보기
# 수익률
chart.CumReturns(Tactical$returns)
# 누적 수익률
prod((1+Tactical$returns)) - 1 
# 연율화 수익률(산술)
mean(Tactical$returns) * 252 
# 연율화 수익률(기하)
(prod((1+Tactical$returns)))^(252 / nrow(Tactical$returns)) - 1 

# 각 함수
# 누적수익률
Return.cumulative(Tactical$returns)
# 연율화 수익률(산술)
Return.annualized(Tactical$returns, geometric = F)
# 연율화 수익률 기하
Return.annualized(Tactical$returns)

# 연율화 변동성(수식)
sd(Tactical$returns) * sqrt(252)
# 연율화 변동성(함수)
StdDev.annualized(Tactical$returns)

#SharpeRatio.annualized(Tactical$returns, Rf = df$RF, geometric = TRUE)

#### 낙폭과 최대낙폭
table.Drawdowns(Tactical$returns)
maxDrawdown(Tactical$returns)
chart.Drawdown(Tactical$returns)
# 칼마지수(연율화 수익률을 최대 낙폭으로 나눈 값)
# 절대 수익률을 추구하는 헤지펀드에서 많이 참조하는 지표
CalmarRatio(Tactical$returns)


#### 연도 별 수익률
apply.yearly(Tactical$returns, Return.cumulative) %>% head()

library(lubridate)
library(tidyr)
library(ggplot2)

R.yr = apply.yearly(Tactical$returns, Return.cumulative) %>%
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

####승률 및 롤링 윈도우 값

## 승률: 포트폴리오가 벤치마크 대비 높은 성과를 기록한 기간 비율
# (포트폴리오>벤치마크) 일수 / 전체 기간
UpsideFrequency(Tactical$returns, MAR = 0)
# Mar 인자는 0이 기본값
# 원하는 벤치마크가 있을 시 이를 입력
'''
롤링 윈도우
 포트폴리오의 시작부터 지금까지 투자를 했다는 전제가 아닌
 무작위 시점에 투자했을 때 향후 n개월 후 승률 혹은 연율화 수익률 등을 계산
'''
roll_12 = Tactical$returns %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_24 = Tactical$returns %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 24, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_36 = Tactical$returns %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 36, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_win = cbind(roll_12, roll_24, roll_36)
print(roll_win)
'''
롤링 윈도우 승률은 무작위 시점에 투자했을 시 
미래 n개월 동안의 연율화 수익률을 구하고
해당 값이 벤치마크 대비 수익이 높았던 비율을 계산
만약 12개월 롤링 윈도우 승률이 100프로(1)라면 어떤 시점에 투자해도 12개월 
후에는 언제나 벤치마크를 이겼다는 의미
Tactical은 승률이 매우 높고 심지어 36개월은 1이다
'''
Tactical$returns %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  fortify.zoo() %>%
  ggplot(aes(x = Index, y = portfolio.returns)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = 'red') +
  xlab(NULL) + ylab(NULL)


####팩터회귀분석 및 테이블로 나타내기
'''
수익이 어디에서 발생했는가에 대한 요인 분석
일반적으로 많이 사용되는 모형: 
기존의 CAPM에 사이즈팩터(SMB), 밸류팩터(HML)를 추가한 3팩터 모형
'''
















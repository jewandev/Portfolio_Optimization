setwd('/Users/wan/GitHub/Portfolio_Optimization/time_series_clustering')
load('dtw_clustering.RData')

library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

krw = read.csv('data/krw_close.csv', row.names = 1,
               stringsAsFactors = FALSE) %>% as.xts()
glimpse(krw)
krw
(ksdq_next_period[[1]][[1]])
as.numeric(gsub('-','', ksdq_next_period[[1]][1])) #>= 201103
ksdq_next_period[[1]][1]

length(ksp_next_period)
ksp_bt <- list()
for(i in 1:length(ksp_next_period)){
  ksp_bt[[i]] <- ksp_next_period[[i]][as.numeric(gsub('-', '', ksp_next_period[[i]])) >= 201903]
}
ksp_bt
ksp_rets_mean

# 코스닥 백테스트 구간 추출
ksdq_bt <- list()
for(i in 1:length(ksdq_next_period)){
  ksdq_bt[[i]] <- ksdq_next_period[[i]][as.numeric(gsub('-', '', ksdq_next_period[[i]])) >= 201903]
}
ksdq_bt
ksdq_rets_mean
# 달러환율 백테스트 추출
usd_bt <- list()
for(i in 1:length(usd_next_period)){
  usd_bt[[i]] <- usd_next_period[[i]][as.numeric(gsub('-', '', usd_next_period[[i]])) >= 201903]
}
usd_bt
usd_rets_mean

### 멀티팩터 포틒로리오
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
# 예시
#substr(usd_bt[[1]][[1]], 6, 7)
as.numeric(str_sub(usd_bt[[1]][[1]], -2, -1))
as.numeric(str_sub(usd_bt[[1]][[1]], 1, 4))
if (month(ym(usd_bt[[1]][[1]])) %in% c(1,2,3,4) ) {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(ym(usd_bt[[1]][[1]])) - 2))
} else {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(ym(usd_bt[[1]][[1]])) - 1))
}
names(KOR_fs[[1]])
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

multifactor <- function(x, ...){
  ### 퀄리티 지표 계산
  if (month(ym(x)) %in% c(1,2,3,4) ) {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(ym(x)) - 2))
  } else {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(ym(x)) - 1))
  }
  
  # 자기자본이익률
  quality_roe = (KOR_fs$"지배주주순이익" / KOR_fs$"자본")[num_col]
  #매출총이익
  quality_gpa = (KOR_fs$"매출총이익" / KOR_fs$"자산")[num_col]
  # 영업활동현금흐름
  quality_cfo =
    (KOR_fs$"영업활동으로인한현금흐름" / KOR_fs$"자산")[num_col]
  
  quality_profit =
    cbind(quality_roe, quality_gpa, quality_cfo) %>%
    setNames(., c("ROE", "GPA", "CFO"))
  
  factor_quality = quality_profit %>%
    mutate_all(list(~min_rank(desc(.)))) %>%
    mutate_all(list(~scale(.))) %>%
    rowSums()
  
  factor_value = KOR_value %>%
    mutate_all(list(~min_rank(.))) %>%
    mutate_all(list(~scale(.))) %>%
    rowSums()
  
  return(list(quality_roe=quality_roe, 
         quality_gpa=quality_gpa, 
         quality_cfo=quality_cfo, 
         quality_profit=quality_profit, 
         value_profit=KOR_value,
         factor_quality=factor_quality,
         factor_value=factor_value))
}

ksp_bt
for(i in )
factors <- multifactor(ksp_bt[[1]][[1]])

head(factors$quality_roe)
head(as$quality_roe)
glimpse(as)
quality_profit
names(quality_roe)
quality_profit

ret_m <- Return.calculate(KOR_price[ksp_bt[[1]][[1]]])%>%
  xts::last(.) %>%
  sapply(., function(x) {prod(1+x) - 1})

ret_bind <- cbind(ret_m) %>% data.frame()
factor_mom <- ret_bind %>%
  mutate_all(list(~min_rank(desc(.))))
factor_mom <- ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()


invest_mom = rank(factor_mom) <= 20

ranking <- data.frame(cbind("ticker" = KOR_ticker[,1][invest_mom],
                 "corp" = KOR_ticker[,2][invest_mom])) 
ranking['ticker']
ranking
qual <- quality_profit[rownames(quality_profit) %in% ranking[,'ticker'],]
val <- KOR_value[rownames(KOR_value) %in% as.numeric(ranking[,'ticker']),]
cbind(qual, val)[,1]
### 구간 내 지표 평균 값
cbind(qual, val) %>% 
  apply(., 2, mean, na.rm = T) %>% round(3) %>% t()

KOR_price[ksp_bt[[1]][[1]]][,1]
ret_3m[1]
ret_3m <- na.omit(ret_3m)
ret_3m
substr(ym(usd_bt[[1]][[1]]), 1, 7)
rows <- 
Return.calculate(KOR_price[])


### 시점 선택 전략

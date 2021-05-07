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

parse_number(ksdq_next_period[[1]])
ksdq_next_period[[1]][parse_number(ksdq_next_period[[1]]) >= 201903]

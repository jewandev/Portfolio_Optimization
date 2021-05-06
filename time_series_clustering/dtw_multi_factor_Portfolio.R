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


setwd('/Users/wan/GitHub/Portfolio_Optimization/R')
#
# 패키지 이 중에 없는것만 깔기 ----
pkg <- c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries')
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)}

# 예외처리 ----
number <- data.frame(1,2,3,"4",5, stringsAsFactors = FALSE)
str(number)
for (i in number) {
  tryCatch({
    print(i^2)
  }, error = function(e) {
    print(paste('Error:', i))
  })
}

# API 주소를 이용한 Quandl 데이터 다운로드 ----
url.aapl <- "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl <- read.csv(url.aapl)
head(data.aapl)

# getSymbols() 함수를 이용한 API 다운로드----
# API 주소 알면 간단하게 데이터 수집 가능 
# - 원하는 항목에 대한 API 일일이 얻기 힘듦 -> 전 종목 데이터 구하기 사실상 불가능
library(quantmod)
getSymbols('AAPL')
head(AAPL)

chart_Series(Ad(AAPL)) # 수정종가만 차트로 그리기

# 원하는 기간동안의 데이터 다운로드 받기----
data <- getSymbols('AAPL',
                  from = '2000-01-01', to = '2018-12-31',
                  auto.assign = FALSE)
# 따로 설정 안하면 기간 1년으로 다운
# auto.assign을 False로 하면 원하는 변수에 저장 가능

head(data)
ticker <- c('FB', 'NVDA')

getSymbols(ticker)
head(FB)
head(NVDA)

# 국내 종목 주가 다운로드----
# 코스피상장 = 티커.KS / 코스닥상장 = 티커.KQ
getSymbols('005930.KS',
           from = '2000-01-01', to = '2018-12-31')
tail(Ad(`005930.KS`)) # 국내 종목은 수정종가에 오류발생 많음
tail(Cl(`005930.KS`)) # 그래서 단순종가 권장

getSymbols('068760.KQ',
           from = '2000-01-01', to = '2018-12-31')
tail(Cl(`068760.KQ`))

# FRED 데이터 다운로드----
getSymbols('DGS10', src = 'FRED') # 미 국채 10년물 금리
chart_Series(DGS10)
# 각 한목별 티거 찾는 법: 
# - FRED 웹사이트에서 원하는 데이터 검색 
# ex) 원/달러 환율 검색
# - 이 중 페이지 주소에서 /series/ 다음에 DEXKOUS가 해당 티커
getSymbols('DEXKOUS', src='FRED')
tail(DEXKOUS)

# 금융 속보 크롤링(GET) -----
library(rvest)
library(httr)

url <- 'https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258'
data = GET(url)

print(data)

data_title <- data %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes('dl') %>%
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_attr('title')
data_title

# 기업공시채널에서 오늘의 공시 불러오기(POST)----
library(httr)
library(rvest)

# Sys.setlocale("LC_ALL", "English")
# 한글로 작성된 페이지 크롤링하면 오류가 발생 경우 있어서 영어로 설정
url <- 'https://kind.krx.co.kr/disclosure/todaydisclosure.do'
data <- POST(url, body = 
               list(
                 method = 'searchTodayDisclosureSub',
                 currentPageSize = '15',
                 pageIndex = '1',
                 orderMode = '0',
                 orderStat = 'D',
                 forward = 'todaydisclosure_sub',
                 chose = 'S',
                 todayFlag = 'N',
                 selDate = '2018-12-28'
                 ))
# POST() 함수로 url에 원하는 쿼리 요청하는 것 
# 1. 쿼리는 body 내에 리스트 형태로 입력
# 2. 해당 값은 개발자 도구 화면의 Form Data와 동일하게 입력
# 3. 값이 없는 항목은 입력 안해도 됨

data <- read_html(data) %>% # 해당 페이지의 HTML 내용 읽어오기
  html_table(header = FALSE, fill = TRUE) %>% # 테이블 형태의 데이터 읽어오기(셀 병합된 열이 있으므로 fill=TRUE 추가)
  .[[1]] # 첫번째 리스트 선택

# Sys.setlocale("LC_ALL", "Korean") # 한글 읽기 위해 Korean으로 변경

print(head(data))
# 날짜를 바꿔는건 selDate만 변경해주면 됨

# 네이버 금융에서 주식티커 크롤링 ----
# 코스피: https://finance.naver.com/sise/sise_market_sum.nhn?sosok=0&page=1
# 종목 클릭해보면 url 끝 6자리가 거래소 티커임을 확인할 수 있음
# 시가총액에 해당하는 페이지가 몇쪽까지 있는지 확인(맨뒤)
library(httr)
library(rvest)

i = 0
ticker = list()
url = paste0('https://finance.naver.com/sise/',
             'sise_market_sum.nhn?sosok=',i,'&page=1')
# i= 0 -> 코스피 / i=1 -> 코스닥 에 해당하는 url 생성
down_table = GET(url)
# 마지막 페이지가 몇 번째인지 찾아내는 작업
# - 하단 마지막 버튼 검사하면 나오는 속성 입력
navi.final = read_html(down_table, encoding = 'EUC-KR') %>%
  html_nodes(., '.pgRR') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')
# 1. read_html()로 html 내용 읽어오고 euc-kr로 설정
# 2. html_nodes()로 pgRR 클래스 정보 불러오기
# 3. html_nodes()로 a태그 정보
# 4. html_attr()로 href 속성 불러오기
print(navi.final) # 이 중에 알고 싶은 내용(마지막 페이지) = 32

navi.final = navi.final %>%
  strsplit(., '=') %>%
  unlist() %>%
  tail(., 1) %>%
  as.numeric()
# 1. strsplit()로 page= 뒷부분의 데이터를 가져오기 위해 '=' 기준으로 분할
# 2. unlist()로 결과를 벡터 형태로 변환
# 3. tail()로 뒤에서 첫번째 데이터만 선택
# 4. as.numeric()로 해당 값을 숫자로 바꿔줌
print(navi.final)

i = 0 # 코스피
j = 1 # 첫 번째 페이지
url <- paste0('https://finance.naver.com/sise/',
             'sise_market_sum.nhn?sosok=', i, "&page=", j)
down_table <- GET(url) # GET()로 페이지 데이터 다운로드
down_table

table = read_html(down_table, encoding = "EUC-KR") %>%
  html_table(fill = TRUE)
table = table[[2]]
print(head(table))

table[, ncol(table)] <- NULL
# 토론실 정보는 필요 없으니 삭제
table <- na.omit(table)
# 1행처럼 na인 행 삭제
print(head(table))

# ticker 추출
symbol <- read_html(down_table, encoding='EUC-KR') %>%
  html_nodes(., 'tbody') %>%
  html_nodes(., 'tr') %>%
  html_nodes(., 'td') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')
head(symbol)

library(stringr)
symbol <- sapply(symbol, function(x){
  str_sub(x, -6, -1)
  })
head(symbol)
# sapply로 symbol 변수에 str_sub로 마지막 6글자 추출
symbol <- unique(symbol) # 중복된 값 삭제

table$N <- symbol
colnames(table)[1] <- '종목코드'
rownames(table) <- NULL # 위에서 특정 행 삭제해서 행 이름 초기화
ticker[[j]] <- table # 티커 리스트에 삽입

# for loop 이용해서 모든 테이블 생성하기
data = list()

# i = 0 은 코스피, i = 1 은 코스닥 종목
for (i in 0:1) {
  
  ticker <- list()
  url <-
    paste0('https://finance.naver.com/sise/',
           'sise_market_sum.nhn?sosok=',i,'&page=1')
  
  down_table <- GET(url)
  
  # 최종 페이지 번호 찾아주기
  navi.final <- read_html(down_table, encoding = "EUC-KR") %>%
    html_nodes(., ".pgRR") %>%
    html_nodes(., "a") %>%
    html_attr(.,"href") %>%
    strsplit(., "=") %>%
    unlist() %>%
    tail(., 1) %>%
    as.numeric()
  
  # 첫번째 부터 마지막 페이지까지 for loop를 이용하여 테이블 추출하기
  for (j in 1:navi.final) {
    
    # 각 페이지에 해당하는 url 생성
    url <- paste0(
      'https://finance.naver.com/sise/',
      'sise_market_sum.nhn?sosok=',i,"&page=",j)
    down_table <- GET(url)
    
    # 한글 오류 방지를 위해 영어로 로케일 언어 변경
    
    table <- read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    table <- table[[2]] # 원하는 테이블 추출
    
    # 한글을 읽기위해 로케일 언어 재변경
    
    table[, ncol(table)] = NULL # 토론식 부분 삭제
    table <- na.omit(table) # 빈 행 삭제
    
    # 6자리 티커만 추출
    symbol <- read_html(down_table, encoding = "EUC-KR") %>%
      html_nodes(., "tbody") %>%
      html_nodes(., "td") %>%
      html_nodes(., "a") %>%
      html_attr(., "href")
    
    symbol <- sapply(symbol, function(x) {
      str_sub(x, -6, -1) 
    })
    
    symbol <- unique(symbol)
    
    # 테이블에 티커 넣어준 후, 테이블 정리
    table$N <- symbol
    colnames(table)[1] <- "종목코드"
    
    rownames(table) <- NULL
    ticker[[j]] <- table
    
    Sys.sleep(0.5) # 페이지 당 0.5초의 슬립 적용
  }
  
  # do.call을 통해 리스트를 데이터 프레임으로 묶기
  ticker <- do.call(rbind, ticker)
  data[[i + 1]] <- ticker
}

# 코스피와 코스닥 테이블 묶기
data <- do.call(rbind, data)
data


# (기본) 한국거래소의 산업별 현화 및 개발지표 크롤링----
# 업종 분류 현황 크롤링
library(httr)
library(rvest)
library(readr)

gen_otp_url <- 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'

gen_otp_data <- list(
  mktId = 'STK', # 코스피를 뜻 함.(코스닥하고싶으면 KSQ)
  trdDd = '20210319',
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp <- POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text() # OTP값만 추출

down_url <- 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS <- POST(down_url, query = list(code = otp),
                       add_headers(referer = gen_otp_url)) %>%
                     # add_header는 referer를 추가해야 함.
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

print(down_sector_KS)

# 코스닥
gen_otp_data <- list(
  mktId = 'KSQ', # 코스닥으로 변경
  trdDd = '20210108',
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp <- POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()
down_sector_KQ <- POST(down_url, query = list(code=otp),
                       add_headers(regerer=gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv

down_sector <- rbind(down_sector_KS, down_sector_KQ)

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv') # 깨짐

# 개별 종목 지표 크롤링
library(httr)
library(rvest)
library(readr)

gen_otp_url <-
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = '20210108',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp <- POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url <- 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind <- POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
print(down_ind)

#write.csv(down_ind, 'data/krx_ind.csv') 깨짐

# 최근 영업일 기준 데이터 받기
library(httr)
library(rvest)
library(stringr)

url <- 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day <- GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>% # xpath지점 데이터 추출
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>% # 정규식으로 숫자.숫자.숫자 형식의 데이터 추출
  str_replace_all('\\.', '') # 마침표 없애주기

print(biz_day)

# 산업별 현황&개별종목 지표를 최근일자 기준으로 다운 코드
library(httr)
library(rvest)
library(stringr)
library(readr)

# 최근 영업일 구하기
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

# 코스피 업종분류 OTP 발급
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  mktId = 'STK',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스피 업종분류 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

# 코스닥 업종분류 OTP 발급
gen_otp_data = list(
  mktId = 'KSQ',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스닥 업종분류 데이터 다운로드
down_sector_KQ = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

down_sector = rbind(down_sector_KS, down_sector_KQ)

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

# 개별종목 지표 OTP 발급
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = biz_day, # 최근영업일로 변경
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 개별종목 지표 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')

# 거래소 데이터 정리하기

# 위 방법으로는 중복된 열도 있고 불필요한 데이터 있음.
# 따라서 합칠 필요가 있음
down_sector <- read.csv('data/krx_sector.csv', row.names = 1,
                        stringsAsFactors = FALSE)
down_ind <- read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)
# row.names=1로 첫번째 열을 행 이름으로 지정
# stringAsFactor=FALSE로 문자열이 팩터형으로 안되게 하기

down_sector # 산업 현황
down_ind # 개별종목 지표

intersect(names(down_sector), names(down_ind))
# 두 데이터 간 중복되는 열 이름 확인

setdiff(down_sector[, '종목명'], down_ind[, '종목명'])
# 두 데이터에 한 번만 나와 있는 종목

KOR_ticker <- merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE
)
# merge()에서 by를 기준으로 두 게이터 합치기
# 공통으로 존재하는 열 기주능로 입력
# all 값을 true로 하면 합집합, flase로 하면 교집합

KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액']), ] 
# order() 앞에 - 붙여서 시가총액 기준으로 내림차순
print(head(KOR_ticker))

library(stringr)
KOR_ticker <- KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ] 
# '스팩' 들어간 종목 찾기
KOR_ticker <- KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]
# str_sub() 함수로 종목코드 끝이 0이 아닌 우선주 종목 찾기
rownames(KOR_ticker) <- NULL # 행 이름 초기화
write.csv(KOR_ticker, 'data/KOR_ticker.csv')

# (기본) WICS 기준 섹터 정보 크롤링----

# 최근 영업일 구하기
library(httr)
library(rvest)
library(stringr)
library(readr)
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

library(jsonlite)

url <- 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20190607&sec_cd=G10'
data <- fromJSON(url)

lapply(data, head)

sector_code <- c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector <- list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector <- do.call(rbind, data_sector)

write.csv(data_sector, 'data/KOR_sector.csv')

# (심화) 수정주가 크롤링----

# 개별종목 주가 크롤링
library(stringr)

KOR_ticker <- read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])

KOR_ticker$'종목코드' <-
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')
# str_pad()로 6자리가 되지 않는 문자는 왼쪽에 0을 추가해 6자리로 만들어줌

library(xts)

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

i <- 1
name <- KOR_ticker$'종목코드'[i]

price <- xts(NA, order.by = Sys.Date()) # 현재 날짜 입력
print(price)

library(httr)
library(rvest)
library(lubridate)
library(stringr)
library(readr)

from <- (Sys.Date() - years(3)) %>% str_remove_all('-')
to <- Sys.Date() %>% str_remove_all('-')

url <- paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=', name,
             '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')

data <- GET(url)
data_html <- data %>% 
  read_html %>%
  html_text() %>%
  read_csv()

print(data_html)

library(timetk)
price <- data_html[c(1, 5)]
colnames(price) <- (c('Date', 'Price'))
price <- na.omit(price)
price$Date <- parse_number(price$Date) # 열에서 숫자만 추출
price$Date <- ymd(price$Date) # ymd()로 yyyymmdd 형태를 yyyy-mm-dd로 변경
price <- tk_xts(price, date_var = Date) # tk_xts()로 시계열로 변경, 인덱스: price$Date
print(tail(price))

write.csv(data.frame(price), # data.frame으로 설정해서 시계열 인덱스 안 사라지게 함.
          paste0('data/KOR_price/', name, '_price.csv'))

# 전 종목 주가 크롤링
library(httr)
library(rvest)
library(stringr)
library(xts)
library(lubridate)
library(readr)
library(timetk)

KOR_ticker <- read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])
KOR_ticker$'종목코드' <-
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

for(i in 1 : nrow(KOR_ticker) ) {
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  price <- xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name <- KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  from <- (Sys.Date() - years(3)) %>% str_remove_all('-') # 시작일
  to <- Sys.Date() %>% str_remove_all('-') # 종료일
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url <- paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=', name,
                 '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')
    
    # 이 후 과정은 위와 동일함
    # 데이터 다운로드
    data <- GET(url)
    data_html <- data %>% read_html %>%
      html_text() %>%
      read_csv()
    
    # 필요한 열만 선택 후 클렌징
    price <- data_html[c(1, 5)]
    colnames(price) <- (c('Date', 'Price'))
    price <- na.omit(price)
    price$Date <- parse_number(price$Date)
    price$Date <- ymd(price$Date)
    price <- tk_xts(price, date_var = Date)
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 폴더 내 csv 파일로 저장
  write.csv(data.frame(price),
            paste0('data/KOR_price/', name, '_price.csv'))
  
  # 타임슬립 적용
  Sys.sleep(1)
}

# (심화) 재무제표 및 가치지표 크롤링----

# 재무제표 다운로드
# http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930
# 위 사이트에서 재무제표 항목을 모두 테이블 형태로 제공(html_table() 사용)
library(httr)
library(rvest)

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))

url <- paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

data <- GET(url,
            user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2866.71 Safari/537.36'))
# 위는 맥용 유저 에이전트
# http://www.useragentstring.com/pages/useragentstring.php 에서 os별 에이전트 확인 가능

data <- data %>%
  read_html() %>%
  html_table()

lapply(data, function(x){
  head(x, 3)})
# data 리스트에 들어온 6개 테이블
# 포괄손익계산서(연간) / 포괄손익계산서(분기) / 재무상태표(연간) / 재무상태표(분기) / 현금흐름표(연간) / 현금흐름포(분기)
# 연간으로 선택 : 1, 3, 5번째
data_IS <- data[[1]]
data_BS <- data[[3]]
data_CF <- data[[5]]
print(names(data_IS))
data_IS <- data_IS[, 1:(ncol(data_IS)-2)] 
# 포괄손익게산서 테이블(data_IS)의 전년동기, 전년동기(%)열은 통일성을 위해 삭제

# 테이블 묶고 클렌징
data_fs_ex <- rbind(data_IS, data_BS, data_CF)
data_fs_ex[, 1] <- apply(data_fs_ex[, 1], 2, function(x) gsub('계산에 참여한 계정 펼치기', '', x))
# 계산 어쩌구는 상관이 없는 내용이므로 빈칸으로 바꾸기

data_fs_ex <- data_fs_ex[!duplicated(data_fs_ex[, 1]), ]
# 중복되지 않는 계정명만 선택
rownames(data_fs_ex) <- NULL
data_fs <- data_fs_ex
data_fs <- data_fs[, substr(colnames(data_fs), 6, 7) == '12']
# substr()로 끝 2 글자가 '12'인 열만 선택
rownames(data_fs) <- data_fs_ex$`IFRS(연결)`
print(head(data_fs))

sapply(data_fs, typeof)

library(stringr)

data_fs <- sapply(data_fs, function(x){
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))

print(head(data_fs))

sapply(data_fs, typeof)

write.csv(data_fs, 'data/Kor_fs/005930_fs.csv')

# 가치지표 계산하기----
# 위에서 이어짐
# PER 분모 : Earning (순이익)
# PBR 분모 : Book Value (순자산)
# PCR 분모 : Cashflow (영업활동현금흐름)
# PSR 분모 : Sales (매출액)

# 이것들의 분자는 주가, 분모는 재무제표 데이터.

ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

value_type = c('지배주주순이익',
               '자본',
               '영업활동으로인한현금흐름',
               '매출액')

value_index = data_fs[match(value_type, rownames(data_fs)),
                      ncol(data_fs)]
# match()로 value_type으로 정한 항목이 위치한 지점 찾음.
# ncol()로 맨 오른쪽, 가장 최근년도 데이터 선택
print(value_index)
# http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930
# 위에서 주가 xpath
# //*[@id="svdMainChartTxt11"]

library(readr)

url = 'http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930'
data = GET(url,
           user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2866.71 Safari/537.36'))

price = read_html(data) %>%
  html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
  html_text() %>%
  parse_number()

print(price)

# PER = Price/EPS = 주가/주당순이익
# 주당순이익: 순이익을 전체 주식 수로 나눈 값.
# 전체 주식 수를 구해야 함.
# 주식 수 xpath: //*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]
share <- read_html(data) %>%
  html_node(
    xpath = '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
  html_text()

print(share)

share <- share %>%
  strsplit('/') %>%
  unlist() %>% # list를 벡터 형태로 변환
  .[1] %>% # 먼저 나온 수 고르는건데 0 아니고 1임
  parse_number()

print(share)

data_value <- price / (value_index * 100000000 / share) 
# 주가는 원 단위, 재무 데이터는 억원 단위이므로 분모에 억 곱함

names(data_value) <- c('PER', 'PBR', 'PCR', 'PSR')
data_value[data_value < 0] <- NA
# 가치지표가 음수인 경우에는 NA로 변경
print(data_value)

write.csv(data_value, 'data/KOR_value/005930_value.csv')

# 전 종목 재무제표 및 가치지표 다운로드----
library(stringr)
library(httr)
library(rvest)
library(stringr)
library(readr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

for(i in 1 : nrow(KOR_ticker) ) {
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    
    #Sys.setlocale('LC_ALL', 'English')
    
    # url 생성
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    
    # 이 후 과정은 위와 동일함
    
    # 데이터 다운로드 후 테이블 추출
    data = GET(url,
               user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2866.71 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    
    #Sys.setlocale('LC_ALL', 'Korean')
    
    # 3개 재무제표를 하나로 합치기
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs_ex <- rbind(data_IS, data_BS, data_CF)
    data_fs_ex[, 1] <- apply(data_fs_ex[, 1], 2, function(x) gsub('계산에 참여한 계정 펼치기', '', x))
    
    data_fs_ex <- data_fs_ex[!duplicated(data_fs_ex[, 1]), ]
    # 중복되지 않는 계정명만 선택
    rownames(data_fs_ex) <- NULL
    data_fs <- data_fs_ex
    data_fs <- data_fs[, substr(colnames(data_fs), 6, 7) == '12']
    # substr()로 끝 2 글자가 '12'인 열만 선택
    rownames(data_fs) <- data_fs_ex$`IFRS(연결)`
    
    # 여기는 안 되는 구간임 내가 한게 더 잘 돌아감
    #data_fs = rbind(data_IS, data_BS, data_CF)
    
    # 데이터 클랜징
    #data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
    #                    '', data_fs[, 1])
    #data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    
    #rownames(data_fs) = NULL
    #rownames(data_fs) = data_fs[, 1]
    #data_fs[, 1] = NULL
    
    # 12월 재무제표만 선택
    #data_fs =
    #  data_fs[, substr(colnames(data_fs), 6,7) == "12"]
    
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    
    
    # 가치지표 분모부분
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    
    # 해당 재무데이터만 선택
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)-1]
    
    # Snapshot 페이지 불러오기
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2866.71 Safari/537.36'))
    
    # 현재 주가 크롤링
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    
    # 보통주 발행주식수 크롤링
    share = read_html(data) %>%
      html_node(
        xpath =
          '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    
    # 가치지표 계산
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(data_fs, paste0('data/KOR_fs/', name, '_fs.csv'))
  
  # 가치지표 저장
  write.csv(data_value, paste0('data/KOR_value/', name,
                               '_value.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(1)
}

# DART Open API를 이용한 데이터 수집하기----
# 하루 10000번까지 요청 가능
file.edit('~/.Renviron')
dart_api = Sys.getenv("dart_api_key")

library(httr)
library(rvest)

codezip_url = paste0(
  'https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=',dart_api)

codezip_data = GET(codezip_url)
print(codezip_data)

codezip_data$headers[["content-disposition"]]
# content-disposition을 확인해보면 압축파일이 첨부돼있음

# 압축파일 풀기
tf = tempfile(fileext = '.zip')
# 빈 .zip 파일 생성

writeBin( # 바이너리 평태의 파일 저장하는 함수
  content(codezip_data, as = "raw"), 
  # content() 함수로 첨부파일 내용을 raw 형태로 저장
  file.path(tf)
)

nm = unzip(tf, list = TRUE) # unzip()로 zip 내 파일 리스트 확인
print(nm)

library(xml2)
code_data = read_xml(unzip(tf, nm$Name))
print(code_data)
# 파일은 HTML 형식
# corp_code: 고유번호
# corp_name: 종목명
# corp_stock: 거래소 상장 티커

# HTML 태그를 이용해 각 부분을 추출한 후 하나의 데이터로 합치기
corp_code = code_data %>% html_nodes('corp_code') %>% html_text()
corp_name = code_data %>% html_nodes('corp_name') %>% html_text()
corp_stock = code_data %>% html_nodes('stock_code') %>% html_text()

corp_list = data.frame(
  'code' = corp_code,
  'name' = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = FALSE
)
 ####### 다음은 나중에 공부하기
nrow(corp_list)
head(corp_list)
corp_list = corp_list[corp_list$stock != " ", ]

write.csv(corp_list, 'data/corp_list.csv')

# 전체 공시 검색
# https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS001&apiId=2019001
library(lubridate)
library(stringr)
library(jsonlite)

bgn_date = (Sys.Date() - days(7)) %>% str_remove_all('-')
end_date = (Sys.Date() ) %>% str_remove_all('-')
notice_url = paste0('https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,'&bgn_de=',
                    bgn_date,'&end_de=',end_date,'&page_no=1&page_count=100')
notice_data = fromJSON(notice_url) 
notice_data = notice_data[['list']]

head(notice_data)
# 특정 기업 공시 검색
bgn_date = (Sys.Date() - days(30)) %>% str_remove_all('-')
end_date = (Sys.Date() ) %>% str_remove_all('-')
corp_code = '00126380'

notice_url_ss = paste0(
  'https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,
  '&corp_code=', corp_code, 
  '&bgn_de=', bgn_date,'&end_de=',
  end_date,'&page_no=1&page_count=100')
notice_data_ss = fromJSON(notice_url_ss) 
notice_data_ss = notice_data_ss[['list']]

head(notice_data_ss)

notice_url_exam = notice_data_ss[1, 'rcept_no']
notice_dart_url = paste0(
  'http://dart.fss.or.kr/dsaf001/main.do?rcpNo=',notice_url_exam)

print(notice_dart_url)

# 사업보고서 주요 정보
# https://opendart.fss.or.kr/guide/main.do?apiGrpCd=DS002
corp_code = '00126380'
bsns_year = '2019'
reprt_code = '11011'

url_div = paste0('https://opendart.fss.or.kr/api/alotMatter.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', corp_code,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code
)
div_data_ss = fromJSON(url_div) 
div_data_ss = div_data_ss[['list']]

head(div_data_ss)
# 상장기업 재무 정보
# 단일회사 주요계정: https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019016
# 다중회사 주요계정: https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019017

corp_code = '00126380'
bsns_year = '2019'
reprt_code = '11011'

url_single = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_single = fromJSON(url_single) 
fs_data_single = fs_data_single[['list']]

head(fs_data_single)

corp_code = c('00126380,00413046,00190321')
bsns_year = '2019'
reprt_code = '11011'

url_multiple = paste0(
  'https://opendart.fss.or.kr/api/fnlttMultiAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_multiple = fromJSON(url_multiple) 
fs_data_multiple = fs_data_multiple[['list']]

fs_data_list = fs_data_multiple %>% split(f = .$corp_code)

lapply(fs_data_list, head, 2)

corp_code = '00126380'
bsns_year = 2019
reprt_code = '11011'

url_fs_all = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code,'&fs_div=CFS'
)

fs_data_all = fromJSON(url_fs_all) 
fs_data_all = fs_data_all[['list']]

head(fs_data_all)

yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
yr_name = seq(bsns_year, (bsns_year - yr_count + 1))

fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
  cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])

colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name

head(fs_data_all)


# 전 종목 전체 재무제표 데이터 수집하기
library(stringr)
library(dplyr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
corp_list =  read.csv('data/corp_list.csv', row.names = 1)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

corp_list$'code' =
  str_pad(corp_list$'code', 8, side = c('left'), pad = '0')

corp_list$'stock' =
  str_pad(corp_list$'stock', 6, side = c('left'), pad = '0')

ticker_list = KOR_ticker %>% left_join(corp_list, by = c('종목코드' = 'stock')) %>%
  select('종목코드', '종목명', 'code')

ifelse(dir.exists('data/dart_fs'), FALSE, dir.create('data/dart_fs'))

bsns_year = 2019
reprt_code = '11011'

for(i in 1 : nrow(ticker_list) ) {
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  data_fs = c()
  name = ticker_list$code[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  
  tryCatch({
    
    # url 생성
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', name,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code,'&fs_div=CFS'
    )
    
    # JSON 다운로드
    fs_data_all = fromJSON(url) 
    fs_data_all = fs_data_all[['list']]
    
    # 만일 연결재무제표 없어서 NULL 반환시
    # reprt_code를 OFS 즉 재무제표 다운로드
    if (is.null(fs_data_all)) {
      
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                   dart_api, 
                   '&corp_code=', name,
                   '&bsns_year=', bsns_year,
                   '&reprt_code=', reprt_code,'&fs_div=OFS'
      )
      
      fs_data_all = fromJSON(url) 
      fs_data_all = fs_data_all[['list']]
      
    }
    
    
    # 데이터 선택 후 열이름을 연도로 변경
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year, (bsns_year - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
      cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(fs_data_all, paste0('data/dart_fs/', ticker_list$종목코드[i], '_fs_dart.csv'))
  
  # 1초간 타임슬립 적용
  Sys.sleep(1)
}

# 데이터 정리하기 ----
# 주가 정리하기
library(stringr)
library(xts)
library(magrittr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

price_list = list()

for (i in 1 : nrow(KOR_ticker)) {
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  name = KOR_ticker[i, '종목코드']
  price_list[[i]] =
    read.csv(paste0('data/KOR_price/', name,
                    '_price.csv'),row.names = 1) %>%
    as.xts()
  
}

price_list = do.call(cbind, price_list) %>% na.locf()
colnames(price_list) = KOR_ticker$'종목코드'

head(price_list[, 1:5])
tail(price_list[, 1:5])
write.csv(data.frame(price_list), 'data/KOR_price.csv')

# 재무제표 정리하기
library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

data_fs = list()

for (i in 1 : nrow(KOR_ticker)){
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  name = KOR_ticker[i, '종목코드']
  data_fs[[i]] = read.csv(paste0('data/KOR_fs/', name,
                                 '_fs.csv'), row.names = 1)
}

fs_item = data_fs[[1]] %>% rownames()
length(fs_item)

print(head(fs_item))

select_fs = lapply(data_fs, function(x) {
  # 해당 항목이 있을시 데이터를 선택
  if ( '매출액' %in% rownames(x) ) {
    x[which(rownames(x) == '매출액'), ]
    
    # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
  } else {
    data.frame(NA)
  }
})

select_fs = bind_rows(select_fs)

print(head(select_fs))

select_fs = select_fs[!colnames(select_fs) %in%
                        c('.', 'NA.')]
select_fs = select_fs[, order(names(select_fs))]
rownames(select_fs) = KOR_ticker[, '종목코드']

print(head(select_fs))

fs_list = list()

for (i in 1 : length(fs_item)) {
  cat(paste0("\r==== Progress: " , i, "/", length(fs_item), "===="))
  select_fs = lapply(data_fs, function(x) {
    # 해당 항목이 있을시 데이터를 선택
    if ( fs_item[i] %in% rownames(x) ) {
      x[which(rownames(x) == fs_item[i]), ]
      
      # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
    } else {
      data.frame(NA)
    }
  })
  
  # 리스트 데이터를 행으로 묶어줌 
  select_fs = bind_rows(select_fs)
  
  # 열이름이 '.' 혹은 'NA.'인 지점은 삭제 (NA 데이터)
  select_fs = select_fs[!colnames(select_fs) %in%
                          c('.', 'NA.')]
  
  # 연도 순별로 정리
  select_fs = select_fs[, order(names(select_fs))]
  
  # 행이름을 티커로 변경
  rownames(select_fs) = KOR_ticker[, '종목코드']
  
  # 리스트에 최종 저장
  fs_list[[i]] = select_fs
  
}

# 리스트 이름을 재무 항목으로 변경
names(fs_list) = fs_item

saveRDS(fs_list, 'data/KOR_fs.Rds')

# 가치 지표 정리하기
library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

data_value = list()

for (i in 1 : nrow(KOR_ticker)){
  cat(paste0("\r==== Progress: " , i, "/", nrow(KOR_ticker), "===="))
  name = KOR_ticker[i, '종목코드']
  data_value[[i]] =
    read.csv(paste0('data/KOR_value/', name,
                    '_value.csv'), row.names = 1) %>%
    t() %>% data.frame()
  
}

data_value = bind_rows(data_value)
print(head(data_value))

data_value = data_value[colnames(data_value) %in%
                          c('PER', 'PBR', 'PCR', 'PSR')]

data_value = data_value %>%
  mutate_all(list(~na_if(., Inf)))

rownames(data_value) = KOR_ticker[, '종목코드']
print(head(data_value))

write.csv(data_value, 'data/KOR_value.csv')

# 종목정보 데이터 분석----
library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector = read.csv('data/KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,'left', 0)
KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)

library(dplyr)

data_market = left_join(KOR_ticker, KOR_sector,
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))

head(data_market)
glimpse(data_market) # 데이터 구조 확인 
# str이랑 비스하지만, tidy 형태로 결과물이 더 깔끔

head(names(data_market), 15)

data_market = data_market %>%
  rename(`배당수익률(%)` = `배당수익률`)

head(names(data_market), 15)

data_market %>%
  distinct(SEC_NM_KOR) %>% c() 
# distinct()는 함수의 고유한 값 반환(unique()와 동일)
# WICS 기준 10개 섹터 및 섹터 정보가 없는 종목인 NA값이 있음

# select(): 열 선택
data_market %>%
  select(`종목명`) %>% head()

data_market %>%
  select(`종목명`, `PBR`, `SEC_NM_KOR`) %>% head()

# select() 응용 : starts_with(): 특정 문자로 시작하는 열 선택
data_market %>%
  select(starts_with('시')) %>% head()
# select() 응용 : ends_with(): 특정 문자로 끝나는 열 선택
data_market %>%
  select(ends_with('R')) %>% head()
# select() 응용 : contains(): 특정 문자 포함되는 열 선택
data_market %>%
  select(contains('가')) %>% head()

# mutate(): 열  생성 및 데이터 변형
data_market = data_market %>%
  mutate(`PBR` = as.numeric(PBR),
         `PER` = as.numeric(PER),
         `ROE` = PBR / PER,
         `ROE` = round(ROE, 4),
         `size` = ifelse(`시가총액` >=
                           median(`시가총액`, na.rm = TRUE),
                         'big', 'small')
  )

data_market %>%
  select(`종목명`, `ROE`, `size`) %>% head()

# filter(): 조건을 충족하는 행 선택

#
# (기본 종목 선정)베타 계산하기----
# 베타: 개별 주식이 전체 주식시장의 변동에 반응하는 정도를 나타낸 값
# 회귀분석모형:     y = a + bx
# 자산가격결정모형: Ri = Rf + Bi[Rm - Rf]

library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('102110.KS', '039490.KS') # TIGER200, 키움증권
getSymbols(symbols)

prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x))))

ret = Return.calculate(prices) # 수익률 계산
ret = ret['2016-01::2018-12'] # 시작일::종료일

rm = ret[, 1]
ri = ret[, 2]

reg = lm(ri ~ rm)
summary(reg)
# 독립변수: 첫번쨰 열(kospi200), 종속변수: 두번쨰 열(키움증권)
# 상수항은 거의 0, t값 매우 작음 -> 유의 X
# 베타값: 1.76, t값: 19.36 -> 매우 유의
# 조정 결정계수: 0.34

# 베타 시각화
plot(as.numeric(rm), as.numeric(ri), pch = 4, cex = 0.3, 
     xlab = "KOSPI 200", ylab = "Individual Stock",
     xlim = c(-0.02, 0.02), ylim = c(-0.02, 0.02))
abline(a = 0, b = 1, lty = 2) # 기울기가 1인 경우 표시
abline(reg, col = 'red') # 증권주의 회귀분석 결과 
# 기울기가 1보다 가파름 -> 베타가 1보다 크다는 것.

# (기본 종목 선정)저변동 전략 ----

# 변동성: 수익률이 움직이는 정도(일반적으로 표준편차 사용)
#표준편차 함수: sd()
example = c(85, 76, 73, 80, 72)
sd(example)

## 저변동성 포트폴리오 구하기: 일간 기준
# 최근 1년 일간 수익률 기준 변동성 낮은 30종목 선택
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' = 
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price)
std_12m_daily = xts::last(ret, 252) %>% apply(., 2, sd) %>%
  multiply_by(sqrt(252))
# last()함수가 dplyr과 xts 둘 다 있어서 패키지 지정해줌 
# xts::last(): 마지막 n개 데이터 선택
# sd: 표준편차(=변동성) 계산
# muliply_by(): 연율화를 해주기 위해 루트(영업일 수)를 곱해줌

std_12m_daily %>% 
  data.frame() %>%
  ggplot(aes(x = (`.`))) +
  geom_histogram(binwidth = 0.01) +
  annotate("rect", xmin = -0.02, xmax = 0.02,
           ymin = 0,
           ymax = sum(std_12m_daily == 0, na.rm = TRUE) * 1.1,
           alpha=0.3, fill="red") +
  xlab(NULL)
# 변동성 히스토스램에 0에 위치하는 종목이 다수 존재
# 1년 거래정지인 종목인 것. -> NA로 처리해줌
std_12m_daily[std_12m_daily == 0] = NA

std_12m_daily[rank(std_12m_daily) <= 30]
# rank()함수로 순위 구함. R은 기본적으로 오름차순(가장 낮은 값 순위가 1)
# 따라서 변동성이 낮은 순서로 됨. -> 변동성 낮은 30종목 선택
# 아래는 해당 종목들의 변동성 확인
std_12m_daily[rank(std_12m_daily) <= 30] %>%
  data.frame() %>%
  ggplot(aes(x = rep(1:30), y = `.`)) +
  geom_col() +
  xlab(NULL)

invest_lowvol = rank(std_12m_daily) <= 30
# 일일 기준 변동성 낮은 30개만 추출

# 티커와 종목명, 연율화 변동성 확인
KOR_ticker[invest_lowvol, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` = round(std_12m_daily[invest_lowvol], 4))
# 종목코드, 종목명 열을 invest_lowvol의 열 수만큼 추출
# invest_lowvol 소수점 4자리까지 값으로 변동성이라는 열 생성

## 저변동성 포트폴리오 구하기: 주간기준
std_12m_weekly = xts::last(ret, 252) %>%
  apply.weekly(Return.cumulative) %>%
  apply(., 2, sd) %>% multiply_by(sqrt(52))
# 연율화를 위해 루트(영업일의 일주일 수)만큼 곱해줌
std_12m_weekly[std_12m_weekly == 0] = NA

std_12m_weekly[rank(std_12m_weekly) <= 30]

invest_lowvol_weekly = rank(std_12m_weekly) <= 30
KOR_ticker[invest_lowvol_weekly, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` =
           round(std_12m_weekly[invest_lowvol_weekly], 4))

# intersect() 로 일간 변동성, 주간 변동성 기준 모두 포함되는 종목 찾기
intersect(KOR_ticker[invest_lowvol, '종목명'],
          KOR_ticker[invest_lowvol_weekly, '종목명'])

# (기본 종목 선정) 모멘텀 전략 ----
# 모멘텀: 주가 혹은 이익의 추세.
# 상승(/하락) 추세의 주식은 지속적으로 상승(/하락)

# 모멘텀 발생 주 원인: 투자자들의 스스로에 대한 과잉 신뢰
# 모멘텀
#  - 이익 모멘텀: 기업의 이익에 대한 추세
#  - 가격 모멘텀: 주가에 대한 단기(~1개월)/중기(~1년)/장기(~5년) 모멘텀

## 모멘텀 포트폴리오 구하기: 12개월 모멘텀
# 최근 1년동안 수익률이 높은 30종목 선택
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})
# prod()로 각 종목의 1년 누적 수일률 계산: 누적곱
# 누적 수익률 = (1+수익률1) * (1+수익률2) * ... * (1+수익률n) - 1

ret_12m[rank(-ret_12m) <= 30]
# rank(-대상): 내림차순으로 순위 계산(기본값은 오름차순)
# -> 누적 수익률 가장 높은 종목이 1번. -> 30등까지 선택
invest_mom = rank(-ret_12m) <= 30

# 티커, 종목명, 누적수익률 확인
KOR_ticker[invest_mom, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom], 4))

## 모멘텀 포트폴리오 구하기: 위험조정 수익률

# 단순히 과거 수익률로만 모멘텀 종목 선택하면 각종 테마, 이벤트에 
# 따른 급등으로 변동성이 지나치게 높은 종목이 있을 수 있음.
# -> 누적수익률을 변동성으로 나눠서 위험을 고려. -> 상대적으로 안정적

ret = Return.calculate(KOR_price) %>% xts::last(252) 
# 누적수익률
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})
# 변동성
std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))
# 누적수익률/변동성
sharpe_12m = ret_12m / std_12m

# 이를 통해 수익률이 높으면서 변동성이 낮은 종목 선정할 수 잆음
invest_mom_sharpe = rank(-sharpe_12m) <= 30
KOR_ticker[invest_mom_sharpe, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom_sharpe], 2),
         `변동성` = round(std_12m[invest_mom_sharpe], 2),
         `위험조정 수익률` =
           round(sharpe_12m[invest_mom_sharpe], 2)) %>%
  as_tibble() %>%
  print(n = Inf) 
# tibble은 기본적으로 10개만 출력하는데 print(n=Inf)하면 모든 행 출력

# 단순수익률 및 위험조정 수익률 기준 모두에 포함되는 종목
intersect(KOR_ticker[invest_mom, '종목명'],
          KOR_ticker[invest_mom_sharpe, '종목명'])

# 위험조정 수익률 상위 30종목 가격 그래프
library(xts)
library(tidyr)
library(ggplot2)

KOR_price[, invest_mom_sharpe] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) %>%
  ggplot(aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap(. ~ ticker, scales = 'free') +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

# (기본 종목 선정) 밸류 전략----

# 가치주 효과: 내재가치 대비 낮은 가격의 주식(저PER, 저PBR 등)이
#              내재가치 대비 비싼 주식보다 수익률이 높은 현상

# 1.위험한 기업은 비교적 낮은 가격에 거래됨
#          -> 위험을 감당한 대가로 수익 발생
# 2.투자자들의 성장주에 대한 과잉 반응으로 인해 가치주는 시장에서 소외
#          -> 제자리를 찾아가는 과정에서 수익이 발생.

# 기업의 가치를 나타내는 지표: 일반적으로 PER, PBR, PCR, PSR 이 사용됨

## 밸류 포트폴리오 구하기: 저PBR
library(stringr)
library(ggplot2)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

invest_pbr = rank(KOR_value$PBR) <= 30

KOR_ticker[invest_pbr, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`PBR` = round(KOR_value[invest_pbr, 'PBR'], 4))

## 각 지표 결합하기

# 저PBR 하나의 지표만으로도 우수한 성과를 거둘 수 있음은 오랜기간 증명됨
# 그러나 저평가 주식이 계속해서 저평가에 머무르는 가치함정에 위험 존재. 
# 따라서 여러 지표를 동시에 볼 필요 있음

library(corrplot)

rank_value = KOR_value %>% 
  mutate_all(list(~min_rank(.)))

cor(rank_value, use = 'complete.obs') %>%
                # NA가 있는 종목은 삭제해주는 complete.obs
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt=45, tl.col = 'black',
           col = colorRampPalette(
             c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))
# 상관관계 그림을 보면 같은 가치지표임에도 상관관계가 낮은 지표도 있음.
# 띠리서 지표를 통합적으로 고려하면 분산효과 기대할 수 있음.

rank_sum = rank_value %>%
  rowSums() # 종목별 랭킹들의 합 구해줌.

invest_value = rank(rank_sum) <= 30
# 네 개 지표 랭킹의 합 기준 랭킹이 낮은 30종목 선택

KOR_ticker[invest_value, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(KOR_value[invest_value, ], 2))

# 저PBR 기준 선택한 종목과 비교해봤을 때 겹치는 종목 줄어듬
intersect(KOR_ticker[invest_pbr, '종목명'],
          KOR_ticker[invest_value, '종목명'])

# (기본 종목 선정)퀄리티 전략----
# 퀄리티: 기업의 우량성

# 업계에서 사용되는 우량성 관련 지표(주로 재무제표 데이터 사용)
# 1. Profitability (수익성)
# 2. Earning stability (수익의 안정성)
# 3. Capital structure (기업 구조)
# 4. Growth (수익의 성장성)
# 5. Acounting quality (회계쩍 우량성)
# 6. Payout/dilution (배당)
# 7. Investment (투자)

## F-Score
#  : 재무적 우량 정도를 [수익성], [재무성과], [운영 효율성]으로 구분해서 
#     9개 지표 선정

library(stringr)
library(ggplot2)
library(dplyr)

KOR_fs = readRDS('data/KOR_fs.Rds') # 저장해뒀던 재무제표 데이터
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

# 수익성
ROA = KOR_fs$'지배주주순이익' / KOR_fs$'자산'
CFO = KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산'
ACCURUAL = CFO - ROA                            # CFO가 ROA보다 커야 +

# 재무성과
LEV = KOR_fs$'장기차입금' / KOR_fs$'자산'        # Leverage
LIQ = KOR_fs$'유동자산' / KOR_fs$'유동부채'      # Liquidity
OFFER = KOR_fs$'유상증자' 
# 발행주식 수 데이터를 구할 수 없어서 대용치로 유상증자 여부 사용

# 운영 효율성
MARGIN = KOR_fs$'매출총이익' / KOR_fs$'매출액'   # 매출 총이익 
TURN = KOR_fs$'매출액' / KOR_fs$'자산'           # 회전율

# 위의 지표들이 조건을 충족하는지에 따라 1점 or 0점 부여
# 총 0~9점까지의 포트폴리오 구성

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

F_1 = as.integer(ROA[, num_col] > 0)
F_2 = as.integer(CFO[, num_col] > 0)
F_3 = as.integer(ROA[, num_col] - ROA[, (num_col-1)] > 0)
F_4 = as.integer(ACCURUAL[, num_col] > 0) 
F_5 = as.integer(LEV[, num_col] - LEV[, (num_col-1)] <= 0) 
F_6 = as.integer(LIQ[, num_col] - LIQ[, (num_col-1)] > 0)
F_7 = as.integer(is.na(OFFER[,num_col]) |
                   OFFER[,num_col] <= 0)
F_8 = as.integer(MARGIN[, num_col] -
                   MARGIN[, (num_col-1)] > 0)
F_9 = as.integer(TURN[,num_col] - TURN[,(num_col-1)] > 0)

# 테이블로 묶어주기
F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9) 
# 행으로 다 더해서 F-Score결과값 추출
F_Score = F_Table %>%
  apply(., 1, sum, na.rm = TRUE) %>%
  setNames(KOR_ticker$`종목명`) # F_Score에 종목명 세팅

(F_dist = prop.table(table(F_Score)) %>% round(3))

F_dist %>%
  data.frame() %>%
  ggplot(aes(x = F_Score, y = Freq,
             label = paste0(Freq * 100, '%'))) +
  geom_bar(stat = 'identity') +
  geom_text(color = 'black', size = 3, vjust = -0.4) +
  scale_y_continuous(expand = c(0, 0, 0, 0.05),
                     labels = scales::percent) +
  ylab(NULL) +
  theme_classic() 

invest_F_Score = F_Score %in% c(9)
KOR_ticker[invest_F_Score, ] %>% 
  select(`종목코드`, `종목명`) %>%
  mutate(`F-Score` = F_Score[invest_F_Score])
# F-score 9점인 종목은 26개

## 각 지표를 결합하기
# 퀄리티를 측정하는 요소 중 가장 널리 사용되는 지표를 결함한 포트폴리오
# 자기자본이익률(ROE), 매출총이익(Gross Profit), 영업활동현금흐름(Cash Flow From Operating)

library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]
quality_gpa = (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]
quality_cfo =
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))

rank_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.))))
# 퀄리티 지표는 높을 수록 좋은 내림차순으로 계산해야 해서 desc() 추가

library(corrplot)
cor(rank_quality, use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))
# 수익성 지표 역시 서로 간의 상관관계가 낮음
# -> 지표를 통합적으로 고려 시 분사효과를 기대할 수 있음.

rank_sum = rank_quality %>%
  rowSums()
# rowSums() 로 종목별 랭킹들의 합 구함

invest_quality = rank(rank_sum) <= 30
# rank()기본은 오름차순이라 랭킹이 30 보다 작은 숫자이다 = 가장 합이 낮은 30개

KOR_ticker[invest_quality, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(quality_profit[invest_quality, ], 4))

# (심화 종목 선정) 섹터 중립 포트폴리오 ----

# 팩터 전략의 단점 중 하나는 선택된 종목들이 특정 섹터로 쏠리는 경우가 있다는 점.
# 과거 수익률을 토대로 종목을 선정하는 모멘텀 전략은 특정 섹터의 호황기에 
# 동일한 섹터의 모든 종목이 함꼐 움직이는 경향이 있어 쏠림이 심할 수 있음

library(stringr)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})

invest_mom = rank(-ret_12m) <= 30

KOR_sector = read.csv('data/KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)

# data_market이라는 이름으로 결합
data_market = left_join(KOR_ticker, KOR_sector,
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))

data_market[invest_mom, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic(base_family = 'AppleGothic')
# 섹터가 대부분 IT로 몰려있음

# 섹터 중립 포트폴리오 구성
sector_neutral = data_market %>%
  select(`종목코드`, `SEC_NM_KOR`) %>%
  mutate(`ret` = ret_12m) %>% # 연간 수익률로 열 생성
  group_by(`SEC_NM_KOR`) %>%
  mutate(scale_per_sector = scale(`ret`), # scale()로 그룹별 정규화 진행
         scale_per_sector = ifelse(is.na(`SEC_NM_KOR`),
                                   NA, scale_per_sector))
# 전체 종목에서 12개월 수익률을 비교하는 것이 아니라 
# 각 섹터 별로 수익률의 강도를 비교함.
# -> 특정 종목의 과거 수익률이 전체 종목과 비교해서 높아도 
# 섹터 내 순위가 낮다면 정규화된 값은 낮아짐.

invest_mom_neutral =
  rank(-sector_neutral$scale_per_sector) <= 30

data_market[invest_mom_neutral, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic(base_family = 'AppleGothic')
# 잘 된것 같아 보이진 않지만..섹터 종목이 분산된다고 함

# (심화 종목 선정) 마법 공식 ----

# 여러가지 팩터를 결합해 투자해야 좋은 포트폴리오
# -> 멀티 팩터. 
# 그 중 밸류&퀄리티 조합은 전통적(마법공식)

##### 퀄리티와 밸류 간의 관계
# 가치주는 위험이 크기 때문에 시장에서 소외를 받아 저평가가 이뤄짐.
# -> 이러한 위험에 대한 대가로 밸류팩터의 수익률이 높게 됨.
# 반대로 우량주에는 기꺼이 프리미엄을 지불하려 하기 때문에 
# -> 퀄리티의 수익률이 높음
# 동전 양면같지만, 장기적으로 가치주와 우량주 모두 우수한 성과를 기록함.

# 매출총이익(퀄리티) & PBR(밸류 지표) 둘 사이의 관계
library(stringr)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

data_pbr = KOR_value['PBR']

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

data_gpa =
  (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col] %>%
  setNames('GPA')

cbind(data_pbr, -data_gpa) %>%
# GPA 앞에 마이너스 붙임
# PBR은 작을수록 좋은거고(오름차순), GPA는 클수록 좋은거라서(내림차순) 
  cor(method = 'spearman', use = 'complete.obs') %>% round(4)
# 서로 반대 관계에 있음이 확인됨.

# PBR의 분위별 GPA 평균값
cbind(data_pbr, data_gpa) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>% 
           # ntile()로 PBR을 5분위수로 나누어줌
  filter(!is.na(quantile_pbr)) %>%
  group_by(quantile_pbr) %>%
  summarise(mean_gpa = mean(GPA, na.rm = TRUE)) %>%
  ggplot(aes(x = quantile_pbr, y = mean_gpa)) +
  geom_col() +
  xlab('PBR') + ylab('GPA')
# 그림을 보면 PBR이 낮을 수록 GPA도 낮음. -> 가치주일 수록 우량성은 떨어짐
# 반면, PBR이 높을 수록 GPA도 높음. -> 주가가 비쌀 수록 우량성도 높다는 뜻

#### 마법공식 이해하기
# 조엘 그린블란트에 의해 알려진 투자방법
# 투자에 있어 중요한 두 가지 지표를 혼합하면 뛰어난 성과 기대.
# 1. 이율(Earning Yield)
#    : (기업의 수익) / (기업의 가치) 
#    : PER의 역수와 비슷함. 밸류 지표 중 하나.

# 2. 투하자본 수익률(Return on Capital)
#    : (기업의 수익) / (투자한 자본)
#    : ROE와 비슷함. 퀄리티 지표 중 하나.

# 이 두가지 지표의 랭킹의 합 기준 상위 30종목 1년 보유 후 매도

#### 마법공식 구성하기
## 이익수익률: (기업의 수익) / (기업의 가치)

# (기업의 수익): 이자 및 법인세 차감 전 이익
#              : 당기순이익+법인세+이자비용

# (기업의 가치): 시가총액+순차입금
#              : 시가총액+총부채-여유자금
#              : 시가총액+총부채-(현금-max(0,유동부채-유동자산+현금))

library(stringr)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

# 분자
magic_ebit = (KOR_fs$'지배주주순이익' + KOR_fs$'법인세비용' +
                KOR_fs$'이자비용')[num_col]

# 분모
magic_cap = KOR_value$PER * KOR_fs$'지배주주순이익'[num_col]
magic_debt = KOR_fs$'부채'[num_col]
magic_excess_cash_1 = KOR_fs$'유동부채' - KOR_fs$'유동자산' +
  KOR_fs$'현금및현금성자산'
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 =
  (KOR_fs$'현금및현금성자산' - magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_debt - magic_excess_cash_2

# 이익수익률
magic_ey = magic_ebit / magic_ev


## 투하자본 수익률: (이자 및 법인세 차감 전 이익)/(투하자본)

# (이자및법인세차감전이익) 
#           : 당기순이익+법인세+이자비용

# (투자하본) 
#           : (유동자산-유동부채)+(비유동자산-감가상각비)

magic_ic = ((KOR_fs$'유동자산' - KOR_fs$'유동부채') +
              (KOR_fs$'비유동자산' - KOR_fs$'감가상각비'))[num_col]
magic_roc = magic_ebit / magic_ic

invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30
# 내림차순으로 값을 구하기 위해 마이너스 붙여줌

# 결과값
KOR_ticker[invest_magic, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`이익수익률` = round(magic_ey[invest_magic, ], 4),
         `투하자본수익률` = round(magic_roc[invest_magic, ], 4))
# (심화 종목 선정) 이상치 데이터 제거 및 팩터의 결합 ----

#### 이상치 데이터 탐색
library(magrittr)
library(ggplot2)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)

max(KOR_value$PBR, na.rm = TRUE) # 545.1138

KOR_value %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)
# 오른쪽으로 꼬리가 긴 분포를 보임.
# 이는 PBR이 545.1138인 이상치 데이터가 있어서 그럼.
# 극단치 처리해야 됨.

####  트림(trim): 이상치 데이터 삭제
# 상하위 1% 데이터 삭제하기
library(dplyr)

value_trim = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99, NA, PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01, NA, PBR))
# 상하위 1% 데이터를 NA로 변경

value_trim %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)
# x축 스케일이 많이 줄어듦

# 평균이나 분산같이 통계값을 구하는 과정에서는 
# 이상치 데이터를 제거하는 거싱 바람직함.

# 그러나 팩터를 이용해 포트폴리오를 구하는 과정에서는 사용하지 않음
# 제거된 종목 중 정말 좋은 종목이 있을 수 있기 떄문.

#### 윈저라이징(Winsorizing): 이상치 데이터 대체

# 포트폴리오 구성 중 이상치 데이터 대체 방법: 윈저라이징

# 상위 99% 초과하는 값: 상위 99%의 값으로 대체
# 하위 1% 미만의 데이터: 하위 1% 데이터로 대체

value_winsor = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99,
                      quantile(., 0.99, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01,
                      quantile(., 0.01, na.rm = TRUE), PBR))

value_winsor %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)

#### 팩터의 결합 방법
# 단순히 팩터를 더하는 방법은 쉽고 효율적이지만 
# 여러가지 문제를 안고 있음

library(tidyr)

KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  
# 랭킹구하는 것의 장점: 극단치로 인한 효과가 사라진다는 점
# 그러나 x축에서의 최대값이 서로 다름.
# 지표별 유효 데이터의 개수가 다르기 때문.
# 단순히 합치는건 좋은 방법이 아님.
# 만약 팩터 별 비중을 다르게 부여한다면 결과는 왜곡됨.

# 해결방법: Z-Score로 정규화
# Z-Score(Rank(Factor A)) + Z-Score(Rank(Factor B)) + ... 끝까지
KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  
# 각 팩터별 랭킹을 정규화한 뒤 더함.
# -> 왜곡 효과가 제거되어 안정적

#### 멀티팩터 포트폴리오
# 앞에서 배운 팩터 이론들과 결합 방법을 응용해서 진행

# 사용되는 지표
# - 퀄리티: 자기자본이익률, 매출 총이익, 영업활동현금흐름
# - 밸류: PER, PBR, PSR, PCR
# - 모멘텀: 3개월 수익률, 6개월 수익률, 12개월 수익률

library(xts)
library(stringr)

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

# 최종적으로 기준 상위 30종목 선택
invest_qvm = rank(factor_qvm) <= 30

### 선택된 종목의 퀄리티 지표 별 분포
KOR_value[invest_qvm, ] %>%
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

# 최종 포트폴리오 내 종목들의 지표 별 평균 값
cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()
cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()

###test#######
KOR_ticker[invest_qvm, ]
price
KOR_price[,invest_qvm]


####








# 포트폴리오 구성을 위한 ETF 데이터 탐색 ----
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

nrow(rets)
tail(rets)
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
# 같은 자산군 내에서는 강상 상관관계를 보이고 
# 주식과 채권 간에는 매우 낮은 상관관계를 보임.
# 주식과 리츠도 높은 상관관계를 보임

# 포트폴리오 최적화에는 분산-공분산 행렬이 대부분 사용됨.
# -> 이는 cov() 함수로 손쉽게 계산 가능
covmat = cov(rets)

# 최소분산 포트폴리오 ----
### slspq(x0, fn, hin=NULL, heq = NULL)
# x0: 초기값 
# fn: 목적함수(포트폴리오 변동성)
# hin: 부등위 제약조건(각 자산의 비중이 0보다 큰 제약조건)
# heq: 등위 제약조건(투자 비중의 합이 1인 제약조건)

# slsqp()로 최소분산포트폴리오 만족하는 투자비중 구하는 과정
# fn, hin, heq에 해당하는 함수들을 각각 만듦
# 함수를 결합해 최적화된 결과값을 얻는다.

## 목적함수(fn)
objective = function(w) {
  obj = t(w) %*% covmat %*% w
  return(obj)
}
# covmat: 사전에 계산된 분산-공분산행렬
# w: 자산의 투자비중
# obj: 포트폴리오의 변동성을 계싼한 값
# 계산된 w를 바탕으로 포트폴리오의 변동성 반환. 
# -> 해당 갑싱 최소가 되도록 하는 것이 목표

## wi >= 0 제약조건에 해당하는 부등위 제약조건
hin.objective = function(w) {
  return(w)
}
# 패키지에서는 hin >= 0의 형태로 인식하므로 계산된 비중인 w를 단순히 입력

## 나머지 제약조건에 해당하는 등위제약조건
heq.objective = function(w) {
  sum_w = sum(w)
  return( sum_w - 1 )
}
# 계산된 비중인 w들의 합계를 구한 뒤 해당 값에서 1을 빼주는 값을 반환
# 프로그래밍 내에서는 heq==0의 형태로 인식하므로 (sum_w-1)==0
# 즉, sum_w == 1의 제약조건과 동일

library(nloptr)

result = slsqp( x0 = rep(0.1, 10),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective)

print(result$par) 
# result$par: 최소분산 포트폴리오를 구성하는 투자자산비중
print(result$value)
# result$value: $par에서 산출된 값을 목적함수 fn에 넣었을 때 나오는 값
# 포트폴리오의 분산

w_1 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_1)

### solve.QP() 를 이용한 최적화
# 설명은 노션에.
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10)))
bvec = c(1, rep(0, 10), -rep(1, 10))
meq = 1

library(quadprog)
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

print(result$solution) # MVP를 구성하는 투자자산비중
print(result$value) 
# $solution에서 산출한 값을 목적함수에 넣었을 때 나오는 값
# = 포트폴리오의 분산

w_2 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_2)


### optimalPortfolio() 를 이용한 최적화
library(RiskPortfolios)

w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_3)  

### 결괏값 비교
library(ggplot2)

data.frame(w_1) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_1)) +
  geom_col() +
  xlab(NULL) + ylab(NULL)

### 최대 및 최소 투자비중 제약조건
# slspq()
result = slsqp( x0 = rep(0.1, 10),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective,
                lower = rep(0.05, 10), # 최소비중
                upper = rep(0.20, 10)) # 최대비중

w_4 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_4)

# solve.QP()
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10)))
bvec = c(1, rep(0.05, 10), -rep(0.20, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_5 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_5)

# optimalProtfolio()
w_6 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'user',
                                      LB = rep(0.05, 10),
                                      UB = rep(0.20, 10))) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_6)

# 비중 제약조건 결과
data.frame(w_4) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_4)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

### 각 자산 별 제약조건 추가
# solve.QP()로 쉽게 가능
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10))) 
bvec = c(1, c(0.10, 0.10, 0.05, 0.05, 0.10,
              0.10, 0.05, 0.05, 0.03, 0.03),
         -c(0.25, 0.25, 0.20, 0.20, 0.20,
            0.20, 0.10, 0.10, 0.08, 0.08))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_f <- result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))
# 자산 별 제약조건 결과 그래프로 확인
data.frame(w_f) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_f)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)


# 최대 분산효과 포트폴리오 ----
# MDP
### solve.QP()를 이용한 최적화
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(sqrt(diag(covmat)), diag(10)))
bvec = c(1, rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

# 표준화
w = (w / sum(w)) %>%
  round(., 4)

print(w)
data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_col() +
  xlab(NULL) + ylab(NULL)

### optimalPortfolio()를 이용한 최적화
w = optimalPortfolio(covmat,
                     control = list(type = 'maxdiv',
                                    constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

### 최소 및 최대 투자비중 제약조건
Dmat = covmat
dvec = rep(0, 10)
Alb = -rep(0.05, 10) %*% matrix(1, 1, 10) + diag(10)
Aub = rep(0.20, 10) %*% matrix(1, 1, 10) - diag(10)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

### 각 자산 별 제약조건 추가
Dmat = covmat
dvec = rep(0, 10)
Alb = -c(0.10, 0.10, 0.05, 0.05, 0.10,
         0.10, 0.05, 0.05, 0.03, 0.03) %*%
  matrix(1, 1, 10) + diag(10)
Aub = c(0.25, 0.25, 0.20, 0.20, 0.20,
        0.20, 0.10, 0.10, 0.08, 0.08) %*%
  matrix(1, 1, 10) - diag(10)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

# 위험균형 포트폴리오 ----

### 위험기여도(RC)
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w
  port_std = sqrt(port_vol)
  
  MRC = (covmat %*% w) / as.numeric(port_std)
  RC = MRC * w
  RC = c(RC / sum(RC))
  
  return(RC)
}

### 주식 60%와 채권 40%의 포트폴리오 위험 기여도
ret_stock_bond = rets[, c(1, 5)] # 미국 주식, 장기채
cov_stock_bond = cov(ret_stock_bond)

# 자산별 위험기여도 계산
RC_stock_bond = get_RC(c(0.6, 0.4), cov_stock_bond)
RC_stock_bond = round(RC_stock_bond, 4)

print(RC_stock_bond)
# 비중을 6:4로 맞춰도 위험기여도는 전혀 다름

### rp()를 이용한 최적화
library(cccp)

opt = rp(x0 = rep(0.1, 10),
         P = covmat,
         mrc = rep(0.1, 10))

w = getx(opt) %>% drop()
# drop()로 벡터 형태로 변환
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

# 위험기여도 확인
get_RC(w, covmat)

### 위험예산 포트폴리오
library(cccp)

opt = rp(x0 = rep(0.1, 10),
         P = covmat,
         mrc = c(0.15, 0.15, 0.15, 0.15, 0.10,
                 0.10, 0.05, 0.05, 0.05, 0.05))

w = getx(opt) %>% drop()
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)

get_RC(w, covmat)

# 인덱스 포트폴리오 구성하기 ----
library(stringr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv',
                      row.names = 1, stringsAsFactors = FALSE) 

KOSPI200 = KOR_ticker %>% filter(시장구분 == 'KOSPI') %>%
  slice(1:200) %>% # 1번부터 200번까지 데이터 추출
  mutate(시가총액비중 = 시가총액 / sum(시가총액))
# 각 주식의 시가총액을 전체 시가총액으로 나누고 시가총액 비중 저장

# 시가총액 비중 시각화
library(ggplot2)

KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() + # 산점도
  xlab('종목명') +
  ylab('시가총액비중(%)') +
  scale_y_continuous(labels = scales::percent)+ # y축을 퍼센트 형식으로
  theme_classic(base_family = 'AppleGothic')
# 종목이 너무 많음 & 삼성전자만 너무 독보적임

# 수정 후 시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  xlab('종목명') +
  ylab('시가총액비중(로그 스케일링)') +
  scale_y_log10() +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme_classic(base_family = 'AppleGothic')
# scale_y_log10() 로 y축을 로그값으로 스케일링
# scale_x_discrete() 로 x축에 일부 종목만 표현

# 1억이 있을 경우 KOSPI 200을 복제하는 방법
KOSPI200 = KOSPI200 %>%
  mutate(매수금액 = 100000000 * 시가총액비중,
             매수주수 = 매수금액 / 종가)
# 갖고 있는 금액에 시가총액 비중을 곱해 각 주식 당 매수해야 하는 금액 도출

KOSPI200 %>% select(매수금액, 매수주수) %>% head()
# 이론적으로는 계산된 주식 수 만큼  매수해야 인덱스를 정확히 복제.
# but, 주식은 1주 단위로 거래할 수 있으므로 
# floor() 로 내림처리를 통해 보유 금액 내에서  적합한 매수주 수 수정
KOSPI200 = KOSPI200 %>% mutate(매수주수 = floor(매수주수))

KOSPI200 %>% select(매수금액, 매수주수) %>% head()

inv_money = KOSPI200 %>% mutate(실제매수금액 = 종가 * 매수주수) %>%
  summarize(sum(실제매수금액))

print(inv_money)

### 팩터를 이용한 인헨스드 포트폴리오 구성
KOSPI200 = KOSPI200 %>% select(종목명, PBR, 시가총액비중) %>%
  mutate(PBR = as.numeric(PBR)) 
# as.numeric() 처리를 함으로써 pbr 데이터 없는 곳에 [-]에서 NA로 변경

# 단순가감법
KOSPI200 = KOSPI200 %>%
  mutate(랭킹 = rank(PBR),
           조절비중 = ifelse(랭킹 <= 100, 시가총액비중 + 0.0005, 시가총액비중 - 0.0005),
           조절비중 = ifelse(조절비중 < 0, 0, 조절비중),
           조절비중 = 조절비중 / sum(조절비중),
           차이 = 조절비중 - 시가총액비중) 
# PBR 랭킹(낮은게 높은 순위) 상위 100 종목에는 각각 5bp를 더해주고, 
# 나머지 100 종목에서 각각 5bp 빼줌
# 비중이 5bp 미만인 종목은 5bp를 차감할 경우 비중이 0 미만이 되므로, 0으로 만들어 줌

library(tidyr)

head(KOSPI200)
tail(KOSPI200)

KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200, aes(x = reorder(종목명, -시가총액비중), y = 조절비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'))
# 검은색 점: 인덱스 내 시가총액비중 & 붉은색 점: 5bp씩 더하거나 뺀 투자비중 
# 약간씩의 베팅만 했으므로 기초지수와 크게 차이가 없음

KOSPI200_mod = KOSPI200 %>% arrange(PBR)

KOSPI200_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /10000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 10000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.title = element_text(family = 'AppleGothic')) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic' ))

### 팩터에 대한 전체 종목의 틸트
KOSPI200_tilt = KOSPI200 %>%
  select(종목명, PBR, 시가총액비중, 랭킹) %>%
  mutate(zscore = -scale(랭킹),
# 저PBR(높은 랭킹)은 zscore가 음수라서 마이너스 붙임
# -> 랭킹 높은 종목의 누적확률 값을 높이기 위해
         cdf = pnorm(zscore),
# pnorm()으로 누적확률 계산
# 랭킹높은 종목이 누적확률 크게 나옴
         투자비중 = 시가총액비중 * cdf,
        # 시총 * 누적확률
         투자비중 = 투자비중 / sum(투자비중),
         차이 = 투자비중 - 시가총액비중)

head(KOSPI200_tilt)
tail(KOSPI200_tilt)

# 시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'),
        axis.title = element_text(family = 'AppleGothic')) 


### 비중 차이가 지나치게 벌어지는 것을 방지하기 위한 제약
# 종목 당 시가총액 비중과 투자비중의 차이가 50bp 이상이 되지 않는 제약의 경우
# (제약을 더 크게 설정할 수록, 지수 대비 베팅의 크기가 커짐)

# 비중 차이 제약 걸기 전
KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() +
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'),
        axis.title = element_text(family = 'AppleGothic'))

# 비중 차이 제약 추가
KOSPI200_tilt = KOSPI200_tilt %>%
  mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
  mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
  mutate(투자비중 = 투자비중 / sum(투자비중), 
             차이 = 투자비중 - 시가총액비중)
# mutate_at()으로 차이가 +-50bp 를 넘으면 시가총액비중 +-50bp로 설정
# -> 표준화 다시 하고 차이 다시 구함

head(KOSPI200_tilt)
# 재표준화를 하는 과정에서 50bp를 넘는 종목 재발생

# 모든 종목의 차이가 50bp 이내가 될때까지 해당 작업 반복하도록 설정
while (max(abs(KOSPI200_tilt$차이)) > (0.005 + 0.00001)) {
  KOSPI200_tilt = KOSPI200_tilt %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
    mutate(투자비중 = 투자비중 / sum(투자비중), 
               차이 = 투자비중 - 시가총액비중)
}

head(KOSPI200_tilt)

# 제약 만족 후 차이 부분 시각화
KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() +
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'),
        axis.title = element_text(family = 'AppleGothic'))

# 벤치마크 지수와의 투자 비중 차이 시각화
KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'),
        axis.title = element_text(family = 'AppleGothic'))

# PBR에 따른 비중 차이
KOSPI200_tilt_mod = KOSPI200_tilt %>% arrange(PBR)

KOSPI200_tilt_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /2000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 2000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, family = 'AppleGothic'),
        axis.title = element_text(family = 'AppleGothic'))

# 포트폴리오 백테스트 ----
### 전통적인 60:40 포트폴리오 백테스트
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

ticker = c('SPY', 'TLT')
getSymbols(ticker)

prices = do.call(cbind,
                 lapply(ticker, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()
cor(rets)
portfolio = Return.portfolio(R = rets,
                             weights = c(0.6, 0.4),
                             rebalance_on = 'years',
                             verbose = TRUE)

# Return.portfolio 계산 과정
portfolio_show <- round(cbind(portfolio$BOP.Value$SPY.Adjusted, portfolio$BOP.Value$TLT.Adjusted, 
                        portfolio$BOP.Value$SPY.Adjusted + portfolio$BOP.Value$TLT.Adjusted, 
                        portfolio$BOP.Weight$SPY.Adjusted, portfolio$BOP.Weight$TLT.Adjusted,
                        (portfolio$EOP.Value$SPY.Adjusted/portfolio$BOP.Value$SPY.Adjusted)-1,
                        (portfolio$EOP.Value$TLT.Adjusted/portfolio$BOP.Value$TLT.Adjusted)-1,
                        portfolio$EOP.Value$SPY.Adjusted, portfolio$EOP.Value$TLT.Adjusted,
                        portfolio$EOP.Value$SPY.Adjusted + portfolio$EOP.Value$TLT.Adjusted,
                        portfolio$EOP.Weight$SPY.Adjusted, portfolio$EOP.Weight$TLT.Adjusted,
                        portfolio$returns) %>%
  setNames(c('주식시작가치', '채권시작가치','시작가치합계', '주식시작비중','채권시작비중', 
             '일별주식수익률', '일별채권수익률', '주식종료가치', '채권종료가치', '가치종료합계', 
             '주식종료비중', '채권종료비중', '최종수익률')), 3)

par(family='AppleGothic')

portfolios = cbind(rets, portfolio$returns) %>%
  setNames(c('주식', '채권', '60대 40'))


charts.PerformanceSummary(portfolios,
                          main = '60대 40 포트폴리오') %>%
  par(family='AppleGothic')

turnover = xts(
  rowSums(abs(portfolio$BOP.Weight -
                timeSeries::lag(portfolio$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(portfolio$BOP.Weight))

chart.TimeSeries(turnover)

###시점 선택전략 백테스트
# 단순 매수 후 보유 대비 극심한 하락장에서 낙폭을 줄일 수 있음
# -> 위험 대비 수익률 올릴 수 있음
# 리밸런싱 매월 실행
library(quantmod)
library(PerformanceAnalytics)

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




### 동적 자산배분 백테스트
library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

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

# 기존 비용을 고려하지 않은 포트폴리오(검은색)에 비해
# 비용을 차감한 포트폴리오(빨)의 수익률이 시간이 지남에 따라 서서히 감소
# 이러한 차이는 비용이 크거나 매매 회전율이 높을 수록 더욱 벌어짐

# 성과 및 위험 평가 ----
library(dplyr)
library(readxl)
library(xts)
library(timetk)

url = 'https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx'

tf = tempfile(fileext = '.xlsx')
download.file(url, tf, mode = 'wb')

excel_sheets(tf)

df_QMJ = read_xlsx(tf, sheet = 'QMJ Factors', skip = 18) %>%
  select(DATE, Global)
df_MKT = read_xlsx(tf, sheet = 'MKT', skip = 18) %>%
  select(DATE, Global)
df_SMB = read_xlsx(tf, sheet = 'SMB', skip = 18) %>%
  select(DATE, Global)
df_HML_Devil = read_xlsx(tf, sheet = 'HML Devil',
                         skip = 18) %>%
  select(DATE, Global)
df_UMD = read_xlsx(tf, sheet = 'UMD', skip = 18) %>%
  select(DATE, Global)
df_RF = read_xlsx(tf, sheet = 'RF', skip = 18) 

df = Reduce(function(x, y) inner_join(x, y, by = 'DATE'),
            list(df_QMJ, df_MKT, df_SMB,
                 df_HML_Devil,df_UMD, df_RF)) %>%
  setNames(c('DATE','QMJ', 'MKT', 'SMB',
             'HML', 'UMD', 'RF')) %>%
  na.omit() %>%
  mutate(DATE = as.Date(DATE, "%m/%d/%Y"),
         R_excess = QMJ - RF,
         Mkt_excess = MKT - RF) %>%
  tk_xts(date_var = DATE)

### 결과측정 지표
# 수익률 및 변동성
library(dplyr)
library(readxl)
library(xts)
library(timetk)

url = 'https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx'

tf = tempfile(fileext = '.xlsx')
download.file(url, tf, mode = 'wb')

excel_sheets(tf)

df_QMJ = read_xlsx(tf, sheet = 'QMJ Factors', skip = 18) %>%
  select(DATE, Global)
df_MKT = read_xlsx(tf, sheet = 'MKT', skip = 18) %>%
  select(DATE, Global)
df_SMB = read_xlsx(tf, sheet = 'SMB', skip = 18) %>%
  select(DATE, Global)
df_HML_Devil = read_xlsx(tf, sheet = 'HML Devil',
                         skip = 18) %>%
  select(DATE, Global)
df_UMD = read_xlsx(tf, sheet = 'UMD', skip = 18) %>%
  select(DATE, Global)
df_RF = read_xlsx(tf, sheet = 'RF', skip = 18) 

df = Reduce(function(x, y) inner_join(x, y, by = 'DATE'),
            list(df_QMJ, df_MKT, df_SMB,
                 df_HML_Devil,df_UMD, df_RF)) %>%
  setNames(c('DATE','QMJ', 'MKT', 'SMB',
             'HML', 'UMD', 'RF')) %>%
  na.omit() %>%
  mutate(DATE = as.Date(DATE, "%m/%d/%Y"),
         R_excess = QMJ - RF,
         Mkt_excess = MKT - RF) %>%
  tk_xts(date_var = DATE)


library(PerformanceAnalytics)
chart.CumReturns(df$QMJ)

prod((1+df$QMJ)) - 1 # 누적수익률
mean(df$QMJ) * 12 # 연율화 수익률(산술)
(prod((1+df$QMJ)))^(12 / nrow(df$QMJ)) - 1 # 연율화 수익률(기하)
Return.cumulative(df$QMJ) # 누적수익률
Return.annualized(df$QMJ, geometric = FALSE) # 연율화 수익률(산술)
Return.annualized(df$QMJ) # 연율화 수익률(기하)
sd(df$QMJ) * sqrt(12) # 연율화 변동성
StdDev.annualized(df$QMJ) # 연율화 변동성
SharpeRatio.annualized(df$QMJ, Rf = df$RF, geometric = TRUE)

table.Drawdowns(df$QMJ)
maxDrawdown(df$QMJ)

chart.Drawdown(df$QMJ)
CalmarRatio(df$QMJ)
apply.yearly(df$QMJ, Return.cumulative) %>% head()

library(lubridate)
library(tidyr)
library(ggplot2)

R.yr = apply.yearly(df$QMJ, Return.cumulative) %>%
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

UpsideFrequency(df$QMJ, MAR = 0)

roll_12 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_24 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 24, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_36 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 36, Return.annualized) %>% na.omit() %>%
  UpsideFrequency()

roll_win = cbind(roll_12, roll_24, roll_36)
print(roll_win)

df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  fortify.zoo() %>%
  ggplot(aes(x = Index, y = QMJ)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = 'red') +
  xlab(NULL) + ylab(NULL)

# 팩터 회귀분석 및 테이블로 나타내기
reg = lm(R_excess ~ Mkt_excess + SMB + HML + UMD, data = df)
# summary(reg)
summary(reg)$coefficient

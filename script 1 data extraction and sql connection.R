############################################################
# STRESS TESTING MODEL 
# SCRIPT 1: Data Extraction & SQL database Connection
# Data Source: Yahoo Finance
# Tools: R packages
# Author: Maina Silvia
############################################################

#Step 1: create database 'financedata_db' and table 'thefour_stock_prices' in MySQL

#Step 2: get data for 4 stock prices from yahoo finance(Apple, JP Morgan, Tesla, Vanguard)

# =========================
#  DATA COLLECTION
# =========================
library(quantmod)

tickers <- c("AAPL", "JPM", "TSLA", "VWO")
tickers

getSymbols(tickers, src = "yahoo", from = "2020-01-01")


# ===========================
# CLEANING AND COMBINING DATA
# ===========================

library(dplyr)

process_stock <- function(symbol){
  raw <- get(symbol)

  df <- data.frame(
    date = as.Date(index(raw)),
    adjusted = as.numeric(Ad(raw)),   
    close = as.numeric(Cl(raw)),
    volume = as.numeric(Vo(raw))
  )

  df$ticker <- symbol
  return(df)
}

all_data <- bind_rows(lapply(tickers, process_stock))


# ===========================
# CONNECT R TO MYSQL DATABASE
# ===========================

library(DBI)
library(RMySQL)

con <- dbConnect(
   RMySQL::MySQL(),
   dbname = "financedata_db",
   host = "localhost",
   user = "root",
   password = "",
   client.flag = CLIENT_LOCAL_FILES)

# ========================================
# INSERT,VERIFY, SUMMARIZE & EXPORT DATA 
# ========================================

dbListTables(con)

#insert data into sql database
dbWriteTable(con, "thefour_stock_prices", all_data, overwrite = TRUE,
             row.names = FALSE)

#verify data extraction
dbGetQuery(con, "SELECT DISTINCT ticker FROM thefour_stock_prices")

dbGetQuery(con, "SELECT * FROM thefour_stock_prices LIMIT 10")

#query specific verification for all 4 stocks

#Apple stock prices 
apple_data <- dbGetQuery (con,
               "SELECT * FROM thefour_stock_prices
               WHERE ticker = 'AAPL'
")

#JP Morgan stock prices
jpm_data <- dbGetQuery (con,
               "SELECT * FROM thefour_stock_prices
               WHERE ticker = 'JPM'
")

#Tesla stock prices
tesla_data <- dbGetQuery (con,
               "SELECT * FROM thefour_stock_prices
               WHERE ticker = 'TSLA'
")

#Vanguard FTSE stock prices
vanguard_data <- dbGetQuery (con,
               "SELECT * FROM thefour_stock_prices
               WHERE ticker = 'VWO'
")

#data summary
dbGetQuery(con, "
    SELECT ticker,
    COUNT(*) AS rows_count,
    MIN(date) AS start_date,
    MAX(date) AS end_date
    FROM thefour_stock_prices
    GROUP BY ticker
")

#export extracted stock price data to excel
write.csv(all_data, "thefour_stock_prices.csv", row.names = FALSE)

write.csv(all_data, "backup_stock_data.csv")

##############################################################
# STRESS TESTING MODEL 
# SCRIPT 2: Stock Returns Computation & Data Analysis
# Data Source: Yahoo Finance
# Tools: R Packages 
# Author: Maina Silvia
##############################################################

#Step 1: compute returns from the stock prices data

#Step 2: create a portfolio of all the 4 assets returns

# =========================
# SECTION 1: LOAD DATA
# =========================

#load data summary 
dbGetQuery(con, "
    SELECT ticker, COUNT(*) 
    FROM thefour_stock_prices 
    GROUP BY ticker
")

#load clean data
stock_data <- dbGetQuery(con, "SELECT * FROM thefour_stock_prices")
class(stock_data)

# Ensure correct types
stock_data$date <- as.Date(stock_data$date)

#load required library
library(dplyr)

#sort data properly
stock_data <- stock_data %>%
           arrange(ticker, date)

#check structure
str(stock_data)
head(stock_data)

#verify structure
stock_data %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarise(n = n())

# =========================
# SECTION 2: RETURNS
# =========================

#comupte returns
returns_data <- stock_data %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(returns = log(close / lag(close))) %>%
  ungroup() %>%
  filter(!is.na(returns))


#view returns data grouped summary
returns_data %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarise(
    mean_return = mean(returns),
    sd_return = sd(returns)
  )


#export the asset returns data to excel
write.csv(returns_data, "stock_returns_data.csv")

# ===========================
#  EXPLORATORY DATA ANALYSIS
# ===========================

# =========================
#  STOCK PRICE TRENDS
# =========================

library(ggplot2)

ggplot(stock_data,
  aes(x = date, y = adjusted, color = ticker, group = ticker)) +
  geom_line() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Stock Price Trends",
    x = "Date",
    y = "Adjusted Price"
  )

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/stock_data.png")

# =========================
# RETURNS DISTRIBUTION 
# =========================

ggplot(returns_data,
  aes(x = returns, fill = ticker)) +
  geom_histogram(bins = 50, alpha = 0.6) +
  facet_wrap(~ticker, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(title = "Stock Returns Distributions")

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/retrns_data.png")

# =========================
# CORRELATION ANALYSIS
# =========================

library(tidyr)

#reshape to wide format
returns_wide <- returns_data %>%
  dplyr::select(date, ticker, returns) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = returns) %>%
  tidyr::drop_na()

cor_matrix <- stats::cor(dplyr::select(returns_wide, -date))

#heatmap

cor_df <- as.data.frame(as.table(cor_matrix))

ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  theme_minimal(base_size = 14) +
  labs(title = "Correlation Matrix")

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/cor_df.png")


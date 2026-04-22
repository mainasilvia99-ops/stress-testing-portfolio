library(quantmod)
library(dplyr)
library(tidyr)

tickers <- c("AAPL", "JPM", "TSLA", "VWO")
tickers 

getSymbols(tickers, src = "yahoo", from = "2020-01-01")


process_stock <- function(symbol){
  data <- get(symbol)

  data.frame(
    date = as.Date(index(data)),
    ticker = symbol,
    close = as.numeric(Cl(data))
  )
}

price_data <- bind_rows(lapply(tickers, process_stock))
price_data <- price_data %>% arrange(ticker, date)

returns_data <- price_data %>%
  group_by(ticker) %>%
  mutate(returns = log(close / lag(close))) %>%
  ungroup() %>%
  filter(!is.na(returns))

returns_wide <- returns_data %>%
  select(date, ticker, returns) %>%
  pivot_wider(names_from = ticker, values_from = returns) %>%
  na.omit()

weights <- c(0.25, 0.25, 0.25, 0.25)
portfolio_value <- 1000000

portfolio_returns <- as.matrix(returns_wide[, tickers]) %*% weights

returns_wide$portfolio <- portfolio_returns

qqnorm(portfolio_returns)
qqline(portfolio_returns, col = "red")



library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

#portfolio returns histogram
ggplot(data.frame(portfolio_returns), aes(x = portfolio_returns)) +
  geom_histogram(bins = 50, fill = "violet", color = "white", alpha = 0.8) +
  labs(
    title = "Portfolio Returns Distribution",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal(base_size=14) 

#Plot portfolio returns Gains vs Losses (Risk Colour Coding)
returns_wide$sign <- ifelse(returns_wide$portfolio >= 0, "Gain", "Loss")

returns_wide$sign <- ifelse(
  returns_wide$portfolio >= 0, "Gain", "Loss"
)

ggplot(returns_wide, 
       aes(x = date, y = portfolio_returns, color = sign)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_line(color = "grey70", linewidth = 0.05) +
  scale_color_manual(values = c("Gain" = "darkgreen", "Loss" = "red")) +
  labs(
    title = "Portfolio Returns: Gains vs Losses",
    x = "Date",
    y = "Returns",
    color = "Market Move"
  ) +
  theme_minimal(base_size = 14)


#FITTED NORMAL CURVE

mean_return <- mean(portfolio_returns)
volatility <- sd(portfolio_returns)

ggplot(data.frame(portfolio_returns), aes(x = portfolio_returns)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
      fill = "steelblue", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean_return, sd = volatility),
                color = "red", linewidth = 1.2) +
  labs(
    title = "Portfolio Returns vs Normal Distribution",
    x = "Returns",
    y = "Density"
  ) +
  theme_minimal()

#TIME SERIES PLOT
ggplot(returns_wide, aes(x = date, y = portfolio)) +
  geom_line(color = "darkblue") +
  labs(
    title = "Portfolio Returns Over Time",
    x = "Date",
    y = "Return"
  ) +
  theme_minimal()

#Portfolio VaR TABLE
VaR_horizons <- data.frame(
  Horizon = c("1 Day", "10 Day", "21 Day"),
  
  VaR_95 = c(
    VaR_95,
    VaR_95_10d,
    VaR_95_21d
  ),
  
  VaR_99 = c(
    VaR_99,
    VaR_99_10d,
    VaR_99_21d
  )
)

VaR_horizons

gt_table <- gt(VaR_horizons)

gtsave(gt_table, "outputs/Scaled Portfolio Value at Risk.png")

VaR_95_value <- VaR_95 * portfolio_value
VaR_95_value

VaR_99_value <- VaR_99 * portfolio_value
VaR_99_value


#EXPECTED SHORTFALL -ES
ES_95 <- mean(portfolio_returns[portfolio_returns <= VaR_95])
ES_95

ES_99 <- mean(portfolio_returns[portfolio_returns <= VaR_99])
ES_99

#Portfolio Base Risk Metrics in the Normal Scenario
install.packages("e1071")   # run once
library(e1071)

install.packages("PerformanceAnalytics")  # once
library(PerformanceAnalytics)

risk_summary <- function(r) {
  
  data.frame(
    
    Mean_Return = colMeans(r),
    
    Variance = apply(r, 2, var),
    
    SD = apply(r, 2, sd),
    
    Skewness = apply(r, 2, function(x) e1071::skewness(x)),
    
    Kurtosis = apply(r, 2, function(x) e1071::kurtosis(x)),
    
    Volatility_Annualized = apply(r, 2, sd) * sqrt(252),
    
    VaR_95 = apply(r, 2, function(x) PerformanceAnalytics::VaR(x, p = 0.95, method = "historical")),
    
    ES_95 = apply(r, 2, function(x) PerformanceAnalytics::ES(x, p = 0.95, method = "historical"))
    
  )
}

base_risk_metrics <- risk_summary(portfolio_returns)

library(knitr)
kable(base_risk_metrics, caption = "Base Risk Measures under Normal Market Conditions")


#VaR VISUALIZATION (EXTREMELY USEFUL)
ggplot(data.frame(portfolio_returns), aes(x = portfolio_returns)) +
  geom_histogram(bins = 50, fill = "violet", alpha = 0.7) +
  geom_vline(xintercept = VaR_95, color = "red", linetype = "dashed", lwd = 1) +
  geom_vline(xintercept = VaR_99, color = "darkred", linetype = "dashed", lwd = 1) +
  labs(
    title = "VaR Visualization",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal()

#STRESS TESTING 

stress_shock <- function(data, shock_vector){
  mat <- as.matrix(data)
  mat * matrix(shock_vector,
               nrow = nrow(mat),
               ncol = ncol(mat),
               byrow = TRUE)
}


#----- SCENARIO 1: CRASH -----
shock_crash <- c(-2, -2, -2, -2)
names(shock_crash) <- tickers

crash <- returns_wide
crash[, tickers] <- stress_shock(crash[, tickers], shock_crash)

crash_portfolio <- as.matrix(crash[, tickers]) %*% weights


#---- SCENARIO 2: RATE SHOCK ----
shock_rate <- c(-1.5, -0.5, -1.8, -1.2)
names(shock_rate) <- tickers

rate <- returns_wide
rate[, tickers] <- stress_shock(rate[, tickers], shock_rate)

rate_portfolio <- as.matrix(rate[, tickers]) %*% weights

#---- SCENARIO 3: EM SHOCK ----
shock_em <- c(-1.2, -1.1, -1.3, -3.0)
names(shock_em) <- tickers

em <- returns_wide
em[, tickers] <- stress_shock(em[, tickers], shock_em)

em_portfolio <- as.matrix(em[, tickers]) %*% weights

#STRESS COMPARISON TABLE
risk_summary <- data.frame(
  Scenario = c("Base", "Crash", "Rate", "EM"),
  VaR_95 = c(
    VaR_95,
    quantile(crash_portfolio, 0.05),
    quantile(rate_portfolio, 0.05),
    quantile(em_portfolio, 0.05)
  ),
  VaR_99 = c(
    VaR_99,
    quantile(crash_portfolio, 0.01),
    quantile(rate_portfolio, 0.01),
    quantile(em_portfolio, 0.01)
  )

)

risk_dashboard <- data.frame(
  Metric = c(
    "VaR 95% (1D)", "VaR 95% (10D)", "VaR 95% (21D)",
    "VaR 99% (1D)", "VaR 99% (10D)", "VaR 99% (21D)",
    "ES 95%", "ES 99%"
  ),
  
  Value = c(
    VaR_95, VaR_95_10d, VaR_95_21d,
    VaR_99, VaR_99_10d, VaR_99_21d,
    ES_95, ES_99
  )
)

risk_dashboard

install.packages("gt")
library(gt)

gt_table <- gt(risk_dashboard)
gtsave(gt_table, "outputs/risk_dashboard.png")

#Value at Risk Table of different scenarios at 95% Confidence
horizons <- c(1,10,22)# 1-day, 10-day, ~1-month
horizons

var_table <- expand.grid(
  Scenario = c("Base", "Market Crash", "Rate Shock", "EM Shock"),
  Horizon = horizons
)

# 1 Day VaR inputs
base_var_95 <- VaR_95
crash_var_95 <- quantile(crash_portfolio, 0.05)
rate_var_95  <- quantile(rate_portfolio, 0.05)
em_var_95    <- quantile(em_portfolio, 0.05)

# Map scenario VaR
var_table$VaR_95_scaled <- c(
  base_var_95,
  crash_var_95,
  rate_var_95,
  em_var_95
)

# Scale for horizons
var_table$VaR_95_Return <- var_table$VaR_95_scaled * sqrt(var_table$Horizon)

# Convert to money
var_table$VaR_95_Money <- var_table$VaR_95_Return * portfolio_value

# Clean formatting
var_table$VaR_95_Return <- round(var_table$VaR_95_Return, 4)
var_table$VaR_95_Money  <- round(var_table$VaR_95_Money, 0)

library(dplyr)

var_table <- var_table %>%
  arrange(Scenario, Horizon) 
var_table

gt_table <- gt(var_table)
gtsave(gt_table, "outputs/scaled_var_metrics_combining_scenarios.png")

#VAR STRESS COMPARISON PLOT

ggplot(risk_summary, aes(x = Scenario, y = VaR_95, fill = Scenario)) +
  geom_col(width = 0.6) +
  
  coord_flip() +
  
  labs(
    title = "Value at Risk (95%) Across Stress Scenarios",
    x = "Scenario",
    y = "VaR (Return)"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

#VAR percent values
ggplot(risk_summary, aes(x = Scenario, y = VaR_95, fill = Scenario)) +
  geom_col(width = 0.6) +
  
  geom_text(aes(label = round(VaR_95, 4)),
            hjust = -0.2,
            size = 4) +
  
  coord_flip() +
  
  labs(
    title = "Value at Risk (95%) Across Stress Scenarios",
    x = "Scenario",
    y = "VaR (Return)"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#VAR in money terms
var_money <- data.frame(
  Scenario = c("Base", "Crash", "Rate", "EM"),
  VaR_95 = c(
    VaR_95,
    quantile(crash_portfolio, 0.05),
    quantile(rate_portfolio, 0.05),
    quantile(em_portfolio, 0.05)
  ),
  VaR_Money = c(
    base_var_95*portfolio_value,
    crash_var_95*portfolio_value,
    rate_var_95*portfolio_value,
    em_var_95*portfolio_value
  )
  
)

var_money 

ggplot(var_money , aes(x = Scenario, y = VaR_Money, fill = Scenario)) +
  geom_col(width = 0.6) +
  
  geom_text(aes(label = scales::comma(round(VaR_Money, 0))),
            hjust = -0.2,
            size = 4) +
  
  coord_flip() +
  
  labs(
    title = "Portfolio's 1 Day Value at Risk at 95%
               Normal vs Stress Scenarios",
    x = "Scenario",
    y = "VaR (Currency in $)"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#export all the table outputs in one excel file
install.packages("openxlsx")

var_table <- as.data.frame(var_table)
attr(var_table, "out.attrs") <- NULL

var_table$Scenario <- as.character(var_table$Scenario)

library(openxlsx)

dir.create("outputs", showWarnings = FALSE)

wb <- createWorkbook()

addWorksheet(wb, "Portfolio Risk")
writeData(wb, "Portfolio Risk", base_risk_metrics)

addWorksheet(wb, "VaR Horizons")
writeData(wb, "VaR Horizons", VaR_horizons)

addWorksheet(wb, "VaR Money")
writeData(wb, "VaR Money", var_money)

addWorksheet(wb, "Risk Dashboard")
writeData(wb, "Risk Dashboard", risk_dashboard)

addWorksheet(wb, "VaR Scenarios")
writeData(wb, "VaR Scenarios", var_table)

saveWorkbook(wb, "outputs/portfolio_risk_report.xlsx", overwrite = TRUE)


#########################
#MONTE CARLO SIMULATION
#########################

#MULTI-VARIATE NORMAL MONTE CARLO

library(MASS)

mu <- colMeans(returns_wide[, tickers])
sigma <- cov(returns_wide[, tickers])

set.seed(47)

sim <- mvrnorm(10000, mu = mu, Sigma = sigma)

sim_portfolio <- sim %*% weights

MC_VaR_95 <- quantile(sim_portfolio, 0.05)
MC_VaR_95

MC_VaR_99 <- quantile(sim_portfolio, 0.01)
MC_VaR_99

#Multivariate Plot simulation distribution
library(ggplot2)

ggplot(data.frame(sim_portfolio),
       aes(x = sim_portfolio)) +
  geom_histogram(bins = 60, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = MC_VaR_95, color = "yellow") +
  geom_vline(xintercept = MC_VaR_99, color = "red") +
  labs(
    title = "Monte Carlo Simulated Portfolio Returns 
        with 95% and 99% VAR ",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)


#STUDENT-T MONTE CARLO SIMULATION

library(MASS)

mu <- colMeans(returns_wide[, tickers])
sigma <- cov(returns_wide[, tickers])
weights <- c(0.25, 0.25, 0.25, 0.25)

set.seed(65)

n_sim <- 10000
df <- 5   # degrees of freedom (controls fat tails)

Z <- matrix(rt(n_sim * length(tickers), df = df),
            ncol = length(tickers))

chol_sigma <- chol(sigma)

sim_t <- Z %*% chol_sigma

sim_t <- sweep(sim_t, 2, mu, "+")

sim_portfolio_t <- sim_t %*% weights

MC_t_VaR_95 <- quantile(sim_portfolio_t, 0.05)
MC_t_VaR_95

MC_t_VaR_99 <- quantile(sim_portfolio_t, 0.01)
MC_t_VaR_99

MC_t_ES_95 <- mean(sim_portfolio_t[sim_portfolio_t <= MC_t_VaR_95])
MC_t_ES_95

MC_t_ES_99 <- mean(sim_portfolio_t[sim_portfolio_t <= MC_t_VaR_99])
MC_t_ES_99


#Student-t Plot simulation distribution
library(ggplot2)

ggplot(data.frame(sim_portfolio_t),
       aes(x = sim_portfolio_t)) +
  geom_histogram(bins = 80, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = MC_t_VaR_95, color = "yellow") +
  geom_vline(xintercept = MC_t_VaR_99, color = "red") +
  labs(
    title = "Student T MC Simulated Portfolio Returns 
        with 95% and 99% VAR ",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)


#MONTE CARLO SIMULATED RETURNS COMPARISON
df_plot <- data.frame(
  Normal = sim_portfolio,
  StudentT = sim_portfolio_t
)

ggplot(df_plot) +
  geom_density(aes(x = Normal), color = "blue", lwd = 1) +
  geom_density(aes(x = StudentT), color = "red", lwd = 1) +
  labs(
    title = "Monte Carlo: Normal vs Student-t",
    x = "Returns",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# HISTORICAL vs MONTE CARLO VAR COMPARISON PLOT

comparison_df <- tibble(
  Returns = c(portfolio_returns, sim_portfolio, sim_portfolio_t),
  Type = c(rep("Historical", length(portfolio_returns)),
           rep("Multivariate MC", 10000),
           rep ("Student-t", 10000))
)

ggplot(comparison_df, aes(x = Returns, fill = Type)) +
  geom_density(alpha = 0.4) +
  labs(title = "Historical vs Multivariate MC vs 
       Student-t MC Returns") +
  theme_minimal(base_size = 14)





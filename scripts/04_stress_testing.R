############################################################
# STRESS TESTING MODEL 
# SCRIPT 4: Scenario Analysis & Stress Testing
# Data Source: Yahoo Finance
# Tools: R Packages
# Author: Maina Silvia
############################################################

library(dplyr)

select <- dplyr::select
mutate <- dplyr::mutate
filter <- dplyr::filter

# =============================
# SCENARIO ANALYSIS
# =============================

#---- Scenario 1: Market Crash (-20%) ----
library(dplyr)

crash <- returns_wide

crash$AAPL <- crash$AAPL * -2
crash$TSLA <- crash$TSLA * -2
crash$JPM  <- crash$JPM  * -2
crash$VWO  <- crash$VWO  * -2

crash$portfolio_return <- rowMeans(
  crash[, c("AAPL","TSLA","JPM","VWO")],
  na.rm = TRUE
)

# ---- Scenario 2: Interest Rate Shock ----
library(dplyr)

rate <- returns_wide

rate$AAPL <- rate$AAPL * -1.5
rate$TSLA <- rate$TSLA * -1.8
rate$JPM  <- rate$JPM  * -0.5
rate$VWO  <- rate$VWO  * -1.2

rate$portfolio_return <- rowMeans(
  rate[, c("AAPL","TSLA","JPM","VWO")],
  na.rm = TRUE
)

# ---- Scenario 3: Emerging Market Crisis ----
em_crisis <- returns_wide

em_crisis$VWO  <- em_crisis$VWO  * -3
em_crisis$AAPL <- em_crisis$AAPL * -1.2
em_crisis$TSLA <- em_crisis$TSLA * -1.3
em_crisis$JPM  <- em_crisis$JPM  * -1.1

em_crisis$portfolio_return <- rowMeans(
  em_crisis[, c("AAPL","TSLA","JPM","VWO")],
  na.rm = TRUE
)


# ====================
# STRESS COMPARISON 
# ====================

#Stress Comparison 

stress_plot <- data.frame(
  Scenario = c("Baseline", "Market Crash", "Interest Rate Shock", "Emerging Market Crisis"),
  Volatility = c(
    sd(returns_wide$portfolio_return),
    sd(crash$portfolio_return),
    sd(rate$portfolio_return),
    sd(em_crisis$portfolio_return)
  )
)

ggplot(stress_plot, aes(x = Scenario, y = Volatility, fill = Scenario)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(
    title = "Portfolio Volatility Under Stress Scenarios",
    y = "Volatility"
  )

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/portfolio volatility under stress.png")


#Compute VaR and ES (FOR ALL SCENARIOS)
alpha <- 0.05

risk_summary <- data.frame(
  Scenario = c("Baseline", "Market Crash", "Interest Rate Shock", "Emerging Market Crisis"),
  
  VaR = c(
    quantile(returns_wide$portfolio_return, alpha, na.rm = TRUE),
    quantile(crash$portfolio_return, alpha, na.rm = TRUE),
    quantile(rate$portfolio_return, alpha, na.rm = TRUE),
    quantile(em_crisis$portfolio_return, alpha, na.rm = TRUE)
  ),
  
  ES = c(
    mean(returns_wide$portfolio_return[returns_wide$portfolio_return
        <= quantile(returns_wide$portfolio_return, alpha, na.rm = TRUE)], na.rm = TRUE),
    mean(crash$portfolio_return[crash$portfolio_return
        <= quantile(crash$portfolio_return, alpha, na.rm = TRUE)], na.rm = TRUE),
    mean(rate$portfolio_return[rate$portfolio_return 
        <= quantile(rate$portfolio_return, alpha, na.rm = TRUE)], na.rm = TRUE),
    mean(em_crisis$portfolio_return[em_crisis$portfolio_return 
        <= quantile(em_crisis$portfolio_return, alpha, na.rm = TRUE)], na.rm = TRUE)
  )
)


#Convert to LONG format

library(tidyr)

risk_long <- risk_summary %>%
  pivot_longer(cols = c(VaR, ES),
               names_to = "Measure",
               values_to = "Value")

#Combined VaR + ES under stress Plot
ggplot(risk_long, aes(x = Scenario, y = Value, fill = Measure)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparing Baseline VaR and ES with
         All Simulated Stress Scenarios",
    x = "",
    y = "Loss",
    fill = "Risk Measure"
)


ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/portfolio VAR & ES stress comparison.png")


#compute BASE RISK statistics for baseline and per simulated scenario
summary_stats <- function(x){
  c(
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

#baseline
summary_stats(returns_wide$portfolio_return)

#market crash
summary_stats(crash$portfolio_return)

#interest rate shock
summary_stats(rate$portfolio_return)

#emerging markets crisis
summary_stats(em_crisis$portfolio_return)


#compute DOWNSIDE RISK METRICS VaR + ES per scenario
risk_metrics <- function(x){
  VaR_95 <- quantile(x, 0.05, na.rm = TRUE)
  ES_95 <- mean(x[x <= VaR_95], na.rm = TRUE)

  c(
    VaR_95 = VaR_95,
    ES_95 = ES_95
  )
}

#baseline downside
risk_metrics(returns_wide$portfolio_return)

#market crash downside
risk_metrics(crash$portfolio_return)

#interest rate shock downside
risk_metrics(rate$portfolio_return)

#emerging markets crisis downside
risk_metrics(em_crisis$portfolio_return)

summary(returns_wide$portfolio_return)
summary(crash$portfolio_return)
summary(rate$portfolio_return)
summary(em_crisis$portfolio_return)


#TAIL SEVERITY COMPARISON
tail_severity <- data.frame(
  Scenario = c("Baseline", "Crash", "Rate Shock", "EM Crisis"),
  VaR_95 = c(
    quantile(returns_wide$portfolio_return, 0.05),
    quantile(crash$portfolio_return, 0.05),
    quantile(rate$portfolio_return, 0.05),
    quantile(em_crisis$portfolio_return, 0.05)
  )
)

tail_severity

#WORST-DAY COMPARISON 
worst_days <- data.frame(
  Scenario = c("Baseline", "Crash", "Rate", "EM"),
  Worst_Day = c(
    min(returns_wide$portfolio_return),
    min(crash$portfolio_return),
    min(rate$portfolio_return),
    min(em_crisis$portfolio_return)
  )
)
worst_days


#PROBABILITY OF EXTREME LOSS

extreme_loss_prob <- function(x, threshold){
  mean(x <= threshold, na.rm = TRUE)
}

base_var <- quantile(returns_wide$portfolio_return, 0.05, na.rm = TRUE)

c(
  Baseline = extreme_loss_prob(returns_wide$portfolio_return, base_var),
  Crash = extreme_loss_prob(crash$portfolio_return, base_var),
  Rate = extreme_loss_prob(rate$portfolio_return, base_var),
  EM = extreme_loss_prob(em_crisis$portfolio_return, base_var)
)

base_var


###############################################################
# MONTE CARLO SIMULATION FOR POSSIBLE FUTURE PORTFOLIO RETURNS
###############################################################

library(MASS)

# Extract return matrix (remove date column)
returns_matrix <- returns_wide %>%
  dplyr::select(AAPL, TSLA, JPM, VWO)


# Estimate parameters
mu <- colMeans(returns_matrix)
sigma <- cov(returns_matrix)

mu

sigma


# Simulation settings
set.seed(123)
n_sim <- 10000   # number of simulations
n_assets <- ncol(returns_matrix)

# Simulate returns
simulated_returns <- mvrnorm(n = n_sim, mu = mu, Sigma = sigma)

# Convert to tibble
sim_df <- as_tibble(simulated_returns)
colnames(sim_df) <- colnames(returns_matrix)

# Compute portfolio returns (equal weights)
sim_df <- sim_df %>%
  mutate(portfolio = rowMeans(across(everything())))


# MONTE CARLO DISTRIBUTION VISUALIZATION

ggplot(sim_df, aes(x = portfolio)) +
  geom_histogram(bins = 60) +
  labs(title = "Monte Carlo Simulated Portfolio Returns",
       x = "Returns",
       y = "Frequency") +
  theme_minimal(base_size = 14)


ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/MC simulated portfolio returns.png")


# MONTE CARLO VaR ESTIMATION

MC_VaR_95 <- quantile(sim_df$portfolio, 0.05)
MC_VaR_99 <- quantile(sim_df$portfolio, 0.01)

MC_VaR_95
MC_VaR_99


#Plot simulation distribution
library(ggplot2)

ggplot(data.frame(sim_df$portfolio),
       aes(x = sim_df$portfolio)) +
  geom_histogram(bins = 60, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = MC_VaR_95, color = "yellow") +
  geom_vline(xintercept = MC_VaR_99, color = "red") +
  labs(
    title = "Monte Carlo Simulated Portfolio Returns with 95% and 99% VAR ",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/MC simulated portfolio VAR.png")


# HISTORICAL vs MONTE CARLO VAR COMPARISON PLOT

comparison_df <- tibble(
  Returns = c(portfolio_returns, sim_df$portfolio),
  Type = c(rep("Historical", length(portfolio_returns)),
           rep("Monte Carlo", n_sim))
)

ggplot(comparison_df, aes(x = Returns, fill = Type)) +
  geom_density(alpha = 0.4) +
  labs(title = "Historical vs Monte Carlo Returns") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/Historcal vs MC stock returns.png")


#EXTREME LOSSES

# Worst 1% outcomes
extreme_losses <- sim_df %>%
  filter(portfolio <= quantile(portfolio, 0.01))

summary(extreme_losses$portfolio)


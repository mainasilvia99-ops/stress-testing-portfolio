############################################################
# STRESS TESTING MODEL 
# SCRIPT 3: Portfolio Construction & Risk Visualization
# Data Source: Yahoo Finance
# Tools: R Packages
# Author: Maina Silvia
############################################################

#Step 1: assign weights to the assets and construct an equal weighted portfolio

#Step 2: analyze portfolio returns using base risk metrics and visualize it

# =========================
# PORTFOLIO CONSTRUCTION
# =========================

#Assign equal portfolio weights
weights <- c(0.25, 0.25, 0.25, 0.25)

#reshape data
returns_wide$portfolio_return <- rowSums(
  returns_wide[, -1] * weights
)

returns_wide <- returns_wide %>%
  dplyr::mutate(portfolio = rowMeans(dplyr::select(., -date)))

#Base risk measures estimation
portfolio_returns <- returns_wide$portfolio_return

mean_return <- mean(portfolio_returns)
mean_return

sd_return <- sd(portfolio_returns)
sd_return

#VAR ESTIMATION
#confidence levels

alpha_95 <- 0.05
alpha_99 <- 0.01

# VaR
VaR_95 <- quantile(returns_wide$portfolio_return, alpha_95, na.rm = TRUE)
VaR_95

VaR_99 <- quantile(returns_wide$portfolio_return, alpha_99, na.rm = TRUE)
VaR_99

# ES
ES_95 <- mean(returns_wide$portfolio_return[
        returns_wide$portfolio_return <= VaR_95],
          na.rm = TRUE)
ES_95

ES_99 <- mean(returns_wide$portfolio_return[
         returns_wide$portfolio_return <= VaR_99], 
          na.rm = TRUE)

ES_99

#Risk measures summary table
risk_summary <- data.frame(
     Metric = c("Mean Return", "Std Dev", "VaR 95%", "VaR 99%", "ES 95%", "ES 99%"),
     Value = c(mean_return, sd_return, VaR_95, VaR_99, ES_95, ES_99)
)

print(risk_summary)

# ====================================
# PORTFOLIO RETURN RISK VISUALIZATION
# ====================================

#Plot Portfolio Returns
ggplot(returns_wide, 
   aes(x = date, y = portfolio_return)) +
   geom_line(color = "green", linewidth = 0.6) +
   labs(
    title = "Portfolio Returns Over Time",
    x = "Date",
    y = "Returns"
  ) +
  theme_minimal(base_size = 14)

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/returns_wide.png")


#Plot portfolio returns Gains vs Losses (Risk Colour Coding)
returns_wide$sign <- ifelse(returns_wide$portfolio_return >= 0, "Gain", "Loss")

returns_wide$sign <- ifelse(
    returns_wide$portfolio_return >= 0, "Gain", "Loss"
)

ggplot(returns_wide, 
  aes(x = date, y = portfolio_return, color = sign)) +
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

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/portfolio returns.png")


#PLOT EXTREME EVENTS (STRESS)
threshold <- sd(returns_wide$portfolio_return) * 2

returns_wide$stress <- abs(returns_wide$portfolio_return) > threshold

ggplot(returns_wide, 
  aes(x = date, y = portfolio_return)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(data = subset(returns_wide, stress),
             color = "red", size = 2) +
  labs(
    title = " Extreme Market Movements (Stress Events)",
    x = "Date",
    y = "Returns"
  ) +
  theme_minimal(base_size = 14)

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/stress events.png")


#Volatility clustering plot
ggplot(returns_wide, aes(x = date, y = portfolio_return)) +
  geom_line(color = "blue", linewidth = 0.5) +
  geom_point(
       data = subset(returns_wide, stress),
       color = "red", alpha = 0.6, size = 1.5
  ) +
  geom_ribbon(
       aes(ymin = 0, ymax = portfolio_return),
              fill = "blue", alpha = 0.1
  ) +
  labs(
    title = "Volatility Clustering in the Portfolio Returns",
    x = "Date",
    y = "Returns"
  ) +
  theme_minimal(base_size = 14)

ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/volatility clustering.png")


#PORTFOLIO VAR PLOTS VISUALIZATION

ggplot(returns_wide, aes(x = portfolio_return)) +
  
  geom_histogram(bins = 60, fill = "violet", color = "white", alpha = 0.8) +
  
  geom_vline(aes(xintercept = VaR_95, color = "VaR 95%"),
             linetype = "dashed", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = VaR_99, color = "VaR 99%"),
             linetype = "dotted", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = ES_95, color = "ES 95%"),
             linetype = "solid", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = ES_99, color = "ES 99%"),
             linetype = "longdash", linewidth = 1.2) +
  
  scale_color_manual(values = c(
    "VaR 95%" = "gold",
    "VaR 99%" = "orange",
    "ES 95%"  = "red",
    "ES 99%"  = "darkred"
  )) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Portfolio Return Distribution 
            with VaR and ES ",
    x = "Portfolio Returns",
    y = "Frequency",
    color = "Risk Measures" +

    geom_density(color = "black", linewidth = 1, alpha = 0.3)
  )


ggsave("C:/Users/Admin/Desktop/GITHUB/stress testing portfolio project/outputs/graphical plots/portfolio VAR & ES.png")


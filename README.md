# stress-testing-portfolio
This project investigates how a multi-asset portfolio behaves under extreme market conditions using stress testing and Monte Carlo simulation.
This model demonstrates how institutional investors evaluate portfolio resilience under extreme conditions and highlights the limitations of traditional risk metrics.

Overview
This project develops a financial risk analysis pipeline to evaluate how a multi-asset portfolio behaves under both normal and extreme market conditions.

It combines:
•	Historical risk analysis
•	Scenario-based stress testing
•	Monte Carlo simulation

The objective is to quantify downside risk, tail exposure, and portfolio vulnerability using real market data.

Tools & Technologies
•	R (dplyr, ggplot2, MASS, tidyr)
•	MySQL (data storage)
•	Yahoo Finance (data source)

Portfolio Composition
•	Apple stock (AAPL) – Large-cap technology
•	JP Morgan Stock (JPM) – Banking sector
•	Tesla Stock (TSLA) – High-growth / high-volatility
•	Vanguard FTSE EFT Stock (VWO) – Emerging markets ETF
Equal-weighted portfolio (25% each)

Methodology
1. Data Pipeline
•	Data extraction via Yahoo Finance API
•	Storage in MySQL database
•	Retrieval and transformation in R
2. Portfolio Construction
•	Log returns computed for each asset
•	Portfolio returns calculated using equal weights
3. Risk Metrics
•	Value at Risk (VaR)
•	Expected Shortfall (ES)
•	Volatility and distribution analysis
4. Stress Testing Scenarios
•	Market Crash (-20% shock across assets)
•	Interest Rate Shock (sector-specific impact)
•	Emerging Market Crisis (localized shock to VWO)
5. Monte Carlo Simulation
•	10,000 simulated scenarios
•	Multivariate normal distribution
•	Portfolio return distribution analysis

Key Observations
•	Portfolio risk is non-linear under stress
•	Correlations increase during crises, reducing diversification
•	Tail risk is substantial and cannot be captured by volatility alone
•	Historical models underestimate extreme losses

Key Results
•	Stress scenarios significantly increase downside risk
•	Market crash produces the most severe losses
•	Diversification benefits weaken during systemic shocks
•	Expected Shortfall consistently exceeds VaR, highlighting tail risk
•	Monte Carlo simulation confirms risk but may underestimate extreme events

Key Insights
- Market crash produced the largest tail risk
- Diversification breaks down during stress
- Expected Shortfall exceeds VaR meaning significant tail exposure
   
How to Run
1.	Clone the repository
2.	Open R project
3.	Run scripts in order:
01_data_extraction.R  
02_returns_computation.R  
03_portfolio_construction.R  
04_stress_testing.R  

Skills Demonstrated
•Financial risk modeling
•Data engineering (SQL + R pipeline)
•Statistical analysis
•Monte Carlo simulation
•Scenario-based stress testing

Limitations: The model assumes normally distributed returns, which may underestimate extreme market events.

Future Improvements
•Using fat-tailed distributions (t-distribution) 
•Employing volatility modeling (GARCH)
•Dynamic portfolio optimization
•Macro-driven stress scenarios

Author
Maina Silvia

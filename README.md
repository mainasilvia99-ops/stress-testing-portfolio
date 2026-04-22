#  Portfolio Stress Testing & Scenario Analysis

##  Overview

This project develops a comprehensive **portfolio risk and stress testing framework** to evaluate how financial assets behave under both normal and extreme market conditions.

Traditional risk models such as Value at Risk (VaR) often underestimate losses during periods of financial stress. This project addresses these limitations by integrating:

* Historical risk analysis
* Scenario-based stress testing
* Monte Carlo simulation (Normal & Student-t)

The result is a **fully reproducible risk analytics pipeline** implemented in R.

##  Key Objectives

* Quantify portfolio risk under normal market conditions
* Evaluate downside risk using VaR and Expected Shortfall (ES)
* Analyze portfolio behavior under macroeconomic stress scenarios
* Compare risk estimates across different statistical models
* Demonstrate the impact of fat tails and model assumptions

## Portfolio Composition

An equally weighted portfolio valued arbitrarily at 1M$ consisting of:

* **AAPL** – Large-cap technology
* **TSLA** – High-growth, high-volatility
* **JPM** – Banking sector
* **VWO** – Emerging markets exposure
  
## Methodology

### Data

* Source: Yahoo Finance
* Frequency: Daily prices (from 2020 onward)
* Storage: MySQL database for reproducibility

### Risk Measures

* Mean Return
* Volatility (Standard Deviation)
* Skewness & Kurtosis
* Value at Risk (VaR)
* Expected Shortfall (ES)

### Stress Testing Scenarios

* Market Crash (-20%)
* Interest Rate Shock
* Emerging Market Crisis

### Monte Carlo Simulation

* 10,000 simulated scenarios
* Multivariate Normal distribution
* Multivariate Student-t distribution (fat tails)

## Key Results

### Base Risk Metrics Profile

* Annualized volatility: **~29%**
* Negative skewness → **downside asymmetry**
* High kurtosis (~8.6) → **fat tails / extreme events**

###  Tail Risk Insights

* VaR (95%): **~2.75% daily loss**
* Expected Shortfall: significantly higher → **tail losses are severe**

> VaR underestimates extreme risk — ES provides a more realistic measure.

### Stress Testing Results

* Market crash scenario produces **2x loss amplification**
* Portfolio losses exceed **25% over a 1-month horizon** under stress
* Diversification weakens significantly during systemic shocks

###  Model Comparison 

| Model                 | VaR 95% | VaR 99% |
| --------------------- | ------- | ------- |
| Normal Monte Carlo    | -2.95%  | -4.14%  |
| Student-t Monte Carlo | -3.63%  | -5.59%  |

Student-t captures **fat tails and extreme losses**
Normal distribution **underestimates tail risk**

This highlights the importance of model selection in financial risk management.

## Key Takeaways

* Financial returns are **non-normal and heavy-tailed**
* Tail events dominate portfolio risk
* Diversification is **state-dependent**
* VaR alone is insufficient → ES + stress testing are essential
* Model assumptions significantly impact risk estimates

##  Limitations

* Assumes static portfolio weights
* Relies partly on historical data
* Normal Monte Carlo underestimates extreme events

## Future Improvements

* Extreme Value Theory (EVT) for tail modeling
* GARCH models for volatility clustering
* Dynamic portfolio optimization
* Macro-driven scenario modeling

##  Project Structure
portfolio-stress-testing/
├── README.md
├── scripts/
│   └── main_pipeline.R
├── data/
│   └──financedata_db_backup.sql
│   └── sql_queries_financedata_db.sql
│   └── stock_returns_data.pdf
│   └── thefour_stock_prices_raw_data.pdf
├── outputs/
│   └── graphical plots
│   └── table results


## Tech Stack

* **R** (quantmod, PerformanceAnalytics, e1071, MASS)
* **SQL (MySQL)** for data storage
* **Excel** for reporting

## How to Run

1. Clone the repository
2. Open scripts/main_pipeline.R
3. Run the script to:

   * fetch data
   * compute risk metrics
   * generate stress scenarios
   * export results

## Why This Project Matters

This project demonstrates practical skills in:

* Quantitative risk modeling
* Financial data analysis
* Scenario-based stress testing
* Model validation and comparison
* Building reproducible analytical pipelines

## Author
**Maina Silvia**
Aspiring Quant / Financial Engineer

## Final Insight
Portfolio risk is driven not by average outcomes, but by extreme events — making tail risk modeling essential for real-world financial decision-making.

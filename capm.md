
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
#fill the code
df <- df %>%
  mutate(
    AMD_return = (AMD - lag(AMD)) / lag(AMD),
    GSPC_return = (GSPC - lag(GSPC)) / lag(GSPC)
  )
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#fill the code
annual_rf_rate <- 5 / 100  # 5% annual rate
df <- df %>%
  mutate(daily_rf_rate = (1 + annual_rf_rate)^(1/360) - 1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#fill the code
df <- df %>%
  mutate(
    AMD_excess_return = AMD_return - daily_rf_rate,
    GSPC_excess_return = GSPC_return - daily_rf_rate
  )
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#fill the code
capm_model <- lm(AMD_excess_return ~ GSPC_excess_return, data = df)
summary(capm_model)
beta <- coef(capm_model)[2]
beta
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The beta ($\beta$) of AMD was calculated to be approximately 1.57, indicating that AMD is more volatile than the market; for every 1% change in the S&P 500's excess return, AMD's excess return is expected to change by about 1.57%.

The $R^2$ value of 0.4017 indicates that approximately 40.17% of the variance in AMD's excess returns is explained by the S&P 500's excess returns. The adjusted $R^2$ value of 0.4013 shows minimal reduction when adjusting for the number of predictors.

The p-value for the GSPC_excess_return coefficient is less than 2e-16, indicating the statistical significance of the relationship between AMD's and the S&P 500's excess returns.

In summary, AMD's higher beta reflects its greater volatility relative to the market. The model explains a moderate portion of this variance, providing insights into AMD's systematic risk.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
#fill the code
ggplot(df, aes(x = GSPC_excess_return, y = AMD_excess_return)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "CAPM Regression Line",
    x = "S&P 500 Excess Return",
    y = "AMD Excess Return"
  )
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#fill the code
# Calculate daily standard deviation of excess returns
daily_sd <- sd(df$AMD_excess_return, na.rm = TRUE)
# Annual standard deviation
annual_sd <- daily_sd * sqrt(252)

# Expected market return
expected_return_sp500 <- 0.133
# Risk-free rate
risk_free_rate <- 0.05
# Expected return of AMD using CAPM formula
expected_return_amd <- risk_free_rate + beta * (expected_return_sp500 - risk_free_rate)

# Calculate 90% prediction interval
lower_bound <- expected_return_amd - 1.645 * annual_sd
upper_bound <- expected_return_amd + 1.645 * annual_sd

cat("Expected Return of AMD:", expected_return_amd, "\n")
cat("Annual Standard Deviation:", annual_sd, "\n")
cat("90% Prediction Interval for AMD's Annual Expected Return: [", lower_bound, ", ", upper_bound, "]\n")
```
### Results Interpretation

The expected return of AMD, calculated using the CAPM formula, is approximately 18.03%. The annual standard deviation of AMD's excess returns is 52.66%, indicating high volatility. The 90% prediction interval for AMD's annual expected return ranges from -68.60% to 104.66%.

This wide interval reflects significant uncertainty in AMD's future returns, consistent with its beta ($\beta$) of 1.57, which indicates higher volatility relative to the market. A higher beta suggests greater systematic risk, meaning AMD's returns are more sensitive and reactive to market movements.

Investors should be aware of this increased risk, as the potential for higher returns can also mean the possibility of significant losses. The model explains a moderate portion of AMD's return variance, indicating that while market performance is a significant factor, other variables still play a role in this statistic. This highlights the importance of a diversified investment approach to mitigate risk.

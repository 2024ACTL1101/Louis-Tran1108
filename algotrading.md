
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Fill your code here
for (i in 1:nrow(amd_df)) {
current_price <- amd_df$close[i]
   
   if (previous_price == 0) {
     # Initial buy
     amd_df$trade_type[i] <- 'buy'
     amd_df$costs_proceeds[i] <- -current_price * share_size
     accumulated_shares <- accumulated_shares + share_size
   } else if (current_price < previous_price) {
     # Buy condition
     amd_df$trade_type[i] <- 'buy'
     amd_df$costs_proceeds[i] <- -current_price * share_size
     accumulated_shares <- accumulated_shares + share_size
   }
   
   # Last day condition to sell
   if (i == nrow(amd_df)) {
     amd_df$trade_type[i] <- 'sell'
     amd_df$costs_proceeds[i] <- current_price * accumulated_shares
     accumulated_shares <- 0
   }
   
   amd_df$accumulated_shares[i] <- accumulated_shares
   previous_price <- current_price
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
# Define the start and end dates for the trading period
 start_date <- as.Date("2019-01-01")
 end_date <- as.Date("2020-01-01")
 
 # Subset the dataframe for the specified trading period
 amd_df <- subset(amd_df, date >= start_date & date <= end_date)
 
 # Display the first few rows of the subsetted dataframe to verify
 head(amd_df)
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Fill your code here
 # Calculate Total Profit/Loss
 total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
 
 # Calculate Total Invested Capital
 total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
 
 # Calculate ROI
 roi <- (total_profit_loss / total_invested_capital) * 100
 
 # Display the results
 cat("Total Profit/Loss: ", total_profit_loss, "\n")
 cat("Total Invested Capital: ", total_invested_capital, "\n")
 cat("ROI: ", roi, "%\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Fill your code here
## Option chosen: Profit-Taking Strategy
 # Define the start and end dates for the trading period
 start_date <- as.Date("2019-01-01")
 end_date <- as.Date("2020-01-01")
 
 # Subset the dataframe for the specified trading period
 amd_df <- subset(amd_df, date >= start_date & date <= end_date)
 
 # Calculate the average purchase price
 average_purchase_price <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE) /
   sum(amd_df$accumulated_shares[amd_df$trade_type == 'buy'], na.rm = TRUE)
 
 # Define the percentage increase for profit-taking
 percentage_increase <- 20 / 100
 
 # Initialize a column to store the action
 amd_df$action <- NA
 
 # Implement the profit-taking strategy
 for (i in 1:nrow(amd_df)) {
   if (!is.na(amd_df$close[i])) {
     if (amd_df$close[i] >= (average_purchase_price * (1 + percentage_increase))) {
       amd_df$action[i] <- "sell_half"
       # Adjust accumulated shares and costs_proceeds
       half_shares <- amd_df$accumulated_shares[i] / 2
       amd_df$costs_proceeds[i] <- amd_df$close[i] * half_shares
       amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i] - half_shares
     }
   }
 }
 
 # Display the dataframe to verify the profit-taking strategy
 print(amd_df)
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Disucss
# Calculate Total Profit/Loss
 total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
 
 # Calculate Invested Capital
 invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
 
 # Calculate ROI
 roi <- (total_profit_loss / invested_capital) * 100
 
 cat("Total Profit/Loss: ", total_profit_loss, "\n")
 cat("Invested Capital: ", invested_capital, "\n")
 cat("ROI: ", roi, "%\n")
 
 # Print the final dataframe for reference
 print(head(amd_df, 20))
```
Discussion:
During the trading period from January 1, 2019, to January 1, 2020, the ROI remained at -100%, indicating no improvement. This suggests that the stock price did not increase by 20% from the average purchase price at any point during the trading period, leading to no sell transactions being triggered. The maximum stock price during the period was below the profit-taking threshold, resulting in continuous buys and no opportunity to realize profits through sales. Consequently, the total profit/loss also did not improve. A relevant market event that could explain these results is the impact of the COVID-19 pandemic, which started affecting global markets significantly in early 2020. The pandemic created significant market volatility and uncertainty, likely preventing AMD's stock from achieving the necessary price increase for the profit-taking strategy to be effective. As a result, the strategy did not yield the desired outcomes during this tumultuous period, and both P/L and ROI remained unchanged.


Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.





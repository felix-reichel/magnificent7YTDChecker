library(quantmod)
library(bizdays)
library(stats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META", "GOOG")
weights <- c(0.0467, 0.0477, 0.0162, 0.0273, 0.0467, 0.0167, 0.0141)
scaled_weights <- weights / sum(weights)
msci_ticker <- "URTH"
ten_year_bond_ticker <- "DGS10" # 10-year US Treasury bond yield

### FUNCTIONS ######################################################
get_adjusted_prices <- function(ticker) {
  tryCatch({
    data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    if (!is.null(data) && nrow(data) > 0) {
      colname <- paste(ticker, "Adjusted", sep = ".")
      if (colname %in% colnames(data)) {
        return(data[, colname])
      } else {
        message(paste("Column", colname, "not found for", ticker))
      }
    } else {
      message(paste("No data found for", ticker))
    }
  }, error = function(e) {
    message(paste("Error downloading data for", ticker, ":", e$message))
  })
  return(NULL)
}

get_fred_data <- function(ticker) {
  tryCatch({
    data <- getSymbols(ticker, src = "FRED", auto.assign = FALSE)
    if (!is.null(data) && nrow(data) > 0) {
      return(data)
    } else {
      message(paste("No data found for", ticker))
    }
  }, error = function(e) {
    message(paste("Error downloading data for", ticker, ":", e$message))
  })
  return(NULL)
}

fetch_prices <- function(tickers) {
  prices_list <- lapply(tickers, get_adjusted_prices)
  do.call(merge, prices_list)
}

calculate_returns <- function(prices_df) {
  na.omit(ROC(prices_df, type = "discrete"))
}

calculate_portfolio_returns <- function(returns, weights) {
  rowSums(returns * weights)
}
###########################################################

magnificent7_prices <- fetch_prices(tickers)
msci_prices <- get_adjusted_prices(msci_ticker)
ten_year_bond_yield <- get_fred_data(ten_year_bond_ticker)

magnificent7_returns <- calculate_returns(magnificent7_prices)
msci_returns <- calculate_returns(msci_prices)
ten_year_bond_returns <- calculate_returns(ten_year_bond_yield) # Assuming we can use yield as price to calculate returns for bond

magnificent7_portfolio_returns <- calculate_portfolio_returns(magnificent7_returns, scaled_weights)

plot(magnificent7_portfolio_returns, type = "l")
plot(cumsum(magnificent7_portfolio_returns), type = "l")
plot(cumsum(msci_returns), type = "l")

current_date <- Sys.Date()
current_year <- format(current_date, "%Y")
start_of_year <- as.Date(paste0(current_year, "-01-01"))

all_dates <- seq.Date(from = start_of_year, to = current_date, by = "day")
weekends <- all_dates[format(all_dates, "%u") %in% c(6, 7)]

bank_holidays <- as.Date(c(
  "2024-01-01", 
  # UK dates
  "2024-03-29",  #29 March - Good Friday - FRIDAY
  "2024-04-01",  #1 April - Easter Monday - MONDAY
  "2024-05-06",  #6 May - Early May bank holiday - MONDAY
  "2024-05-27",  #27 May - Spring bank holiday - MONDAY
  # US dates
  "2024-02-19",  # Washington's Birthday (Presidents Day)	February 19
  "2024-05-27",  # Memorial Day	May 27	
  "2024-06-19",  # Juneteenth National Independence Day	June 19	
  "2024-07-04",   # Independence Day	July 4	
  # add upcoming ytd
  "2024-12-25"))

non_business_days <- unique(c(weekends, bank_holidays))

cal <- create.calendar("UnitedStates", 
                       holidays = non_business_days, 
                       weekdays = c(
                         "saturdays", 
                         "sundays")
)
bizseq <- bizseq(start_of_year, 
                 current_date, 
                 cal)

unknown_bank_holidays_amount <- 1
idx_mag7 <- length(magnificent7_portfolio_returns) - length(bizseq) + unknown_bank_holidays_amount
idx_msci <- length(msci_returns) - length(bizseq) + unknown_bank_holidays_amount
idx_bond <- length(ten_year_bond_returns) - length(bizseq) + unknown_bank_holidays_amount

magnificent7_portfolio_ret <- magnificent7_portfolio_returns[idx_mag7:length(magnificent7_portfolio_returns)]
msci_ret <- msci_returns[idx_msci:length(msci_returns)]
bond_ret <- ten_year_bond_returns[idx_bond:length(ten_year_bond_returns)]


### YTD Plots: magnificent7_portfolio_ret vs msci_ret vs 10yr US bond ###

plot(cumsum(magnificent7_portfolio_ret), type = "l", col = "red", ylab = "Cumulative Returns", xlab = "Date", main = "Cumulative Returns of Portfolios and 10Y US Bond")
lines(cumsum(as.numeric(msci_ret$URTH.Adjusted)), type = "l", col = "blue")
lines(cumsum(as.numeric(bond_ret$DGS10)), type = "l", col = "green")

legend("topleft", legend = c("Magnificent 7", "MSCI", "10Y US Bond"), col = c("red", "blue", "green"), lty = 1)

data <- data.frame(Magnificent7 = magnificent7_portfolio_ret, MSCI = msci_ret, Bond = bond_ret)

avg_magnificent7_return <- mean(data$Magnificent7, na.rm = TRUE)
avg_msci_return <- mean(data$URTH.Adjusted, na.rm = TRUE)
avg_bond_return <- mean(data$DGS10, na.rm = TRUE)

print(avg_magnificent7_return)
print(avg_magnificent7_return * length(data$Magnificent7))

print(avg_msci_return)
print(avg_msci_return * length(data$URTH.Adjusted))

print(avg_bond_return)
print(avg_bond_return * length(data$DGS10))




# ACF and CCF plots for 3 TS data
ts_mag7_ytd <- as.ts(data$Magnificent7)
ts_msci_ytd <- as.ts(data$URTH.Adjusted)
ts_dgs10_ytd <- as.ts(data$DGS10)

par(mfrow = c(3, 1))
acf(ts_mag7_ytd, main = "ACF for Magnificent 7 Returns")
acf(ts_msci_ytd, main = "ACF for MSCI Returns")
acf(ts_dgs10_ytd, main = "ACF for 10Y US Bond Returns")

par(mfrow = c(3, 1))
ccf(ts_mag7_ytd, ts_msci_ytd, main = "Cross-correlation: Magnificent 7 and MSCI")
ccf(ts_msci_ytd, ts_dgs10_ytd, main = "Cross-correlation: Magnificent 7 and 10Y US Bond")
ccf(ts_dgs10_ytd, ts_dgs10_ytd, main = "Cross-correlation: MSCI and 10Y US Bond")

length(ts_mag7_ytd)
length(bizseq)




# Other Economic TS data from FRED
gdp <- get_fred_data("GDP")
unemployment_rate <- get_fred_data("UNRATE")
inflation_rate <- get_fred_data("CPIAUCSL")


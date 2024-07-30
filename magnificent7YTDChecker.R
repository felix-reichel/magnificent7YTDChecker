library(quantmod)
library(bizdays)

tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META", "GOOG")
weights <- c(0.0467, 0.0477, 0.0162, 0.0273, 0.0467, 0.0167, 0.0141)
scaled_weights <- weights / sum(weights)
msci_ticker <- "URTH"

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

magnificent7_prices <- fetch_prices(tickers)
msci_prices <- get_adjusted_prices(msci_ticker)

magnificent7_returns <- calculate_returns(magnificent7_prices)
msci_returns <- calculate_returns(msci_prices)

magnificent7_portfolio_returns <- calculate_portfolio_returns(magnificent7_returns, scaled_weights)

plot(magnificent7_portfolio_returns, type = "l")
plot(cumsum(magnificent7_portfolio_returns), type = "l")
plot(cumsum(msci_returns), type = "l")

current_date <- Sys.Date()
current_year <- format(current_date, "%Y")
start_of_year <- as.Date(paste0(current_year, "-01-01"))

all_dates <- seq.Date(from = start_of_year, to = current_date, by = "day")
weekends <- all_dates[format(all_dates, "%u") %in% c(6, 7)]
bank_holidays <- as.Date(c("2024-01-01", "2024-12-25"))
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

unknown_bank_holidays_amount <- 8
idx_mag7 <- length(magnificent7_portfolio_returns) - length(bizseq) + unknown_bank_holidays_amount
idx_msci <- length(msci_returns) - length(bizseq) + unknown_bank_holidays_amount

magnificent7_portfolio_ret <- magnificent7_portfolio_returns[idx_mag7:length(magnificent7_portfolio_returns)]
plot(cumsum(magnificent7_portfolio_ret), type = "l")

msci_ret <- msci_returns[idx_msci:length(msci_returns)]
lines(cumsum(as.numeric(msci_ret$URTH.Adjusted)), type = "l", col = "red")

data <- data.frame(Magnificent7 = magnificent7_portfolio_ret, MSCI = msci_ret)

avg_magnificent7_return <- mean(data$Magnificent7, na.rm = TRUE)
avg_msci_return <- mean(data$URTH.Adjusted, na.rm = TRUE)

print(avg_magnificent7_return)
avg_magnificent7_return*length(data$Magnificent7)

print(avg_msci_return)
avg_msci_ex7_return*length(data$URTH.Adjusted)

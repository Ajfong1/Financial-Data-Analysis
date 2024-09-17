library(quantmod)
library(ggplot2)
library(tidyverse)
library(rvest)

scrapeStock <- function(Ticker) {
  # Scrape Stock Name from Yahoo Finance
  yahooFinance <- paste0("https://finance.yahoo.com/quote/", Ticker, "/")
  readFinance <- read_html(yahooFinance)
  
  # Extract Name of the stock
  stockName <- readFinance %>%
    html_nodes("h1") %>%
    html_text()
  
  # Makes sure Name is a single value
  stockName <- stockName[1]
  
  # Get Stock Price Data from Quantmod
  stockPrice <- getSymbols(Ticker, src = "yahoo", auto.assign = FALSE)
  
  # Combine Data into a Data Frame
  stockDF <- data.frame(stockPrice)
  colnames(stockDF) <- c("Open", "Day High", "Day Low", "Day Close", "Day Volume", "Adjusted Price")
  
  # Add the Stock Name
  stockDF$Name <- stockName
  
  # Assign the data frame to a variable named after the ticker
  assign(Ticker, stockDF, envir = .GlobalEnv)
}

# Prompt the user for a ticker symbol
Ticker <- readline("Enter the ticker symbol of the stock: ")

# Run the function to create a new data frame for the specified ticker
scrapeStock(Ticker)


graphFunction <- function(plotStock) {
  # Get Stock Price Data from Quantmod
  stockChart <- getSymbols(plotStock, src = "yahoo", auto.assign = FALSE)
  
  # Set date to only include one year
  endDate <- Sys.Date()
  startDate <- endDate - 365
  stockChart <- stockChart[time(stockChart) >= startDate & time(stockChart) <= endDate, ]
  
  # Plot the stock prices and volume using chartSeries
  chartSeries(stockChart, 
              name = paste(plotStock, "Stock Price and Volume"), 
              TA = c(addTA(Vo(stockChart), col = "blue"),  # Volume chart in a separate pane
                     addTA(Hi(stockChart), col = "green", on = 1),  # High price overlay
                     addTA(Lo(stockChart), col = "purple", on = 1)  # Open price overlay
              )
  )
}

# Prompt the user for a ticker symbol
graphStock <- readline("Enter the ticker symbol of the stock you want to see the chart of: ")

# Run the function to plot the stock data
graphFunction(graphStock)

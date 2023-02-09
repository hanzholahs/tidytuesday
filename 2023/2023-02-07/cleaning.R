library(tidyverse)
library(here)
library(janitor)

# Source for datasets. The datasets were downloaded and extracted to an
# "archive" folder within the working directory for processing, but they are not
# included in this repo.
"https://www.kaggle.com/datasets/evangower/big-tech-stock-prices"

# This is mostly equivalent to fs::dir_map, but we need to keep the info from
# the filename.
big_tech_stock_prices_list <- purrr::map(
  fs::dir_ls(
    here::here("2023", "2023-02-07", "data"),
    glob = "*.csv"
  ),
  \(path) {
    ticker <- fs::path_file(path) |> fs::path_ext_remove()
    readr::read_csv(
      file = path,
      col_types = cols(
        Date = col_date(format = ""),
        Open = col_double(),
        High = col_double(),
        Low = col_double(),
        Close = col_double(),
        `Adj Close` = col_double(),
        Volume = col_double()
      )
    ) |> 
      dplyr::mutate(stock_symbol = ticker, .before = 1)
  }
)

big_tech_stock_prices <- purrr::list_rbind(big_tech_stock_prices_list) |> 
  janitor::clean_names()
dplyr::glimpse(big_tech_stock_prices)

readr::write_csv(
  big_tech_stock_prices,
  here::here("2023", "2023-02-07", "big_tech_stock_prices.csv")
)

big_tech_stock_prices |> 
  dplyr::count(stock_symbol, sort = TRUE)


# Make a lookup for the symbols.
tibble::tibble(
  stock_symbol = c(
    "AAPL",
    "ADBE",
    "AMZN",
    "CRM",
    "CSCO",
    "GOOGL",
    "IBM",
    "INTC",
    "META",
    "MSFT",
    "NFLX",
    "NVDA",
    "ORCL",
    "TSLA"
  ),
  company = c(
    "Apple Inc.",
    "Adobe Inc.",
    "Amazon.com, Inc.",
    "Salesforce, Inc.",
    "Cisco Systems, Inc.",
    "Alphabet Inc.",
    "IBM Corporation",
    "Intel Corporation",
    "Meta Platforms, Inc.",
    "Microsoft Corporation",
    "Netflix, Inc.",
    "NVIDIA Corporation",
    "Oracle Corporation",
    "Tesla, Inc."
  )
) |> 
  readr::write_csv(
    here::here("2023", "2023-02-07", "big_tech_companies.csv")
  )

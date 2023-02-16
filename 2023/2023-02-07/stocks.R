# Inspirations: 
# https://twitter.com/issa_madjid/status/1623027270314086401
# https://twitter.com/danoehm/status/1623236802214760448

library(ggplot2)
library(dplyr)
library(showtext)
library(ggimage)

font_add_google("Ubuntu mono", "ubuntu_mono")
showtext_auto()

dt_prices <- readr::read_csv(here::here("2023", 
                                        "2023-02-07", 
                                        "big_tech_stock_prices.csv"))
dt_info   <- readr::read_csv(here::here("2023", 
                                        "2023-02-07", 
                                        "big_tech_companies.csv")) |> 
  left_join(summarise(dt_prices, 
                      y_coor = max(adj_close) * 1.15, 
                      x_coor = min(date) + 15, 
                      .by = stock_symbol)) |>
  mutate(company = stringr::str_replace(company, 
                                        "International Business Machines", 
                                        "IBM"))

colour_palette <- c(
  "AAPL"  = "#A2AAAD",
  "ADBE"  = "#FF0202",
  "AMZN"  = "#FF9900",
  "CRM"   = "#1798C1",
  "CSCO"  = "#21C4ED",
  "GOOGL" = "#36A955",
  "IBM"   = "#0530AD",
  "INTC"  = "#056AB6",
  "META"  = "#0769E1", 
  "MSFT"  = "#F15326",
  "NFLX"  = "#E50914", 
  "NVDA"  = "#77B903",
  "ORCL"  = "#C94C3A",
  "TSLA"  = "#E31937"
)


dt_prices |> 
  ggplot(aes(x = date, 
             y = adj_close,
             colour = stock_symbol,
             fill = stock_symbol)) +
  geom_area(alpha = .5) +
  geom_line(linewidth = 1) +
  geom_text(data = dt_info, aes(x = x_coor, y = y_coor, label = company), 
            vjust = 2, hjust = 0, size = 5) +
  facet_wrap(. ~ stock_symbol, ncol = 3, scale = "free") +
  scale_colour_manual(values = colour_palette, guide = "none") +
  scale_fill_manual(values = colour_palette, guide = "none") +
  labs(title = "Big Tech Stock Prices",
       subtitle = "#tidytuesday Week 6, inspired by the work of @issa_madjid",
       caption = "@hanzholahs",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, 
                              family = "ubuntu_mono", 
                              color = "#343434"),
    plot.subtitle = element_text(size = 10, 
                                 family = "ubuntu_mono", 
                                 color = "#343434"),
    plot.caption = element_text(size = 8, 
                                family = "ubuntu_mono", 
                                color = "#343434",
                                hjust = 0.5), 
    plot.background = element_rect(fill = "#eeeeee"),
    panel.background = element_rect(fill = "#eeeeee", colour = NA),
    panel.grid = element_blank(), 
    strip.text = element_blank()
  )

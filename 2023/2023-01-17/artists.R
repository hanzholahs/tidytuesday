# Art History Data, TidyTuesday 2023-01-17
# This program is written by Hanzholah Shobri (hanzholahs@gmail.com)
# Feel free to use any part / all the code below.
# 20 January 2022
# ----
# 
# The program is an attempt to replicate visualization presented by Holland 
# Stam at the thesis, titled 'Quantifying Art Narratives. The data comes from
# `arthistory` data package which is download from TidyTuesday repository.


library(dplyr)
library(ggplot2)
library(stringr)



# Dataset -----------------------------------------------------------------
# url: https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-01-17
#
# This dataset contains data that was used for Holland Stam’s thesis work. The 
#   data was collected to assess the demographic representation of artists 
#   through editions of Janson’s History of Art and Gardner’s Art Through the 
#   Ages.

# set url
.dataURL <- paste('https://raw.githubusercontent.com',
                  'rfordatascience',
                  'tidytuesday',
                  'master',
                  'data',
                  '2023',
                  '2023-01-17',
                  'artists.csv',
                  sep = "/")

# download data
artists <- readr::read_csv(.dataURL)



# Preparation -------------------------------------------------------------

# artists.csv: Contains various information about artists by edition of Gardner
#   or Janson’s art history textbook from 1926 until 2020. Data includes
#   demographic information, space occupied in the textbook, as well as 
#   presence in the MoMA and Whitney museums. 

# glimpse at the data
glimpse(artists)

# check unique values for each column
purrr::map(artists, unique)$artist_race

# replace NAs
artists <- artists |> 
  purrr::map_if(is.character, 
                function(x) {
                  str_replace_all(x, "^N/A.*", "") |> 
                    na_if("")
                }) |> 
  as_tibble() |> 
  na.omit()


# Visualization -----------------------------------------------------------

# count the number of artists of Gardner's book for each year
artists |> 
  filter(book == "Gardner") |> 
  group_by(year) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = year, y = count)) +
  geom_col(width = 2, fill = "#43aa65") +
  geom_text(aes(label = count, y = count + 15)) +
  theme_minimal() +
  labs(title = "Overall Count of Artists in Gardner's Art Through the Ages",
       x = "Year of Publication",
       y = "Count")

# calculate the average proportion of male artist 
gardner_male_avg <- mean(filter(artists, book == "Gardner")$artist_gender == "Male")

# view gender distribution of Gardner's book for each year
artists |> 
  filter(book == "Gardner") |> 
  ggplot(aes(x = year, fill = artist_gender)) +
  geom_bar(position = "fill", width = 2) +
  geom_hline(yintercept = gardner_male_avg, size = 1) +
  scale_fill_manual(values = c("#aa4365", "#4365aa")) +
  theme_minimal() +
  labs(title = "Gender of Artists in Gardner's Art Through the Ages",
       x = "Year of Publication",
       y = "Proportion",
       fill = "Artist Gender")

# count the number of artists of Janson's book for each year
artists |> 
  filter(book == "Janson") |> 
  group_by(year) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = year, y = count)) +
  geom_col(width = 2, fill = "#43aa65") +
  geom_text(aes(label = count, y = count + 15)) +
  theme_minimal() +
  labs(title = "Overall Count of Artists in Gardner's Art Through the Ages",
       x = "Year of Publication",
       y = "Count")

# calculate the average proportion of male artist 
gardner_male_avg <- mean(filter(artists, book == "Janson")$artist_gender == "Male")

# view gender distribution of Gardner's book for each year
artists |> 
  filter(book == "Janson") |> 
  ggplot(aes(x = year, fill = artist_gender)) +
  geom_bar(position = "fill", width = 2) +
  geom_hline(yintercept = gardner_male_avg, size = 1) +
  scale_fill_manual(values = c("#aa4365", "#4365aa")) +
  theme_minimal() +
  labs(title = "Gender of Artists in Gardner's Art Through the Ages",
       x = "Year of Publication",
       y = "Proportion",
       fill = "Artist Gender")
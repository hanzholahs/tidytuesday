# Holywood Age Gaps Data, TidyTuesday 2023-02-15
# This program is written by Hanzholah Shobri (hanzholahs@gmail.com)
# Feel free to use any part / all the code below.
# 13 February 2022
# ----
# 
# The program provides exploratory analysis and visualisation of data from
# Alone package. 

library(dplyr)
library(ggplot2)

theme_set(theme_minimal())


# Dataset -----------------------------------------------------------------
# url: https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-02-14
#
# This dataset originally comes from `Hollywood Age Gap` data from Lynn Fisher.
#   The data contains the difference between main actors' ages, which is 
#   collected from more than 630 movies.

# set url
.repoURL <- paste('https://raw.githubusercontent.com',
                  'rfordatascience',
                  'tidytuesday',
                  'master',
                  'data',
                  '2023',
                  '2023-02-14',
                  sep = "/")

# download data
age_gaps <- readr::read_csv(paste(.repoURL, 'age_gaps.csv', sep = "/"))



# Exploration -------------------------------------------------------------

# glimpse at the data
age_gaps |> glimpse()

# there are some movies with more than 1 couples
count(age_gaps, couple_number)
sum(count(age_gaps, movie_name)$n > 1)
sum(count(age_gaps, movie_name)$n == 1)

# about half of directors have more than two movies
sum(count(age_gaps, director)$n > 1)
sum(count(age_gaps, director)$n == 1)

# male actors are usually the older one
count(age_gaps, character_1_gender)
count(age_gaps, character_2_gender)


# no disticntive correlation between release year and age difference
ggplot(age_gaps, aes(x = release_year, y = age_difference)) +
  geom_point() +
  geom_smooth()

# it is typical to have older male actors
bind_rows(select(age_gaps, 
                 name = actor_1_name, 
                 age = actor_1_age, 
                 gender = character_1_gender),
          select(age_gaps, 
                 name = actor_2_name, 
                 age = actor_2_age, 
                 gender = character_2_gender)) |> 
  ggplot(aes(x = age, y = gender)) +
  geom_boxplot()

# no correlation between years and actors' ages
bind_rows(select(age_gaps, 
                 release_year,
                 name = actor_1_name, 
                 age = actor_1_age, 
                 gender = character_1_gender),
          select(age_gaps, 
                 release_year,
                 name = actor_2_name, 
                 age = actor_2_age, 
                 gender = character_2_gender)) |> 
  mutate(year = floor(release_year / 10) |> as.factor()) |> 
  ggplot(aes(x = age, y = gender, colour = year)) +
  geom_jitter()
  


# Cleaning ----------------------------------------------------------------

age_gaps |> 
  count(director) |> 
  filter(n > 1)



# Visualisation 1 ---------------------------------------------------------

# to show the trends of remaining proportion of survivalists given time


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
library(patchwork)
library(ggtext)

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

# `age_difference` calculated correctly
age_gaps |> 
  mutate(testcase = age_difference == abs(actor_1_age - actor_2_age)) |> 
  summarise(this_must_equal_to_one = mean(testcase))

# Actor 1 is always as old as or older than actor 2
age_gaps |> 
  mutate(testcase = actor_1_age >= actor_2_age) |> 
  summarise(this_must_equal_to_one = mean(testcase))

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
  
# distribution of age difference
ggplot(age_gaps, aes(x = age_difference)) + geom_boxplot()
ggplot(age_gaps, aes(x = age_difference)) + geom_histogram(binwidth = 2)



# Cleaning and preparation ------------------------------------------------

age_gaps |> 
  count(director) |> 
  filter(n > 1)

couple_ref_tbl <- age_gaps |> 
  mutate(ID = paste(movie_name, couple_number),
         ID = forcats::fct_reorder(ID, age_difference)) |> 
  select(ID, movie_name, release_year, director, age_difference)

age_gaps_long <- bind_rows(
  age_gaps |>
    mutate(ID = paste(movie_name, couple_number)) |> 
    select(ID, contains("1")) |> 
    rename("name" = actor_1_name,
           "gender" = character_1_gender,
           "birthdate" = actor_1_birthdate,
           "age" = actor_1_age),
  age_gaps |>
    mutate(ID = paste(movie_name, couple_number)) |> 
    select(ID, contains("2")) |> 
    rename("name" = actor_2_name,
           "gender" = character_2_gender,
           "birthdate" = actor_2_birthdate,
           "age" = actor_2_age)
) |> 
  left_join(couple_ref_tbl, by = "ID")

avg_age_man <- mean(filter(age_gaps_long, gender == "man")$age)
avg_age_woman <- mean(filter(age_gaps_long, gender == "woman")$age)



# Visualisation -----------------------------------------------------------

# This visualisation takes some inspiration from the following work: 
#   https://twitter.com/nrennie35/status/1625471525448032260

# plot 1: distribution of age difference throughout years
p1 <- ggplot(age_gaps, aes(x = release_year, y = age_difference)) +
  geom_jitter(alpha = 0.5, size = 2, colour = "#ab1223") +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(1935, 2025, 15)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(title = "Age differences throughout time",
       x = NULL, y = NULL)
p1

# directors who employ the widest couple's age gap
p2 <- age_gaps |> 
  group_by(director) |> 
  summarise(age_diff_avg = max(age_difference),
            n = n()) |> 
  filter(n > 5) |> 
  head(10) |> 
  mutate(director = forcats::fct_reorder(director, age_diff_avg),
         director = paste0("**", director, "**")) |> 
  ggplot(aes(x = age_diff_avg, y = director)) +
  geom_col(width = 0.8) +
  geom_vline(xintercept = mean(age_gaps$age_difference), linetype = "twodash") +
  geom_text(aes(label = age_diff_avg, x = age_diff_avg + 0.3), hjust = 0) +
  labs(title = "Directors who employ artists with widest age gap", 
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
p2

# movies with the widest couple's age gap
p3 <- age_gaps_long |> 
  mutate(movie = paste0("**", movie_name, "**<br>(", release_year, ")")) |> 
  arrange(desc(age_difference)) |> 
  head(10 * 2) |> 
  ggplot(aes(x = age, y = movie, group = ID)) +
  geom_line() +
  geom_point(aes(colour = gender, shape = gender), size = 5) +
  geom_text(aes(label = age)) +
  geom_vline(xintercept = avg_age_man, linewidth = 3, alpha = 0.4) +
  geom_vline(xintercept = avg_age_woman, linewidth = 3, alpha = 0.4) +
  geom_richtext(aes(x = avg_age_man + 0.5, 
                    y = 11, 
                    label = paste0("Man avg age<br>", 
                                   round(avg_age_man, digits = 1),
                                   " y/o")), 
                hjust = 0, size = 3, label.colour = NA, fill = NA) +
  geom_richtext(aes(x = avg_age_woman + 0.5, 
                    y = 11, 
                    label = paste0("Woman avg age<br>", 
                                   round(avg_age_woman, digits = 1),
                                   " y/o")), 
                hjust = 0, size = 3, label.colour = NA, fill = NA) +
  labs(title = "Top 10 artists with the widest age gap", x = NULL, y = NULL) +
  guides(colour = FALSE, shape = FALSE) +
  theme(axis.text.x = element_blank())
p3

# combining all plots
(p1 + (p2 / p3)) +
  plot_annotation(
    title = "Holywood Age Gaps",
    subtitle = paste0(
      "The age gaps dataset includes `gender` columns, which always contain",
      "the values `man` or `woman`. These values appear to indicate\n",
      "how the characters in each film identify. Some of these values do not",
      "match how the actor identifies. We apologize if any characters\n",
      "are misgendered in the data!"
    ),
    caption = "Source: {Holywood Age Gap, Data is Plural}",
    theme = theme(plot.title = element_text(size = 24),
                  plot.subtitle = element_text(size = 16))
  ) &
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    axis.text.y = element_markdown(),
    panel.grid = element_blank()
  )


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
library(showtext)

font_add_google("Montserrat", "montserrat")
showtext_auto()

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
avg_age_diff <- mean(age_gaps$age_difference)


# Visualisation -----------------------------------------------------------

# This visualisation takes some inspiration from the following work: 
#   https://twitter.com/nrennie35/status/1625471525448032260

# plot 1: distribution of age difference throughout years
p1 <- ggplot(age_gaps, aes(x = release_year, y = age_difference)) +
  geom_jitter(alpha = 0.7, size = 2, colour = "#8AA29E") +
  geom_smooth(se = FALSE, colour = "#FF9E99", linewidth = 2) +
  scale_x_continuous(breaks = seq(1935, 2025, 15)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(title = "Decreasing trend in age gaps throughout time",
       x = NULL, y = NULL)
p1

# directors who employ the widest couple's age gap
p2 <- age_gaps |> 
  group_by(director) |> 
  summarise(age_diff_avg = max(age_difference),
            n = n()) |> 
  filter(n >= 5) |> 
  mutate(director = paste0("**", director, "**"),
         director = forcats::fct_reorder(director, age_diff_avg)) |> 
  arrange(desc(age_diff_avg)) |> 
  head(10) |> 
  ggplot(aes(x = age_diff_avg, y = director)) +
  geom_col(width = 0.8, fill = "#8AA29E") +
  geom_vline(xintercept = avg_age_diff, colour = "#FF9E99", linewidth = 1.5) +
  geom_text(aes(label = age_diff_avg, x = age_diff_avg + 0.3), hjust = 0,
            colour = "white") +
  geom_richtext(aes(x = avg_age_diff + 0.5, y = 12, 
                    label = paste0("Avg age difference<br>", 
                                   round(avg_age_diff, digits = 1),
                                   " y/o")), 
                hjust = 0, vjust = 1.2, size = 3, label.colour = NA, 
                colour = "white", fill = NA) +
  labs(title = "Directors (>= 5 movies) who cast two actors with huge age gaps", 
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
p2

# movies with the widest couple's age gap
p3 <- age_gaps_long |> 
  mutate(movie = paste0("**", movie_name, "**<br>(", release_year, ")"),
         movie = forcats::fct_reorder(movie, age_difference)) |> 
  arrange(desc(age_difference)) |> 
  head(10 * 2) |> 
  ggplot(aes(x = age, y = movie, group = ID)) +
  geom_vline(xintercept = avg_age_man, linewidth = 3, alpha = 0.6, 
             colour = "#E5B19E") +
  geom_vline(xintercept = avg_age_woman, linewidth = 3, alpha = 0.6,
             colour = "#FFE699") +
  geom_line(linewidth = 1.2, colour = "#FF9E99") +
  geom_point(aes(colour = gender, shape = gender), size = 5, fill = "white",
             stroke = 3) +
  geom_text(aes(label = age), size = 3, fontface = "bold") +
  geom_richtext(aes(x = avg_age_man + 0.5, 
                    y = 11.5, 
                    label = paste0("Avg Man<br>", 
                                   round(avg_age_man, digits = 1),
                                   " y/o")), 
                hjust = 0, vjust = 1.2, size = 3, label.colour = NA, 
                colour = "white", fill = NA) +
  geom_richtext(aes(x = avg_age_woman + 0.5, 
                    y = 11.5, 
                    label = paste0("Avg Woman<br>", 
                                   round(avg_age_woman, digits = 1),
                                   " y/o")), 
                hjust = 0, vjust = 1.2, size = 3, label.colour = NA, 
                colour = "white", fill = NA) +
  scale_colour_manual(values = c("#E5B19E", "#FFE699")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Movies with the biggest age gaps between couple casts",
       colour = "Gender",
       shape = "Gender",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
p3

# combining all plots
p_final <- (p1 + (p2 / p3)) +
  plot_annotation(
    title = "Holywood Age Gaps",
    subtitle = paste0(
      "The holywood age gaps dataset contain information regarding the gaps",
      "between two actors played as couple in the film. Generally, there is\n",
      "a decreasing trend for the age gaps throughout the year. The director ",
      "Joel Coen is the top directors (with >= 5 available observations)\n",
      "who casts actors with the highest of average age gaps as a couple.",
      "Harold and Maude (1971) is the movie with the widest age difference\n",
      "between actors playing the couple roles with 52 years difference."
    ),
    caption = "Source: {Holywood Age Gap, Data is Plural}",
    theme = theme(plot.title = element_text(),
                  plot.subtitle = element_text(colour = "white"),
                  plot.caption = element_text(colour = "white"))
  ) +
  plot_layout(guides = "collect") &
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(colour = "white"),
    plot.background = element_rect(colour = NA, fill = "#040F0F"),
    axis.text.y = element_markdown(colour = "white"),
    axis.text.x = element_markdown(colour = "white"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", colour = "white"),
    legend.text = element_text(colour = "white")
  )

p_final

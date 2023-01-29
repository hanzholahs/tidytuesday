# Alone Data, TidyTuesday 2023-01-24
# This program is written by Hanzholah Shobri (hanzholahs@gmail.com)
# Feel free to use any part / all the code below.
# 28 January 2022
# ----
# 
# The program provides straightforward analysis and visualisation of data from
# Alone package. In total, there are three visualisations: 1 inspired by
# the official website of the package, 1 inspired by visualisation created by
# @TanyaSaphiro, and 1 original visualisation.

library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)
library(ggtext)

theme_set(theme_minimal())


# Dataset -----------------------------------------------------------------
# url: https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-01-24
#
# This dataset contains data from the TV series Alone collected and shared by 
#   Dan Oehm. As described in Oehm's blog post, in the survival TV series 
#   ‘Alone,’ 10 survivalists are dropped in an extremely remote area and must 
#   fend forthemselves. They aim to last 100 days in the Artic winter, living 
#   off the land through their survival skills, endurance, and mental fortitude.

# set url
.repoURL <- paste('https://raw.githubusercontent.com',
                  'rfordatascience',
                  'tidytuesday',
                  'master',
                  'data',
                  '2023',
                  '2023-01-24',
                  sep = "/")

# download data
survivalists <- readr::read_csv(paste(.repoURL, 'survivalists.csv', sep = "/"))
loadouts     <- readr::read_csv(paste(.repoURL, 'loadouts.csv', sep = "/"))
episodes     <- readr::read_csv(paste(.repoURL, 'episodes.csv', sep = "/"))
seasons      <- readr::read_csv(paste(.repoURL, 'seasons.csv', sep = "/"))



# Exploration -------------------------------------------------------------

# survivalists.csv: A data frame of survivalists across all 9 seasons detailing 
#   name and demographics, location and profession, result, days lasted, 
#   reasons for tapping out (detailed and categorised), and page URL.

# glimpse at the data
survivalists |> dplyr::glimpse()

# age distribution: half within 30-45 y/o
survivalists$age |> qplot(geom = "boxplot")

# gender: most are male
survivalists$gender |> table()

# country: most from US, 13 from CA, 1 from UK, 1 from US Virgin Island
survivalists$country |> table()

# medical evacuation: higher than initial expectation (69 : 25)
survivalists$medically_evacuated |> table()

# reason to forfeit: family and health
survivalists$reason_category  |> table()

# professions: seems vary
survivalists$profession  |> table()

# duration: half within 10-65 days
survivalists$days_lasted |> qplot(geom = "boxplot")

# multiple participations: not necessarily last longer
survivalists |> 
  filter(name %in% survivalists[duplicated(survivalists$name), ]$name) |> 
  arrange(name) |> 
  ggplot(aes(x = season, y = days_lasted, colour = name)) +
  geom_line()

# multiple participations: not necessarily better position
survivalists |> 
  filter(name %in% survivalists[duplicated(survivalists$name), ]$name) |> 
  arrange(name) |> 
  ggplot(aes(x = season, y = result, colour = name)) +
  geom_line()

# gender to medical evacuation: males are less likely to get evacuated
survivalists |> 
  group_by(gender) |> 
  summarise(medic= sum(medically_evacuated),
            size = n(),
            prop = medic / size)

# age who get evacuated: evacuated people have lower age average
survivalists |> 
  group_by(evacuated = medically_evacuated) |> 
  summarise(mean_age = mean(age),
            median_age = median(age))



# loadouts.csv: The rules allow each survivalist to take 10 items with them. 
#   This dataset includes information on each survivalist’s loadout. It has 
#   detailed item descriptions and a simplified version for easier aggregation
#   and analysis

# glimpse at the data
loadouts |> glimpse()

# unique names
loadouts$name |> unique()

# unique items: top 5 include pot, fishing gear, sleeping bag, saw, axe
loadouts$item |> unique()
loadouts$item |> table() |> sort()



# episodes.csv: This dataset contains details of each episode including the 
#   title, number of viewers, beginning quote, and IMDb rating. New episodes 
#   are added at the end of future seasons.

# glimpse at the data
episodes |> glimpse()

# viewer distribution: half are within a range of 1.3 to 1.8 mil
na.omit(episodes$viewers * 1e6) |> qplot(geom = "boxplot")

# imdb rating: half are between 7.6 and 8.2
na.omit(episodes$imdb_rating) |> qplot(geom = "boxplot")

# count rating: half are between 50 and 70
na.omit(episodes$n_ratings) |> qplot(geom = "boxplot")



# seasons.csv: The season summary dataset includes location, latitude and 
#   longitude, and other season-level information. It includes the date of 
#   drop-off where the information exists.

# glimpse at the data
seasons |> glimpse()




# Cleaning ----------------------------------------------------------------

survivalists <- survivalists |> 
  mutate(status = case_when(
    reason_category == "Family / personal" ~ "Out - Personal",
    reason_category == "Medical / health"  ~ "Out - Medical", 
    reason_category == "Loss of inventory" ~ "Out - Loss of inventory",
    is.na(reason_category)                 ~ "Winner",
    TRUE ~ ""
  )) |> 
  mutate_at(vars("gender", "city", "state", "country", "reason_tapped_out", 
                 "reason_category", "team", "profession"), as.factor) 



# Visualisation 1 ---------------------------------------------------------

# to show the trends of remaining proportion of survivalists given time
tibble(day = rep(0:100, each = 2), 
       sex = rep(c("Male", "Female"), 101)) |> 
  mutate(survivor = purrr::map2_dbl(
    day, 
    sex, 
    function(x, y) {
      sum(survivalists$days_lasted[survivalists$gender == y] >= x)
    })
  ) |>
  group_by(sex) |> 
  mutate(survivor = survivor / first(survivor)) |> 
  ggplot(aes(x = day, y = survivor, colour = sex)) +
  geom_line(size = 1.2) +
  scale_y_continuous(breaks = 0:5 / 5) +
  scale_colour_manual(values = c("#765631", "#4234A9")) +
  labs(title = "Survival curves",
       subtitle = "there is some evidence that, on average, women tend to survive longer than men",
       y = "Proportion Remaining",
       x = "Days Lasted",
       colour = "Gender") +
  theme(
    plot.title = element_text(face = "bold"), 
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
  )


# Visualisation 2 ---------------------------------------------------------

# create table for average survival of both male and female
survivalists_gender_summary <- group_by(survivalists, gender) |> 
  summarise(days_lasted = mean(days_lasted)) |> 
  mutate(label = paste0("Male avg\n", round(days_lasted, digits = 1), " Days"))

# create table for participants who could last the longest
survivalists_days_lasted_max <- group_by(survivalists, gender) |> 
  arrange(desc(days_lasted)) |> 
  summarise(name = first(name) |> stringr::str_replace(" ", "\n"),
            days_lasted = first(days_lasted))

# calculate the average period of survival
days_lasted_average <- mean(survivalists$days_lasted)

# to illustrate the resignation timing of survivalists from the program
survivalists |> 
  ggplot(aes(x = days_lasted, y = gender)) +
  geom_beeswarm(aes(colour = status), size = 4, alpha = .85, cex = 4) +
  geom_point(data = survivalists_gender_summary, size = 4, shape = 22, 
             fill = "black", colour = "white") +
  geom_text_repel(data = survivalists_gender_summary, aes(label = label),
                  size = 3, nudge_y = .25, nudge_x = -3, point.padding = 2,
                  segment.size = 0.65) +
  geom_text(data = survivalists_days_lasted_max, aes(label = name), 
            size = 2, colour = "#444444", nudge_y = -.18) +
  geom_vline(xintercept = days_lasted_average, linetype = "twodash") +
  scale_color_manual(values = c("#2B4162", "#C3423F", "#9AADBF", "#FBB13C")) +
  scale_y_discrete(labels = c(
    paste0("**Female**<br/>(n=", sum(survivalists$gender == "Female"), ")"),
    paste0("**Male**<br/>(n=", sum(survivalists$gender == "Male"), ")")
  )) +
  labs(
    title = "On average, female survivalists outlast their male counterparts",
    subtitle = paste(
      "Analysis of survivalists competing on the US reality TV series",
      "**Alone**, across all seasons (1-9). <br/> Comparsion of days lasted",
      "by gender. Although the show has yet to crown a female winner, <br/>on ",
      "average as a group, female survivalists last 13 days longer than male",
      "competitors."),
    x = "Days Lasted",
    y = NULL,
    colour = "Survivalist Status",
    caption = "Source: {alone package}"
  ) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 1),
    plot.caption.position = "plot",
    axis.text.y = element_markdown(hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
  )


# Visualisation 3 ---------------------------------------------------------

# to answer what could be the most valuable items
full_join(survivalists, loadouts, by = c("season", "name")) |> 
  group_by(item) |> 
  summarise(n = n(), days_lasted = mean(days_lasted)) |> 
  arrange(desc(days_lasted)) |> 
  filter(n >= 5) |> 
  mutate(top3 = c(rep(TRUE, 3), rep(FALSE, n() - 3)),
         item = paste0("**", item, "**<br/>(n=", n, ")")) |>  
  mutate(item = forcats::fct_reorder(item, days_lasted)) |>
  ggplot(aes(y = item, x = days_lasted, fill = top3)) +
  geom_col(width = .75) +
  geom_vline(xintercept = days_lasted_average, linetype = "twodash") +
  geom_text(aes(label = round(days_lasted, 1)), 
            size = 3, hjust = 0, nudge_x = .5) +
  scale_fill_manual(values = c("#876546", "#402F20")) +
  labs(
    title = "There are items frequently brought by survivalists who last longer",
    subtitle = paste(
      "For all common loadout items (n > 5), survivalists with tapping wire,",
      "frying pan, or paracord tend to last longer."
    ), 
    caption = "Source: {alone package}",
    x = "Days lasted on average",
    y = NULL,
    parse = TRUE
  ) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#CCE8E3", color = FALSE),
    panel.background = element_rect(fill = "#CCE8E3", color = FALSE),
    panel.grid = element_blank(),
    axis.title.x = element_markdown(colour = "#333333", size = 9.5),
    axis.text.y = element_markdown(),
    legend.position = "none",
  )

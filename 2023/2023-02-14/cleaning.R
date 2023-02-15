# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-02-14/

library(tidyverse)
library(here)
library(janitor)

age_gaps <- read_csv("http://hollywoodagegap.com/movies.csv") |> 
  clean_names()

# making sense of the columns
age_gaps |> 
  glimpse()

age_gaps |> 
  map(unique)

age_gaps |> 
  map_lgl(anyNA)

age_gaps <- age_gaps |> 
  mutate(across(ends_with("name"), as.character)) |> 
  mutate(across(ends_with("gender"), as.factor)) |> 
  mutate(across(ends_with("age"), as.integer)) |> 
  mutate(across(ends_with("year"), as.integer)) |> 
  mutate(across(ends_with("difference"), as.integer)) |> 
  mutate(across(ends_with("birthdate"), as.Date)) 

# For the most part, they put the man first if there's a man in the couple. It
# doesn't look like there's a strict rule, though. But beware: Some movies have
# more than 1 couple! Let's use all that to rebuild the data, always putting the
# older character first.
age_gaps <- age_gaps |> 
  mutate(
    couple_number = row_number(),
    .by = "movie_name"
  ) |> 
  pivot_longer(
    cols = starts_with(c("actor_1_", "actor_2_")),
    names_to = c(NA, NA, ".value"),
    names_sep = "_"
  ) |> 
  # Put the older actor first.
  arrange(desc(age_difference), movie_name, birthdate) |> 
  # While we have it pivoted, correct Elliot Page's name. I don't know if other
  # actors are similarly deadnamed, but at least we can fix this one. Note that
  # the *characters* played by Elliot in these particular films were women, so
  # I'll leave the gender as-is.
  mutate(
    name = case_match(
      name,
      "Ellen Page" ~ "Elliot Page",
      .default = name
    )
  ) |>
  mutate(
    position = row_number(),
    .by = c("movie_name", "couple_number")
  ) |> 
  pivot_wider(
    names_from = "position",
    names_glue = "actor_{position}_{.value}",
    values_from = c("name", "gender", "birthdate", "age")
  )

# The gender isn't really the actor so much as it is the character. Let's
# correct that.
age_gaps <- age_gaps |> 
  rename(
    "character_1_gender" = "actor_1_gender",
    "character_2_gender" = "actor_2_gender"
  )

glimpse(age_gaps)

# Save the data.
write_csv(
  age_gaps,
  here::here(
    "2023", "2023-02-14",
    "age_gaps.csv"
  )
)
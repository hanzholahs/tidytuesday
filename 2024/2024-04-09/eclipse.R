# Tidy Tuesday 9 April 2024: 2023 & 2024 US Solar Eclipses
# By Hanzholah Shobri
# 
# This project took an inspiration from others' beautiful works:
# - https://x.com/geokaramanis/status/1777331785875050946
# - https://x.com/npechl/status/1779567698050977924
# - https://x.com/mitsuoxv/status/1777974663727259713



library(ggplot2)
library(dplyr)
library(patchwork)


# Setup
theme_set(theme_void())

plot_caption <- paste("Source: NASA's Scientific Visualization Studio |",
                      "Graphic: Hanzholah Shobri")

plot_desc <- 
  paste("Celestial Spectacles Over the Americas: On October 14, 2023, an",
        "annular solar eclipse created a 'ring of fire' visible across the",
        "Americas. This phenomenon was followed by a total solar eclipse on",
        "April 8, the following year, during which the Sun was entirely",
        "obscured. Here, the graphs illustrate the eclipse durations",
        "and its starting times per state, with special emphasis on specific",
        "cities where each type of eclipse can be observed.") |> 
  stringr::str_wrap(width = 120)

pal <- MetBrewer::met.brewer("Renoir")


# Load US states map
us_states <-
  readr::read_csv("./2024/2024-04-09/us-states.csv") |> 
  mutate(ID = stringr::str_to_lower(State)) |> 
  select(ID, Abbreviation)

us_maps <-
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |> 
  left_join(us_states, by = join_by(ID))


# Load eclipse data from tidytuesday, focusing on the US mainland
year_labels <- c(
  "2023" = "2023 Annular Eclipse",
  "2024" = "2024 Total Eclipse"
)

tuesdata <- tidytuesdayR::tt_load('2024-04-09')

eclipse_annular_2023 <-
  tuesdata$eclipse_annular_2023 |> 
  filter(state %in% us_maps$Abbreviation) |> 
  mutate(year = 2023, type = "annular", label = year_labels["2023"])

eclipse_total_2024 <-
  tuesdata$eclipse_total_2024 |> 
  filter(state %in% us_maps$Abbreviation) |> 
  mutate(year = 2024, type = "total", label = year_labels["2024"])

eclipse_partial_2023 <-
  tuesdata$eclipse_partial_2023 |> 
  filter(state %in% us_maps$Abbreviation) |> 
  mutate(year = 2023, type = "partial", label = year_labels["2023"])

eclipse_partial_2024 <-
  tuesdata$eclipse_partial_2024 |> 
  filter(state %in% us_maps$Abbreviation) |> 
  mutate(year = 2024, type = "partial", label = year_labels["2024"])



# Extract eclipse durations
duration_2023 <-
  bind_rows(
    eclipse_annular_2023 |> 
      mutate(duration = as.numeric(eclipse_6 - eclipse_1) / 60),
    eclipse_partial_2023 |> 
      mutate(duration = as.numeric(eclipse_5 - eclipse_1) / 60)
  ) |> 
  summarise(duration = mean(duration), .by = c(state))

duration_2024 <-
  bind_rows(mutate(eclipse_total_2024,
                   duration = as.numeric(eclipse_6 - eclipse_1) / 60),
            mutate(eclipse_partial_2024,
                   duration = as.numeric(eclipse_5 - eclipse_1) / 60)) |> 
  summarise(duration = mean(duration), .by = c(state))


# Extratct eclipse starting time
eclipse_start_2023 <-
  bind_rows(eclipse_annular_2023, eclipse_partial_2023) |> 
  mutate(year = 2023,
         label = year_labels["2023"],
         start_time = as.POSIXct(eclipse_1),
         type = factor(type, levels = c("annular", "partial"))) |> 
  select(type, state, name, lat, lon, start_time)

eclipse_start_2024 <-
  bind_rows(eclipse_total_2024, eclipse_partial_2024) |> 
  mutate(year = 2024,
         label = year_labels["2024"],
         start_time = as.POSIXct(eclipse_1),
         type = factor(type, levels = c("total", "partial"))) |> 
  select(type, state, name, lat, lon, start_time)


# Make Plot
plot_us_eclipse <- function(us_maps, duration_data, eclipse_start_data, title) {
  
  left_join(us_maps, duration_data, by = c("Abbreviation" = "state")) |> 
    ggplot() +
    
    # plot US map
    geom_sf(aes(fill = duration), color = NA, alpha = 0.3) +
    geom_sf(color = "#aaaaaa", fill = NA, linewidth = 0.2) +
    scale_fill_viridis_c(option = "C", direction = -1, guide = "none") +
    labs(fill = "Duration (Min)") +
    
    # plot observation points
    ggnewscale::new_scale_fill() +
    geom_point(data = eclipse_start_data,
               mapping = aes(x = lon,
                             y = lat,
                             fill = start_time,
                             color = type,
                             alpha = type),
               shape = 21,
               size = 0.5) +
    scale_alpha_manual(values = c(1, 0.1), guide = "none") +
    scale_color_manual(values = c("white", NA), guide = "none") +
    scale_fill_stepsn(colors = pal, trans = "time", n.breaks = 7) +
    labs(fill = "Start Time (UTC)") +
    
    # setup theme
    labs(title = title) +
    theme(plot.title = element_text(size = 8, face = "bold", hjust = .5),
          legend.position = "bottom",
          legend.spacing.x = unit(5, "lines"),
          legend.key.width = unit(2, "lines"),
          legend.key.height = unit(0.3, "lines"),
          legend.text = element_text(size = 4),
          legend.title.position = "top",
          legend.title = element_text(hjust = 0.5, size = 5)
          )
}


p23 <- plot_us_eclipse(us_maps,
                       duration_2023,
                       eclipse_start_2023,
                       year_labels["2023"])

p24 <- plot_us_eclipse(us_maps,
                       duration_2024,
                       eclipse_start_2024,
                       year_labels["2024"])


p_out <- 
  (p23 + p24) +
  plot_annotation(
    title = "2023 and 2024 US Eclipses",
    subtitle = plot_desc,
    caption = plot_caption,
    theme = theme(
      plot.title = element_text(size = 20, hjust = .5, face = "bold",
                                margin = margin(b = 15)),
      plot.subtitle = element_text(size = 11, hjust = .5, face = "italic",
                                   margin = margin(b = 30)),
      plot.caption = element_text(margin = margin(t = 30, r = 15))
    )
  )


ggsave("./2024/2024-04-09/img/plot.jpg", p_out, width = 10, height = 6)
ggsave("./2024/2024-04-09/img/plot.png", p_out, width = 10, height = 6)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggtext)
library(glue)


# Load data ---------------------------------------------------------------

# setup local path to the data
data_path <- "./2024/2024-04-16/data"
.path_shiny_revdeps <- fs::path_join(c(data_path, "shiny_revdeps.csv"))
.path_package_details <- fs::path_join(c(data_path, "package_details.csv"))



# download data if it's not already
if (!fs::dir_exists(data_path)) {
  tuesdata <- tidytuesdayR::tt_load('2024-04-16')
  
  fs::dir_create(data_path)
  write_csv(tuesdata$shiny_revdeps, .path_shiny_revdeps)
  write_csv(tuesdata$package_details, .path_package_details)
}



# load data to R env
shiny_revdeps <- read_csv(.path_shiny_revdeps)
package_details <- read_csv(.path_package_details)





# Explore data ------------------------------------------------------------

# take a quick look at the data, focus on `shiny_revdeps` data
glimpse(shiny_revdeps)
glimpse(package_details)



# there are 18,474 packages that depend on another package
count(shiny_revdeps, child) |> arrange(desc(n))
count(shiny_revdeps, dependency_type) |> arrange(desc(n))
count(shiny_revdeps, parent) |> arrange(desc(n))





# Prepare Data for visualization ------------------------------------------

# extract all direct dependencies of shiny package
shiny_children <- 
  filter(shiny_revdeps, parent == "shiny", dependency_type != "linkingto") |>
  pull(child)



# extract interdependent packages that are direct dependencies of shiny
graph <-
  shiny_revdeps |> 
  filter(child %in% shiny_children, parent %in% shiny_children) |> 
  select(parent, child, dependency_type) |> 
  as_tbl_graph() |> 
  activate(nodes) |> 
  mutate(degree = centrality_degree(), label = if_else(degree > 15, name, ""))





# Create data visualization -----------------------------------------------

# setup visualization
c_black <- "#060b21"
c_light <- "#fbfef9"
c_pink <- "#ef798a"
c_purp <- "#7d82b8"
c_gren <- "#7fb069"
c_yell <- "#e6af2e"


p_title <- paste("Shiny's Constellation: Dissecting the Network of Package",
                 "Interdependencies of the Shiny Dependencies")

p_subtitle <- 
  c(
    paste("Within the expansive realm of R's dynamic ecosystem, Shiny emerges",
          "as a powerful tooling, empowering statisticians and data scientists",
          "to transform analyses into interactive web applications with ease",
          "and interactivity. The network maps the intricate orbit of",
          "interdependencies within the R Shiny dependencies, illustrating the",
          "mutual interconnectedness of packages."),
    paste("Each node, a celestial body in its own right, represents an R",
          "package, with its size reflecting its gravitational pull—a degree",
          "of influence measured by the number of dependencies it has. The",
          "colored trajectories denote the types of dependencies (depends,",
          "imports, suggests, & linkingto), creating a visual symphony of",
          "interconnected paths."),
    paste("Here lies a universe of collaboration and shared progress, where",
          "the collective endeavors of developers are visualized, illuminating",
          "the complex yet orderly cosmos of package interdependencies.")
  ) |> 
  str_wrap(125) |> 
  str_c(collapse = "<br><br>") |> 
  str_replace_all("depends",
                  glue("<span style='color: {c_pink};'>depends</span>")) |> 
  str_replace_all("imports",
                  glue("<span style='color: {c_purp};'>imports</span>")) |> 
  str_replace_all("suggests",
                  glue("<span style='color: {c_gren};'>suggests</span>")) |> 
  str_replace_all("linkingto",
                  glue("<span style='color: {c_yell};'>linkingto</span>")) |> 
  str_replace_all("\\n", "<br>")


p_caption <- "Source: Shiny on CRAN | Graphic: Hanzholah Shobri"



# Plot using ggraph
p1 <- 
  ggraph(graph, layout = 'kk') + 
  geom_edge_arc(aes(color = dependency_type),
                 arrow    = arrow(length = unit(0.3, 'lines'), type = "open"), 
                 end_cap  = circle(.3, 'lines'),
                 lineend  = "round",
                 linejoin = "bevel",
                 alpha    = 0.1) +
  geom_node_point(aes(size = degree), color = 'steelblue', alpha = .8) +
  geom_node_text(aes(label = label),
                 color = "white",
                 repel = TRUE,
                 vjust = 1.8,
                 size  = 6) +
  labs(title = p_title, subtitle = p_subtitle, caption = p_caption) +
  scale_edge_colour_manual(values = c(c_pink, c_purp, c_yell, c_gren),
                             guide = "none") +
  scale_size(guide = "none") +
  theme_void() +
  theme(plot.title = element_text(size   = 36,
                                  hjust  = .5,
                                  face   = "bold",
                                  color  = c_light,
                                  margin = margin(t = 50, b = 50)),
        plot.subtitle = element_markdown(size  = 30,
                                         hjust = 0.5,
                                         color = c_light,
                                         margin = margin(t = 10, b = 10)),
        plot.caption = element_text(size = 24,
                                    color = c_light,
                                    margin = margin(r = 30, b = 30)),
        plot.background   = element_rect(fill = c_black),
        legend.position   = "bottom",
        legend.title.position = "top",
        legend.title = element_text(size = 24, hjust = .5, color = c_light),
        legend.text  = element_text(size = 16, color = c_light))



# save plot
ggsave("./2024/2024-04-16/img/shiny.jpg", p1, height = 25, width = 30)
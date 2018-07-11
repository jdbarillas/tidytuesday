library(tidyverse)
library(magrittr)
library(readxl)
library(gganimate)
library(patchwork)
library(geojsonio)
library(broom)
library(rgeos)

# download.file(url = "https://github.com/rfordatascience/tidytuesday/blob/master/data/week15_beers.xlsx?raw=true",
#               destfile = "week15/week15_beers.xlsx")

theme_custom <-
  theme(
    text = element_text(family = "Helvetica-Narrow", color = "#22211d"),
    legend.background = element_rect(fill = "#fcfcfc", color = NA),
    legend.key = element_rect(fill = "#fcfcfc"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "#fcfcfc"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "white", size = 1)
  )

beers <- read_excel("week15/week15_beers.xlsx", sheet = 1)
breweries <- read_excel("week15/week15_beers.xlsx", 2)

all_brews <- breweries %>%
  inner_join(beers,
             by = c("id" = "brewery_id"),
             suffix = c("_brewery", "_beer")) %>%
  mutate(state_name = state.name[match(state, state.abb)])

brews_data <- breweries %>%
  inner_join(beers,
             by = c("id" = "brewery_id"),
             suffix = c("_brewery", "_beer")) %>%
  mutate(state_name = state.name[match(state, state.abb)]) %>%
  group_by(state_name) %>%
  summarise(n_breweries = n_distinct(id)) %>%
  filter(!is.na(state_name))

brews_data %>% 
  ggplot(aes(x = n_breweries)) +
  geom_histogram(bins = 20, 
                 fill = 'skyblue', 
                 color = 'white') +
  scale_x_continuous(breaks = seq(1, 45))

# Following the gist of
# https://rud.is/b/2015/05/15/u-s-drought-monitoring-with-hexbin-state-maps-in-r/
spdf <-
  geojson_read("week15/us_states_hexgrid.geojson",  
               what = "sp")

spdf@data <- spdf@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", 
                            "", 
                            google_name))

spdf_fortified <- tidy(spdf, 
                       region = "google_name")

# Calculate the centroid of each hexagon to add the label
centers <- cbind.data.frame(data.frame(gCentroid(spdf, 
                                                 byid = TRUE), 
                                       id = spdf@data$iso3166_2))

spdf_states <-  spdf_fortified %>%
  left_join(brews_data, 
            by = c("id" = "state_name"))

# Prepare binning
spdf_states$bin <-
  cut(
    spdf_states$n_breweries ,
    breaks = c(seq(0, 40, 5), Inf),
    labels = c(
      "0-5",
      "5-10",
      "10-15",
      "15-20",
      "20-25",
      "25-30",
      "30-35",
      "35-40",
      "40+"
    ),
    include.lowest = TRUE
  )

# Filter out DC
spdf_states %<>% 
  filter(!is.na(n_breweries)) %>%
  arrange(n_breweries)

# Create group indices 
spdf_states$group_id <-
  spdf_states %>% 
  group_indices(n_breweries, id)

# Plot
p <-   ggplot() +
  geom_polygon(
    data = spdf_states,
    aes(
      fill = bin,
      x = long,
      y = lat,
      group = group,
      frame = group_id,
      cumulative = TRUE
    ),
    size = 0,
    alpha = 0.9
  ) +
  geom_text(
    data = centers,
    aes(x = x, y = y, label = id),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  theme_void() +
  scale_fill_viridis_d(
    name = "Number of breweries",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = 'top',
      nrow = 1
    ),
    end = 0.86
  ) +
  ggtitle("A map of craft breweries, state by state") +
  theme_custom +
  theme(
    legend.position = c(0.5, 0.9),
    plot.title = element_text(
      size = 22,
      hjust = 0.5,
      color = "#22211d",
      margin = margin(
        b = -0.1,
        t = 0.4,
        l = 2,
        unit = "cm"
      )
    ),
  )  +
  labs(caption = "Source: https://circleup.com/blog/2017/10/05/the-rise-of-microbreweries-in-america-a-look-at-the-numbers/")

#p

# Animate
gganimate(
  p,
  "week15/craft_beer_usa.gif",
  title_frame = F,
  ani.width = 800,
  ani.height = 600,
  interval = 0.01,
  loop = 1
)

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gganimate)
library(stringr)
library(readxl)

#download.file("https://github.com/rfordatascience/tidytuesday/blob/master/data/week18_dallas_animals.xlsx?raw=true", destfile = "week18/shelter_data.xlsx")

shelter_data <- read_excel("week18/shelter_data.xlsx", 
                           sheet = "simple")

shelter_data_raw <- read_excel("week18/shelter_data.xlsx", 
                           sheet = "raw")

glimpse(shelter_data)

shelter_data %>% add_count(animal_id, sort = TRUE) %>% filter(n>1) %>% arrange(animal_id, desc(n)) %>% View

shelter_data_raw %>% count(reason) %>% View

shelter_data_raw %>%
  filter(intake_type == "OWNER SURRENDER", outcome_type == "EUTHANIZED", animal_type == "DOG") %>%
  mutate(numeric_out_date = as.numeric(outcome_date),
         outcome_date = as.Date.numeric(x = numeric_out_date, origin="1899-12-30")) -> euthanize_data

# Tracts need leading 0's
to_tract <- function(length, var) {
  sprintf("%s%s", paste0(rep_len(0, 6-length), collapse = ""), var)
}


euthanize_data %>% 
  #rowwise() %>% 
  mutate(census_length = str_count(census_tract),
         tract =  purrr::map2_chr(census_length, census_tract, to_tract)) -> euthanize_tract_data

dallas %>% 
  left_join(euthanize_tract_data %>% 
  group_by(outcome_date, tract) %>% 
  summarise(N = n()), by = c("TRACTCE"="tract"))

library(tigris)

options(tigris_class = "sf")

texas <- tracts(state = "TX", cb = TRUE)

dallas <- tracts(state = "TX", county = "Dallas", cb = TRUE)

dallas

dallas %>% 
  left_join(euthanize_tract_data %>% 
              group_by(outcome_date, tract) %>% 
              summarise(N = n()), by = c("TRACTCE"="tract")) %>% 
  filter(!is.na(outcome_date)) %>% 
ggplot() +
  geom_sf(aes(fill = N))  +
  # Here comes the gganimate specific bits
  labs(title = 'Date: {frame_time}') +
  transition_time(outcome_date) +
  #transition_states(outcome_date, transition_length = 1, state_length = 1) +
  ease_aes('linear') +
  NULL -> p

p

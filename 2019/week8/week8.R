library(here)
library(dplyr)
library(ggplot2)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

phd_field %>% 
  filter(broad_field == "Other",
         major_field == "Business management and administration") %>% 
  group_by(year, field) %>% 
  summarise(n = sum(n_phds, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(field, year) %>% 
  group_by(field) %>% 
  mutate(percent_change = 1+(n-lag(n))/lag(n)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = log(n), color = field))


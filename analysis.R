data <- read.csv("data/shootings-2018.csv", 
                 stringsAsFactors = FALSE)
library("dplyr")
library("ggplot2")
library(ggmap)


number_of_shootings <- nrow(data)
number_lives_lost <- sum(data$num_killed)

most_impacted_city <- group_by(data, city) %>% 
  select(city,num_killed, num_injured) %>% 
  mutate(total_damage = num_killed + num_injured) %>% 
  arrange(-total_damage) %>% 
  filter(total_damage == max(total_damage)) %>% 
  pull(city)
  
lat <- select(data, lat)
long <- select(data, long)



visualization <- ggplot(map_data("usa")) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +
  geom_point(data = data, mapping = aes(x = long, y = lat),
             color = "red") +
  coord_map()

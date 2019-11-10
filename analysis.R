data <- read.csv("data/shootings-2018.csv", 
                 stringsAsFactors = FALSE)

data$damage = data$num_killed + data$num_injured
state_pop <- read.csv("data/State Populations.csv")
library("dplyr")
library("ggplot2")
library("plotly")


number_of_shootings <- nrow(data)
number_lives_lost <- sum(data$num_killed)

most_impacted_city <- group_by(data, city) %>% 
  mutate(total_damage = num_killed + num_injured) %>% 
  arrange(-total_damage) %>% 
  head(1) %>% 
  pull(city)
  
east_of_the_mississppi <- filter(data, long > -90) %>% 
  summarise(e_miss = sum(damage)) %>% 
  pull(e_miss)


west_of_the_mississppi <- filter(data, long <  -90) %>% 
  summarise(w_miss = sum(damage)) %>% 
  pull(w_miss)

lat <- select(data, lat)
long <- select(data, long)


data$damage = data$num_killed + data$num_injured

visualization <- ggplot(map_data("usa")) + 
  geom_polygon(aes(x = long, y = lat)) +
  geom_point(data = data, mapping = aes(x = long, y = lat),
             color = "red", size = (data$most_damage / 5)) 



graph <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray100"),
  subunitcolor = toRGB("gray60"),
  countrycolor = toRGB("gray60"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

usa_plot <- plot_geo(data, x = ~long, y = ~lat) %>%
  add_markers(
    color = ~damage, text = ~paste(city, state, address, 
    paste("Number killed: ", num_killed), 
    paste("Number injured: ", num_injured), sep = "<br />"), 
    size = ~(damage),
    hoverinfo = "text"
  ) %>%
  layout(
    title = 'Shootings in 2018 <br />(Hover for more details)', geo = graph
  )

#Shooting by state

states_by_damage <- select(data, state, damage) %>% 
  group_by(state) %>% 
  summarise(damage_per_state = sum(damage)) %>% 
  ggplot(aes(x = state, y = damage_per_state)) + 
  geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label=damage_per_state), 
                           vjust=0, color="red", size=2.5)
  
state_pop$state = state_pop$State
with_pop <- left_join(state_pop, data, copy = "state") 
with_pop$State <- NULL
with_pop$most_damage <- NULL

by_pop <- group_by(with_pop, state) %>% 
  summarise(mean(X2018.Population), sum(damage))

cleaned_by_pop <- na.omit(by_pop) 

# ggplot states by size and state 

  
  
  
  
  
library("dplyr")
library("ggplot2")
library("plotly")
data <- read.csv("data/shootings-2018.csv", 
                 stringsAsFactors = FALSE)

data$damage = data$num_killed + data$num_injured
state_pop <- read.csv("data/State Populations.csv")
#vehicle_deaths <- read.csv("data/Accident Mortality by State.csv")


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

#Parkland data
parkland <- filter(data, city == "Pompano Beach (Parkland)")
p_date <- parkland[1,1]
p_location <- parkland[1,3]
p_killed <- parkland[1,5]
p_injured <- parkland[1,6]
p_damage <- parkland[1,9]
#Summary table

sum_table <- group_by(data, state) %>% 
  summarise(Damage = sum(num_killed + num_injured)) %>% 
  arrange(-Damage)

damage <- sum(sum_table$Damage)
top10_damage <- round(sum(sum_table$Damage[1:10])/ damage * 100, digits=2)


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
    title = 'Shootings in 2018 <br />(Hover for more details)', geo = graph)

#Shooting by state

states_by_damage <- select(data, state, damage) %>% 
  group_by(state) %>% 
  summarise(damage_per_state = sum(damage)) %>% 
  ggplot(aes(x = state, y = damage_per_state)) + 
  geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label=damage_per_state), 
                           vjust=0, color="red", size=2.5)
  
state_pop$state = state_pop$State
with_pop <- left_join(state_pop, data, by = "state") 
with_pop$State <- NULL
with_pop$most_damage <- NULL

by_pop <- group_by(with_pop, state) %>% 
 summarise(pop = mean(X2018.Population), dam = sum(damage))




cleaned_by_pop <- na.omit(by_pop) 

# ggplot states by size and state 
state_and_pop <- ggplot(data = cleaned_by_pop) +
  geom_col(mapping = aes(x = state, y = pop)) + 
  geom_col(mapping = aes(x = state, y = dam, fill = "red")) +
  coord_flip() + labs(fill = "Damage") + 
  ggtitle("Damage vs. State Population") +
  labs(y = "People")
  
  
  
  
  
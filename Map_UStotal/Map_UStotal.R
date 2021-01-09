library(tidyverse)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(ggpubr)

### US GAS DATA
library(USgas)

### US Map: states consumption gradient
gas_data_all <- us_total

gas_data_all_edit <- gas_data_all %>%
  group_by(state) %>%
  mutate(highest = max(y),
         max_year = ifelse(highest == y,year,NA))

gas_data_all_edit2 <- gas_data_all_edit %>%
  filter(!is.na(max_year)) %>%
  select(!max_year) %>%
  select(!y)

gas_data_all_edit2 <- gas_data_all_edit2 %>%
  filter(!grepl('Hawaii', state)) %>%
  filter(!grepl('District of Columbia', state)) %>%
  filter(!grepl('Alaska', state)) %>%
  filter(!grepl('U.S.', state)) %>%
  filter(!grepl('Federal', state)) 

gas_data_all_edit3 <- gas_data_all_edit2 %>%
  mutate(region = tolower(state))

# Map data: 48 states, color gradient for high-low of highest consumption data
library(maps)
states_map <- map_data("state")
map.dat <- left_join(states_map, gas_data_all_edit3, by = "region")

plot.mapAll <- ggplot(map.dat, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = highest), color = "white") +
  scale_fill_gradient(name = "Gas Consumption Scale",
                      low = "blue", high = "red",
                      breaks=c(50000,4500000),
                      labels=c("Low","High"),
                      limits = c(13800,4619800),
                      guide = guide_colourbar(
                        direction = "horizontal",
                        barheight = unit(2, units = "mm"),
                        barwidth = unit(50, units = "mm"),
                        draw.ulim = F,
                        title.position = 'top',
                        title.hjust = 0.5,
                        label.hjust = 0.5)) +
  theme_map() +
  labs(x = NULL,
       y = NULL,
       title = "US Gas Consumption (1997-2019)",
       subtitle = "Highest consumption by state",
       caption = "Data:USgas package") 

plot.mapAll <- plot.mapAll + 
  theme(plot.title = element_text(color="navyblue", size=16, face="bold.italic", hjust = 0.5),
        plot.subtitle = element_text(color="black", size=11, face="bold", hjust = 0.5))
  



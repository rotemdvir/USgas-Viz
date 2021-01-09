library(tidyverse)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(ggpubr)

### US GAS DATA
library(USgas)

# Treemap: plot the highest consumption with the year for all 48 states 
library(treemapify)

### US data: states consumption 
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

# Treemap Plot (with gradient and rainbow colors)
treeplot1 <- ggplot(gas_data_all_edit2, aes(area = highest, fill = highest, label = state, subgroup = year)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black",
                    place = "centre", grow = T) +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "white", fontface = "italic", min.size = 0) +
  scale_fill_gradientn(colours = rainbow(10),
                    name = "Gas Consumption Scale",
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
  labs(x = NULL, y = NULL,
       title = "US Gas Consumption (1997-2019)",
       subtitle = "Highest consumption by state (year subgroups)",
       caption = "Data:USgas package") 

treeplot1 <- treeplot1 + 
  theme(legend.position = "bottom",
  plot.title = element_text(color="navyblue", size=16, face="bold.italic", hjust = 0.5),
  plot.subtitle = element_text(color="black", size=11, face="bold", hjust = 0.5))
  
# Treemap Plot (with gradient and two colors)
treeplot2 <- ggplot(gas_data_all_edit2, aes(area = highest, fill = highest, label = state, subgroup = year)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black",
                    place = "centre", grow = T) +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "white", fontface = "italic", min.size = 0) +
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
  labs(x = NULL, y = NULL,
       title = "US Gas Consumption (1997-2019)",
       subtitle = "Highest consumption by state (year subgroups)",
       caption = "Data:USgas package") 

treeplot2 <- treeplot2 + 
  theme(legend.position = "bottom",
        plot.title = element_text(color="navyblue", size=16, face="bold.italic", hjust = 0.5),
        plot.subtitle = element_text(color="black", size=11, face="bold", hjust = 0.5))



# Treemap Plot: group highest per state by year (year groups by color)
## Create larger color brewer (12 items in year group)
library(RColorBrewer)
d.cols <- 12
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(d.cols)

# Plot 1: Multiple colors for year group
treeplot3 <- ggplot(gas_data_all_edit2, aes(area = highest, fill = factor(year), label = state, subgroup = year)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black",
                    place = "centre", grow = T) +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "grey", fontface = "italic", min.size = 0) +
  scale_fill_manual(values = mycolors) +
  labs(x = NULL, y = NULL,
       title = "US Gas Consumption (1997-2019)",
       subtitle = "Highest consumption by state (year subgroups)",
       caption = "Data:USgas package") 

treeplot3 <- treeplot3 +
  theme(legend.position = "none",
        plot.title = element_text(color="navyblue", size=16, face="bold.italic", hjust = 0.5),
        plot.subtitle = element_text(color="black", size=11, face="bold", hjust = 0.5)) 

# Plot 2: Multiple facets and colors for year group

## Reduce data to improve plot visual display (years: 2012-2019)
gas_data_all_edit3 <- gas_data_all_edit2 %>%
  filter(year > 2011)

treeplot4 <- ggplot(gas_data_all_edit3, aes(area = highest, fill = factor(year), label = state)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black",
                    place = "centre", grow = T) +
  facet_wrap(~factor(year), nrow = 3) +
  scale_fill_manual(values = mycolors) +
  labs(x = NULL, y = NULL,
       title = "US Gas Consumption (2012-2019)",
       subtitle = "Highest consumption by state (year subgroups)",
       caption = "Data:USgas package") 


treeplot4 <- treeplot4 +
  theme(legend.position = "none",
        plot.title = element_text(color="navyblue", size=16, face="bold.italic", hjust = 0.5),
        plot.subtitle = element_text(color="black", size=11, face="bold", hjust = 0.5)) 
  


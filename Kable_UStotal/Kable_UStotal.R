library(tidyverse)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(ggpubr)

### US GAS DATA
library(USgas)

## Tables of data: US states level
library(kableExtra)

## Create datasets
dat.min <- us_total %>%
  group_by(state) %>%
  mutate(lowest = min(y),
         min_year = ifelse(lowest == y, year, NA))

dat.min2 <- dat.min %>%
  filter(!is.na(min_year)) %>%
  select(!y) %>%
  select(!year) %>%
  filter(!grepl('U.S.', state)) %>%
  filter(!grepl('Federal', state)) 

dat.max <- us_total %>%
  group_by(state) %>%
  mutate(highest = max(y),
         max_year = ifelse(highest == y, year, NA))

dat.max2 <- dat.max %>%
  filter(!is.na(max_year)) %>%
  select(!y) %>%
  select(!year) %>%
  filter(!grepl('U.S.', state)) %>%
  filter(!grepl('Federal', state)) 

dat.mean <- us_total %>%
  group_by(state) %>%
  summarise(avg = mean(y),
            med = median(y),
            IQR = IQR(y)) %>%
  filter(!grepl('U.S.', state)) %>%
  filter(!grepl('Federal', state)) 

dat.table <- left_join(dat.max2, dat.min2, by = "state")
dat.table <- left_join(dat.table, dat.mean, by = "state")

# Calculate top 10 in average consumption (for display in blog post)
dat10 <- dat.table %>%
  arrange(desc(avg)) 

dat10 <- dat10 %>%
  filter(avg > 650000) %>%
  arrange(desc(highest))

# Display top 10
dat10 %>%
  kbl(col.names = c("State", "Total", "Year", "Total",
                    "Year", "Average", "Median.", "IQR"), booktabs = T,
      caption = "US Gas total consumption (1997-2019): Top 10 states") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Highest Consumption" = 2, "Lowest Consumption" = 2, "Summary Stats" = 3)) 

# Calculate bottom 10 in average consumption 
dat10min <- dat.table %>%
  arrange(desc(avg)) 

dat10min <- dat10min %>%
  filter(avg < 90000) %>%
  arrange(desc(lowest))

## Display bottom 10
dat10min %>%
  kbl(col.names = c("State", "Total", "Year", "Total",
                    "Year", "Average", "Median.", "IQR"), booktabs = T,
      caption = "US Gas total consumption (1997-2019): Bottom 10 states") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Highest Consumption" = 2, "Lowest Consumption" = 2, "Summary Stats" = 3)) 

# Display all data: multiple versions
## Option 1: grouping variables (and blue background for selected rows)
dat.table %>%
  kbl(col.names = c("State", "Total", "Year", "Total",
                    "Year", "Average", "Median.", "IQR"), booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Highest Consumption" = 2, "Lowest Consumption" = 2, "Summary Stats" = 3)) %>%
  row_spec(3:6, bold = T, color = "white", background = "darkblue")

## Option 2: grouping variables (wide scale table)
dat.table %>%
  kbl(col.names = c("State", "Highest", "Year", "Lowest",
                    "Year", "Average", "Median.", "IQR"), booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c("", "Gas Consumption" = 4, "Summary Stats" = 3)) 

# Option 3: grouping observations (by order)
dat.table %>%
  kbl(col.names = c("State", "Total", "Year", "Total",
                    "Year", "Average", "Median.", "IQR"), booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Highest Consumption" = 2, "Lowest Consumption" = 2, "Summary Stats" = 3)) %>%
  pack_rows("Group 1", 1, 5) %>%
  pack_rows("Group 2", 6, 10) %>%
  pack_rows("Group 3", 11, 15) 

# Option 4: grouping observations (by census regions)
## Details: I forked a Git project which had census data on regions. 
## Upload xls file and join to dat.table and top 10 for blog

names(dat.table)[names(dat.table) == "state"] <- "State"
dat.table <- left_join(dat.table, US_regions, by = "State")

dat.reg <- dat.table %>%
  arrange(desc(Division)) %>%
  arrange(desc(Region))

## Table groups by regions: (1 West) 1-13; (2 South) 14-30 
## (3 NE 31-39); (4 Midwest 40-51)
## Export table to html file

dat.reg2 <- dat.reg %>%
  select(State,highest,max_year,lowest,min_year,avg,med,IQR,`State Code`,Division)

dat.reg2 %>%
  kbl(col.names = c("State", "Total", "Year", "Total",
                    "Year", "Average", "Median.", "IQR", "State_Code", "Division"), booktabs = T,
      caption = "Gas Consumption: US States (1997-2019)") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("", "Highest Consumption" = 2, "Lowest Consumption" = 2, "Summary Stats" = 3, "States Data" = 2)) %>%
  pack_rows("Region: West", 1, 13) %>%
  pack_rows("Region: South", 14, 30) %>%
  pack_rows("Region: Northeast", 31, 39) %>%
  pack_rows("Region: Midwest", 40, 51) %>%
  save_kable("~/Dropbox/TAMU/New_Projects/Git_edits/US_Gas/Kable_UStotal/tab.html")





library(tidyverse)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(ggpubr)
library(reshape2)

### US GAS DATA
library(USgas)

# Using residential to find the share of top 8 states in certain years

d.res <- us_residential 

names(d.res)[names(d.res) == "y"] <- "month_total"

res.long <- reshape(d.res, v.names = "month_total", idvar = "date",
                timevar = "state", direction = "wide")

st.avg <- res.long %>%
  summarise(cal = mean(month_total.California, na.rm = T),
            Illn = mean(month_total.Illinois, na.rm = T),
            Indy = mean(month_total.Indiana, na.rm = T),
            Mich = mean(month_total.Michigan, na.rm = T),
            Mass = mean(month_total.Massachusetts, na.rm = T),
            NJ = mean(`month_total.New Jersey`, na.rm = T),
            NY = mean(`month_total.New York`, na.rm = T),
            OH = mean(month_total.Ohio, na.rm = T),
            TX = mean(month_total.Texas, na.rm = T),
            Penn = mean(month_total.Pennsylvania, na.rm = T),
            WIS = mean(month_total.Wisconsin, na.rm = T),
            Geo = mean(month_total.Georgia, na.rm = T))


names(res.long)[names(res.long) == "month_total.U.S."] <- "month_total.US"

# Compute share of top 8 states; and share of rest of sample

st.avg <- res.long %>%
  mutate(share_CAL = month_total.California/month_total.US,
         share_Iln = month_total.Illinois/month_total.US,
         share_MICH = month_total.Michigan/month_total.US,
         share_NJ = `month_total.New Jersey`/month_total.US,
         share_NY = `month_total.New York`/month_total.US,
         share_OH = month_total.Ohio/month_total.US,
         share_TX = month_total.Texas/month_total.US,
         share_Penn = month_total.Pennsylvania/month_total.US,
         top8_sum = month_total.California + month_total.Illinois + month_total.Michigan +
           `month_total.New Jersey` + `month_total.New York` + month_total.Ohio + 
           month_total.Texas + month_total.Pennsylvania,
         share_rest = (month_total.US - top8_sum)/month_total.US)

# Create reduced data of selected year/month 
## Add year var
st.share <- st.avg %>%
  filter(date == "1993-06-01" | date == "2012-01-01") %>%
  select(share_CAL, share_Iln, share_MICH, share_NJ, share_NY, share_OH, share_TX, share_Penn, share_rest) %>%
  mutate(year = c("1993", "2012")) %>%
  relocate(year)

# Plot shared consumotion in selected years: donut charts (with facets for years)
## Change data to long format
# EDIT: titles, legend, background
share.long <- gather(st.share, state, prop, share_CAL:share_rest, factor_key = T)

share.plot <- ggplot(share.long, aes(x = 2, y = prop, fill = state, group = factor(year))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(prop*100,1), "%")), position = position_stack(vjust = 0.5)) +
  facet_grid(~year) +
  xlim(0.5, 2.5) +
  ggtitle("US Gas Monthly consumption share: Top 8 states (June 1993; January 2012)") +
  theme_bw() +
  theme(strip.background =element_rect(fill="lightblue")) +
  theme(strip.text = element_text(colour = 'black'))

share.plot <- share.plot + scale_fill_discrete(name = "Share of Consumption:\nTop 8 states",
                        breaks = c("share_CAL", "share_Iln", "share_MICH", "share_NJ", "share_NY",
                                   "share_OH", "share_TX", "share_Penn", "share_rest"),
                        labels = c("California", "Illinois", "Michigan", "New Jersey", "New York",
                                   "Ohio", "Texas", "Penn", "Rest")) +
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(color = "navyblue", size=16, face="bold.italic", hjust = 0.5))

# Plot trends in total consumpotion for top 8 (based on residential data)
## Generate top 8 data of total consumption
top.dat <- us_total %>%
  filter(state == "Texas" | state == "Ohio" | state == "Michigan" | state == "New Jersey" 
         | state == "Illinois" | state == "New York" | state == "California" | state == "Pennsylvania")

top.plot <- ggplot(top.dat, aes(x = factor(year), y = y, group = state)) +
  geom_line(aes(color = state)) +
  geom_point(aes(color = state)) +
  ggtitle("Total consumption trends (1997-2019)") + ylab("Total Consumption - Yearly") + xlab("") +
  scale_x_discrete(breaks = c(1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019)) +
  theme_classic() +
  theme(legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
        plot.title = element_text(color = "navyblue", size=16, face="bold.italic", hjust = 0.5),
        axis.title.y = element_text(margin = margin(r=10)))


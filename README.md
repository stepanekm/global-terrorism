Global Terrorism in Time
================
25 April, 2018

-   [Story Summary](#story-summary)
-   [First Plot](#first-plot)
-   [Second Plot](#second-plot)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Story Summary
-------------

In the two last decades, we have witnessed the rapid increase in the number of terroristic attacks all around the world. This troublesome trend undoubtedly had and still has a big influence on the global politics. Right-wing political parties in developed nations often use the fight against terrorism and refusal to take refugees as the main point of their programs. While common people are scared and easily influenced by well worked-out campaigns, the parties from the right political spectrum enjoy the rise in the number of their voters.

Moreover, any terroristic attack which kills at least a one person in countries such as the United States, France or Germany stir up the wave of solidarity on the social media. But does the terrorism really represent such a big threat to our developed nations? And why not that many people have compassion with countries where the terrorists rage the most?

First Plot
----------

``` r
library(dplyr)
library(tidyverse)
library(mapdata)
library(maps)
library(RColorBrewer)
library(gganimate)

gt <- read_csv("data/database.csv")

gtclean <- gt %>%
  filter(crit1 == 1, crit2 == 1, nkill > 0) %>%
  group_by(country_txt, iyear, nkill) %>%
  summarise(
    count = n(),
  ) %>% 
  mutate(
    killed = nkill * count
  ) %>% 
  group_by(country_txt, iyear) %>%
  summarise(killed = sum(killed))

skilled <- gtclean %>% 
  group_by(iyear) %>% 
  summarise(killed = sum(killed))

myPalette <- colorRampPalette(rev(brewer.pal(6, "OrRd")))

gtscatter1 <- gt %>%
  filter(crit1 == 1, crit2 == 1, nkill > 0) %>%
  group_by(region_txt, iyear, nkill) %>%
  summarise(
    count = n(),
  ) %>% 
  mutate(
    killed = nkill * count
  ) %>% 
  group_by(region_txt, iyear) %>%
  summarise(killed = sum(killed)) %>% 
  mutate(
    id = paste(region_txt, iyear, sep="")
  )

gtscatter2 <- gt %>%
  filter(crit1 == 1, crit2 == 1, nkill > 0) %>%
  group_by(region_txt, iyear) %>%
  summarise(
    count = n(),
  ) %>%
  mutate(
    id = paste(region_txt, iyear, sep="")
  )

scattergrouped <- inner_join(gtscatter1, gtscatter2, by="id") %>% 
  select("region_txt.x", "iyear.x", "killed", "count") %>% 
  mutate(ratio = killed / count) %>% 
  filter(region_txt.x == "Middle East & North Africa")

scattergrouped2 <- inner_join(gtscatter1, gtscatter2, by="id") %>% 
  select("region_txt.x", "iyear.x", "killed", "count") %>% 
  mutate(ratio = killed / count) %>% 
  filter(region_txt.x == "Sub-Saharan Africa")

scattergrouped3 <- inner_join(gtscatter1, gtscatter2, by="id") %>% 
  select("region_txt.x", "iyear.x", "killed", "count") %>% 
  mutate(ratio = killed / count) %>% 
  filter(region_txt.x == "South Asia")

scattergrouped4 <- inner_join(gtscatter1, gtscatter2, by="id") %>% 
  select("region_txt.x", "iyear.x", "killed", "count") %>% 
  mutate(ratio = killed / count) %>% 
  filter(region_txt.x == "North America")

scattergrouped5 <- inner_join(gtscatter1, gtscatter2, by="id") %>% 
  select("region_txt.x", "iyear.x", "killed", "count") %>% 
  mutate(ratio = killed / count) %>% 
  filter(region_txt.x == "Western Europe")

ggplot() +
  geom_line(data = skilled, aes(x=iyear, y=killed, group = 1, color = "#B30000")) +
  geom_line(data = scattergrouped, aes(x = iyear.x, y = killed, group =1, color = "#e6550d")) +
  geom_line(data = scattergrouped2, aes(x = iyear.x, y = killed, group =1, color = "#FDD49E")) + 
  geom_line(data = scattergrouped3, aes(x = iyear.x, y = killed, group =1, color = "#FDBB84")) +
  geom_line(data = scattergrouped4, aes(x = iyear.x, y = killed, group =1, color = "black")) + 
  geom_line(data = scattergrouped5, aes(x = iyear.x, y = killed, group =1, color = "grey")) +
  scale_x_continuous(breaks = seq(1970, 2016, 2)) +
  labs(y="Number of Deaths", title="Number of Terrorist Attacks Victims from 1970 to 2016", x="Year") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold", color = "#4e4d47"),
        axis.title = element_text(size = 8, color = "#4e4d47"),
        axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0)),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = c(-0.01,0)) + 
  scale_colour_manual(name = '', 
                      values =c('#B30000'='#B30000','#e6550d'='#e6550d', "#FDD49E" = "#FDD49E",
                                "#FDBB84" = "#FDBB84", "grey" = "black", "black" = "grey"),
                      labels = c('Total','Middle East & North Africa', "Sub-Saharan Africa",
                                 "South Asia","North America","Western Europe"))
```

![](readme-figs/gt-1.png)

The first graph reveals us a couple of trends in the number of deaths caused by terrorism. On the first glance, we can spot a rapid surge since 2011 in the figure of people killed by the terroristic attack. The number of deaths peaked in 2014 when attacks claimed 43.141 victims. That’s a hardly believable increase of 434% between those years.

The second important output of this graph is the derived information, where terrorism killed the most. Western European countries and the United States have just a tiny fraction of all victims. The most critical regions are Middle East, Northern and Sub-Saharan Africa and South Asia.

Second Plot
-----------

``` r
worldmap <- map_data("world")
newworld <- worldmap %>%
  filter(region != "Antarctica")
newworld$region <- recode(newworld$region
                          ,'USA' = 'United States'
                          ,'UK' = 'United Kingdom'
)

world <- ggplot() +
  geom_polygon(data = newworld, aes(x = long, y = lat, group = group), fill = "grey", color = "#4e4d47") + 
  coord_quickmap() +
  theme_void()

grouped <- inner_join(newworld, gtclean, by = c('region' = 'country_txt')) %>%
  filter(region != "Antarctica") 

map <- world +
  geom_polygon(data = grouped, aes(x = long, y = lat, group = group, fill = killed, frame = iyear), color = "#4e4d47") +
  coord_quickmap() +
  scale_fill_gradientn(colours = rev(myPalette(5)),
                       na.value="#4e4d47",
                       breaks = c(1, 10, 50, 200, 1000, 8000),
                       trans = "log10",
                       name = "People Killed",
                       guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(6, units = "mm"),
                                            label.position = "bottom", title.position = 'top', nrow=1)) +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.05, face = "bold", color = "#4e4d47"),
        plot.caption = element_text(size = 10, hjust = 0.97, vjust = 1.2, color = "#4e4d47"),
        legend.position = c(0.11, 0.01),
        plot.background = element_rect(fill = "#f5f5f2", color = NA)) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Number of People Who Died of Terrorist Attacks in",
       caption="Source: start.umd.edu | By Martin Stepanek")
```

![](https://github.com/stepanekm/global-terrorism/blob/master/map.gif) This global map provides us with a different view on the global terrorism. We can easily identify the countries where the terroristic attacks took over the years the most lives. Over the 46-yearlong tracked period the most severely stricken were Iraq, Afghanistan, Pakistan, and Nigeria.

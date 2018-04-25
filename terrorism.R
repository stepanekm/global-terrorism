library(dplyr)
library(tidyverse)
library(mapdata)
library(maps)
library(RColorBrewer)
library(gganimate)

gt <- read_csv("data/database.csv")

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


#from graph 2 text
skilled2 <- gtclean %>% 
  group_by(country_txt) %>% 
  summarise(killed = sum(killed)) %>%
  filter(killed > 11000) %>% 
  arrange(desc(killed)) 

ggplot(skilled2, aes(reorder(country_txt, -killed), weight=killed)) +
  geom_bar(width=0.75) +
  labs(title = "Number of suicides per 100.000 people ", x="State", y="Suicides") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 8),
        axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0)))

#for geom_line
skilled <- gtclean %>% 
  group_by(iyear) %>% 
  summarise(killed = sum(killed))

grouped <- inner_join(newworld, gtclean, by = c('region' = 'country_txt')) %>%
  filter(region != "Antarctica") 

myPalette <- colorRampPalette(rev(brewer.pal(6, "OrRd")))

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

gganimate(map, ani.width = 900, ani.height = 500)

ggplot(skilled, aes(x=iyear, y=killed)) +
  geom_line(color = rev(myPalette(1))) +
  scale_x_continuous(breaks = seq(1970, 2016, 2)) +
  labs(y="Killed", title="Terrorist Attacks from 1970 to 2016", x="Year") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold", color = "#4e4d47"),
        axis.title = element_text(size = 8, color = "#4e4d47"),
        axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0)),
        plot.background = element_rect(fill = "#f5f5f2", color = NA))

#scatter-plot
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
  labs(y="Killed", title="Terrorist Attacks from 1970 to 2016", x="Year") +
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
                                "#FDBB84" = "#FDBB84", "black" = "black", "grey" = "grey"),
                      labels = c('Total','Middle East & North Africa', "Sub-Saharan Africa",
                                 "South Asia","North America","Western Europe"))

geom_line(data = alcohol, aes(x = Year, y = count, group = 1), color = "red") 

ggplot(scattergrouped, aes(reorder(region_txt.x, -ratio), weight=ratio)) +
  geom_bar(width=0.75) +
  labs(title = "Number of suicides per 100.000 people ", x="State", y="Suicides") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 8),
        axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0)))
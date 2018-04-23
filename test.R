library(dplyr)
library(tidyverse)
library(mapdata)
library(maps)
library(RColorBrewer)
library(gganimate)
options(repr.plot.width=9, repr.plot.height=5)

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

gganimate(map, "map.gif", ani.width = 900, ani.height = 500)

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

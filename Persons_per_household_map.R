#' -------------------------------------------------------------
#' Title : Average Persons's per Household, KNBS 2019 census ######
#' Author: @johnmutiso
#' Date  : 2020MAY31
#' --------------------------------------------------------------

# devtools::install_github('Shelmith-Kariuki/rKenyaCensus')

library(rKenyaCensus)
library(tidyverse)
library(sf)
library(extrafont)
loadfonts(device = 'win')

# Map data ---------------------------------------------------------
KECounties <- rKenyaCensus::KenyaCounties_SHP %>% 
  sf::st_as_sf() %>%
  select(County, Area, geometry)
  
# Average persons per household per county
household_data <-
  rKenyaCensus::V2_T2.2 %>%
  mutate(persons_hld = Sex_Total / Households_Total) %>% #persons per household
  select(County, Sex_Total, Households_Total, persons_hld)

data <- left_join(household_data[-1,], KECounties, by = 'County') %>% st_as_sf()

# Plot 1 ----
plot1 <- 
data %>%
  ggplot() +
  geom_sf(aes(fill = persons_hld), col = 'snow1') +
  geom_sf_text(
    aes(label = County),
    color = '#ffffff',
    size = 2.5,
    fontface = 'bold'
  ) +
  scale_fill_viridis_c('') +
  coord_sf() +
  labs(title = 'Average Number of Persons per\nHousehold by County',
       caption = 'Graphic: @johnmutiso\nData: @KNBS 2019 Census\nvia rKenyaCensus (by @Shel-Kariuki)') +
  theme_void() +
  theme(
    legend.position = c(0.7, 0.9),
    legend.direction = 'horizontal',
    legend.key.width = unit(1.5, 'cm'),
    legend.key.height = unit(0.9, 'cm'),
    legend.text = element_text(size = 14),
    plot.title = element_text(
      hjust = 0.5,
      family = 'Courier New',
      size = 20,
      face = 'bold',
      color = '#bada55'
    ),
    plot.background = element_rect(fill = '#666666'),
    plot.caption = element_text(colour = '#ffffff')
  )

# Cartogram ---
household_data_cartogram <- cartogram::cartogram_cont(data, 'persons_hld')

plot_cartogram <- 
  household_data_cartogram %>%
  ggplot() +
  geom_sf(aes(fill = persons_hld), col = 'snow1') +
  geom_sf_text(
    aes(label = County),
    color = '#ffffff',
    size = 2.5,
    fontface = 'bold'
  ) +
  scale_fill_viridis_c('# persons\nper household') +
  coord_sf() +
  labs(title = 'Cartogram: Average Number of Persons per\nHousehold by County',
       caption = 'Graphic: @johnmutiso\nData: @KNBS 2019 Census\nvia rKenyaCensus (by @Shel-Kariuki)') +
  theme_void() +
  theme(
    legend.key.height = unit(1.8, 'cm'),
    legend.text = element_text(size = 14, color = 'white'),
    legend.title = element_text(size = 14, color = 'white'),
    plot.title = element_text(
      hjust = 0.5,
      family = 'Courier New',
      size = 20,
      face = 'bold',
      color = 'white'
    ),
    plot.background = element_rect(fill = 'gray40'),
    plot.caption = element_text(colour = '#ffffff')
  )

# save
ggsave(
  filename = 'Cartogram_persons_per_hsehld.png',
  plot = plot_cartogram,
  device = 'png',
  dpi = 500,
  height = 9,
  width = 8,
  path = './'
)


#' --------------------------------------------------------------------------
#' Author: John Mutiso (@johnmutiso)
#' Data used: 
#' Date: 2020JUN07
#' Focus: Mobile Phone Use in Kenya based on (KNBS 2019 CENSUS)
#' --------------------------------------------------------------------------

# Reading libraries ---------------------
library(tidyverse)
library(sp)
library(rKenyaCensus)
library(ggrepel)
library(extrafont)
loadfonts(device = 'win')

# Kenya map - SF data ---------------------------------------------------------
KECounties <- rKenyaCensus::KenyaCounties_SHP %>% 
  sf::st_as_sf() %>%
  select(County, Area, geometry)

data <- V4_T2.32

?V4_T2.32


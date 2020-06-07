#' --------------------------------------------------------------------------
#' Author: John Mutiso (@johnmutiso)
#' Purpose: Graphics for @Shelmith-Kariuki rKenyaCensus package launch
#' Data used: Table 2.3
#' Date: 2020APR27
#' --------------------------------------------------------------------------

# Reading libraries ---------------------
library(tidyverse)
library(rKenyaCensus)
library(ggrepel)
library(extrafont)

# fonts ---------------------------------
loadfonts(device = 'win')

# Reading data (table 2.3) --------------------------------------------------
tabV3_2_3 <- rKenyaCensus::V3_T2.3
head(tab2_3)

# +++++++++++++++++++++++++ PLOT 1 +++++++++++++++++++++++++++++++++++++++++++
# Filter the table 2.3 for county level population by sex -------------------
pop_county_sex <- 
  tabV3_2_3 %>% 
  filter(Age == "Total" & SubCounty == "ALL") %>%
  select(-c(Age, SubCounty)) %>% 
  pivot_longer(cols = -c(1,4), values_to = 'pop', names_to = 'sex') %>%  # covert to long format
  mutate_at(.vars = c(2,4), .funs = as.numeric) %>% 
  mutate(percent = round((pop/Total)*100, 2),
         percent1 = paste0(percent,'%')) 

# Total Population -----------------------------
tot_pop <-  
  tabV3_2_3 %>% 
  filter(Age == "Total" & SubCounty == "ALL") %>% 
  mutate_at(.vars = c(4:6), .funs = as.numeric) %>% 
  summarise(tot_pop = sum(Total),
            prop_male = (sum(Male)/tot_pop)*100,
            prop_female  = (sum(Female)/tot_pop)*100,
            tot_pop = tot_pop/1000000) %>% as.numeric()

names(tot_pop) <- c('tot_pop', 'prop_male', 'prop_female')

tot_pop <- map_dbl(tot_pop, round, 3)
  
# County name labels ---------------------------------------------------------
angle_1 <- seq(0,87, length.out = 13)
angle_2 <- seq(87,0, length.out = 13)
angle <- c(87-angle_1, -angle_1, angle_2, 270+angle_2)[1:47] # text rotation angle
hjust <- c(-0.05, rep(0.1,25), rep(0.8,21))


label_data <- 
  pop_county_sex %>% 
  distinct(County, .keep_all = T) %>% 
  arrange(desc(Total)) %>%
  mutate(angle = angle, 
         County_pop = paste0(County, '\n[', round(Total/1000000, 3),']')) %>% 
  select(County, Total, angle, County_pop)

pop_county_sex1 <- 
  pop_county_sex %>% 
  left_join(label_data, by = c('County','Total'))

# plot -----------------------------------------------------------------------
plot1 <- 
  ggplot(data = pop_county_sex1) +
  geom_bar(aes(x = reorder(County, desc(Total)), y = log2(pop), fill = sex), 
           stat = 'identity', position = position_stack()) +
  scale_fill_manual("", values = c('#ff7373','#576675'), 
                    labels = c(paste0('Female (', tot_pop[3], '%)'), paste0('Male (', tot_pop[2], '%)'))) +
  geom_text(aes(reorder(County, desc(Total)), y = log2(pop), label = percent1, 
                angle = angle), fontface = 'bold',
            position = position_stack(vjust = 0.5), size = 7, col = '#ffffff') +
  geom_text(data = label_data, aes(x = reorder(County, desc(Total)), 
                                   y = log10(Total), 
                                   label = County_pop), 
            nudge_y = 35, size = 7, family = 'Courier New', fontface = 'bold',
            col = '#003366', nudge_x = 0, hjust = hjust, angle = angle) +
  expand_limits(y = c(-20,15), x = c(48,52))+
  coord_polar(start = 0)+
  geom_text(aes(x = 0.5, y = -28), label = paste0("Total Population\n (",tot_pop[1], " Million)"), 
            size = 16, family = 'Bauhaus 93', col = '#407294') +
  labs(title = 'Kenyan Population Distribution by County and Gender [2019 Census]',
       subtitle = '[Population size in millions] and Gender Proportions',
       caption = 'Graphic: github.com/johnmutiso\nData: V3_T2.3 (rKenyaCensus package by @Shelmith-Kariuki)') +
  theme_void()+
  theme(axis.text.x = element_blank(), 
        legend.position = c(0.13,0.93),
        plot.background = element_rect(fill = '#dcedc1'),
        legend.key.size = unit(0.5, 'in'), 
        legend.text = element_text(size = 28, family = 'Courier New', face = 'bold', color = '#333333'), 
        plot.title = element_text(size =45, family = 'Colonna MT', face = 'bold', color = '#576675'),
        plot.subtitle = element_text(size = 34, family = 'Comic Sans MS', face = 'bold', hjust = 0, color = '#ff7373'),
        plot.caption = element_text(size = 20, family = 'Arial', color = '#333333', face = 'bold'))

# Save the plot ----------------------------------------------------------------------------
ggsave(plot = plot1, width = 18, height = 19.8, dpi = 400, 
       path = './', filename = 'plot1.png', device = 'png')

#+++++++++++++++++++++++++++++++ PLOT 2 +++++++++++++++++++++++++++++++++++++++++=+++
# Data ------------------------------------------------
tabV2_2_2 <- 
  rKenyaCensus::V2_T2.2 %>% str()
  mutate(persons_hld = Sex_Total/Households_Total) %>% #persons per household
  select(-c(3:8))

# Overall Kenya's Persons per household
persons_hld = tabV2_2_2 %>% filter(County=='KENYA') %>% select(persons_hld) %>% as.numeric() 
# plot ---------------------------------------------
plot2 <- 
  ggplot(data = tabV2_2_2 %>% filter(!County=='KENYA'),
         aes(reorder(County, persons_hld), persons_hld)) +
  geom_point(aes(size = `LandArea(Sq km)`,
                 fill = log10(`Density(Persons per Sq km)`)),
             pch = 21, alpha = 0.7, col = '#fff8e7') +
  geom_hline(yintercept = persons_hld, lty = 3, size = 1, color = '#e6e6fa') +
  geom_segment(aes(y = 2.5, yend = persons_hld - 0.2, 
                   x = reorder(County, persons_hld), xend = reorder(County, persons_hld)), 
               arrow = arrow(type = 'closed', length = unit(0.1, 'in')), 
               arrow.fill = '#eeeeee', color = '#c6e2ff', alpha = 0.3) +
  geom_text(aes(x = 6,y=4.9, label = 'Kenya`s average number\nof persons per household\n(Census-2019)'), col = '#c6e2ff') +
  geom_segment(aes(x = 6, xend = 6, y = 4.5, yend = 4), arrow = arrow(), col = '#c6e2ff') +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  ylim(c(2.5, 8)) +
  labs(y = 'Average Persons per Household', 
       title = "Kenya Counties Average Number of Persons per Household",
       subtitle = "Colored by Persons per Sq km & Point Size as County Size Sq km",
       caption = 'Graphic: github.com/johnmutiso\nData: V2_T2.2 (rKenyaCensus package by @Shelmith-Kariuki)') +
  scale_size_area("Land Area\nin Sq km", max_size = 22, breaks = c(1000,10000,30000,50000),labels = scales::comma) + 
  scale_fill_gradientn("Persons per\nSq km", colors = c('#abf300','#fcff00','#ffbd00','#ff7700','#ff0000'),
                       breaks = c(1,2,3), labels = c('10', '100', '1000')) +
  theme_minimal() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(0.3,0.8), 
        plot.background = element_rect(fill = '#0e2f44'), 
        panel.grid = element_line(linetype = 3, color = '#065535', size = 0.2), 
        legend.key.width = unit(0.7, 'in'), 
        legend.key.height = unit(0.2,'in'), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, colour = '#c6e2ff', family = 'Gabriola'),
        axis.text.x = element_text(size = 10, family = 'Calibri', hjust = 1,
                                   face = 'bold', color = '#c6e2ff'),
        axis.text.y = element_text(size = 20, family = 'Gabriola', color = '#f0f8ff'),
        legend.text = element_text(family = 'Gabriola', size = 20, color = '#f0f8ff'),
        legend.title = element_text(family = 'Gabriola', size = 20, color = '#f0f8ff'), 
        plot.title = element_text(size = 26, color = '#c6e2ff', family = 'Colonna MT'),
        plot.subtitle = element_text(size = 16, family = 'Comic Sans MS', face = 'bold', hjust = 0, color = '#ff7373'),
        plot.caption = element_text(size = 10, family = 'Arial', color = '#c6e2ff'))

# Save plot2 ---------------------------------------------------------------------
ggsave(plot = plot2, width = 16, height = 8, dpi = 400, 
       path = './', filename = 'plot2.png', device = 'png')
             
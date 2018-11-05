library(dplyr)
library(stringr)
library(tidyselect)
library(ggplot2)
library(rmarkdown)
library(RColorBrewer)
library(knitr)
library(forcats)
library(readr)
library(tidyr)
library(broom)
library(purrr)

homicide <- read.csv('data/homicide-data.csv')

head(homicide)

homicide <- homicide %>% 
  unite(cityname, city, state, sep = ", ")

unsolved <- homicide %>% 
  select(cityname, disposition) %>% 
  mutate(disposition = factor(disposition)) %>% 
  group_by(cityname) %>% 
  mutate(totalhomicides = n()) %>% 
  mutate(totalunsolved = disposition == 'Closed without arrest'| disposition == 'Open/No arrest') %>% 
  mutate(totalunsolved = sum(totalunsolved)) %>% 
  group_by(cityname, totalunsolved) %>% 
  count() %>% 
  ungroup() %>% 
  rename(totalhomicides = n) 

unsolved

baltimore <- unsolved %>% 
  filter(cityname == 'Baltimore, MD') 

baltimore_prop <- prop.test(x = baltimore$totalunsolved,
                            n = baltimore$totalhomicides)

baltimore_prop <- tidy(baltimore_prop)
baltimore_prop

unsolvedprop <- unsolved %>% 
  mutate(result = map2(totalunsolved, totalhomicides,  ~ prop.test(.x, n = .y))) %>%
  mutate(result = map(result, tidy)) %>% 
  unnest(.drop = TRUE) %>% 
  select(cityname, estimate, conf.low, conf.high) %>% 
  filter(cityname != 'Tulsa, AL') %>% 
  ggplot() +
  geom_point(mapping = aes(x = estimate, y = reorder(cityname, estimate)), 
             color = 'white') +
  geom_errorbarh(mapping = aes(y = cityname, x = estimate, xmin = conf.low, xmax = conf.high), 
                 color = 'white', 
                 height = 0, 
                 alpha = .5) +
  scale_x_continuous(labels = percent, 
                     breaks = c(.2,.3,.4,.5,.6,.7), 
                     limits = c(0.2, 0.8),
                     minor_breaks = c(.2,.3,.4,.5,.6,.7)) +
  labs(x = 'Percent of homicides that are unsolved', y = '') +
  ggtitle("Unsolved homicides by city", 
          subtitle = "Bars show 95% confidence interval") +
  theme_dark() 


unsolvedprop
  

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

homicide <- read.csv('data/homicide-data.csv')

head(homicide)

homicide <- homicide %>% 
  unite(cityname, city, state, sep = ", ")

unsolved <- homicide %>% 
  select(cityname, disposition) %>% 
  mutate(disposition = factor(disposition)) %>% 
  mutate(disposition = fct_recode(disposition,  '1' = 'Closed without arrest', '1' = 'Open/No arrest', '0' = 'Closed by arrest')) %>%
  group_by(cityname) %>% 
  mutate(totalhomicides = n()) %>% 
  mutate(totalunsolved = disposition == '1') %>% 
  mutate(totalunsolved = sum(totalunsolved)) %>% 
  group_by(cityname, totalunsolved) %>% 
  count() %>% 
  ungroup()



  

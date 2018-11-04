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

homicide %>% 
  select(cityname, disposition) %>% 
  mutate(disposition = factor(disposition)) %>% 
  mutate(disposition = fct_recode(disposition,  '1' = 'Closed without arrest', '1' = 'Open/No arrest', '0' = 'Closed by arrest')) %>%
  group_by(cityname) %>% 
  mutate(totalhomicides = n()) %>% 
  mutate(unsolved = disposition == '1') %>% 
  mutate(unsolved = sum(unsolved)) %>% 
  group_by(cityname, unsolved) %>% 
  count() %>% 
  ungroup()



  

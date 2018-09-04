library(tidyverse)
library(lubridate)
#merge()

rtr <- read_rds('Datav2/RTRv2.rds')
calls <- read_rds('Datav2/CAllsv2.rds')
persons <- read_rds('Datav2/personsv2.rds')
mo <- read_rds('Datav2/MOv2.rds')
incid <- read_rds('Datav2/incidentsv2.rds')




colnames(calls) <- clean_col_names(colnames(calls))
colnames(incid) <- clean_col_names(colnames(incid))
colnames(mo) <- clean_col_names(colnames(mo))
colnames(persons) <- clean_col_names(colnames(persons))
colnames(rtr) <- clean_col_names(colnames(rtr))


incid %>% data.frame() %>% select(Year.of.Incident) %>% 
  filter(Year.of.Incident < 2019 & Year.of.Incident > 2013) %>%
  ggplot(aes(Year.of.Incident)) + geom_bar()

incid %>% data.frame() %>% select(Day1.of.the.Week) %>% 
  ggplot(aes(Day1.of.the.Week)) + geom_bar()

incid %>% data.frame() %>% select(Day1.of.the.Year) %>% 
  ggplot(aes(Day1.of.the.Year)) + geom_bar()

incid %>% data.frame() %>% select(Day1.of.the.Year,Year.of.Incident) %>% 
  filter(Year.of.Incident > 2013 & Year.of.Incident < 2018) %>%
  ggplot(aes(Day1.of.the.Year)) + geom_bar()

incid %>% data.frame() %>% select(Day1.of.the.Year,Year.of.Incident) %>% 
  filter(Year.of.Incident==2014) %>%
  ggplot(aes(Day1.of.the.Year)) + geom_bar()

incid %>% data.frame() %>% select(Day1.of.the.Year,Year.of.Incident) %>% 
  filter(Year.of.Incident==2018) %>%
  ggplot(aes(Day1.of.the.Year)) + geom_bar()

incid %>% data.frame() %>% select(Month1.of.Occurence,Year.of.Incident,calcMonth) %>% 
  filter(Year.of.Incident > 2013 & Year.of.Incident < 2018)

incid %>% ggplot(aes(Month1.of.Occurence)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90))


incid %>% data.frame() %>% select(Responding.Officer..1..Badge.No,Year.of.Incident) %>% 
  filter(Year.of.Incident > 2014 & Year.of.Incident < 2018) %>%
  ggplot(aes(Responding.Officer..1..Badge.No)) + geom_bar()



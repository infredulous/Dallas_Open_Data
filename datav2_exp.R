library(tidyverse)
library(lubridate)
#merge()

View(rtr <- read_rds('Datav2/RTRv2.rds'))
View(calls <- read_rds('Datav2/911CAllsv2.rds'))
View(persons <- read_rds('Datav2/personsv2.rds'))
View(mo <- read_rds('Datav2/MOv2.rds'))
View(incid <- read_rds('Datav2/incidentsv2.rds'))


clean_col_names <- function(incols){
  #print(incols)
  #Istevencident 
  #outcols <- list()  
  for (n in incols){
    #t <- unlist(strsplit(n,' '))
    #t <- paste(t,sep = "",collapse = '.')
    t <- gsub("  ",".",n)
    t <- gsub(" ",".",t)
    t <- gsub(".w/year","",t)
    t <- gsub("[#()/]","",t)
    t <- gsub("Istevencident","Incident",t)
    if (length(outcols)==0) outcols <- t
     else outcols <- outcols + t
    # print(t)
  }
  return(outcols)
}
clean_col_names(colnames(incid))



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



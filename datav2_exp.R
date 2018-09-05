library(tidyverse)
library(lubridate)
library(ggplot2)

#rtr <- read_rds('Datav2/RTRv2.rds')
#calls <- read_rds('Datav2/CAllsv2.rds')
#persons <- read_rds('Datav2/personsv2.rds')
#mo <- read_rds('Datav2/MOv2.rds')
incid <- read_rds('Datav2/incidentsv2.rds')

wi <- select(incid,Incident.Number,Year.of.Incident,Year1.of.Occurrence,Month1.of.Occurrence,
             Day1.of.the.Week,Time1.of.Occurrence,Day1.of.the.Year,Call.Received.Date.Time,
             Call.Date.Time,Call.Cleared.Date.Time,Call.Dispatch.Date.Time,Victim.Race,Victim.Ethnicity,
             Victim.Gender,Victim.Age,Victim.Age.at.Offense,Victim.Zip.Code,Responding.Officer.1.Badge.No,
             Responding.Officer.2.Badge.No,X.Coordinate,Y.Cordinate,Zip.Code,xcoord,ycoord,calcMonth)


wg <- wi %>% select(Responding.Officer.1.Badge.No) %>% filter(is.na(Responding.Officer.1.Badge.No)==FALSE) %>% 
    group_by(Responding.Officer.1.Badge.No) %>% mutate(nc=n()) %>% distinct() %>%
    arrange(-nc)
w10 <- wg[1:5,]

w10 %>% ggplot(aes(x=Responding.Officer.1.Badge.No,y=nc)) + geom_col()


wc <- wi %>% arrange(xcoord,ycoord)
wc <- wc %>% filter(xcoord > 30 & xcoord < 35, ycoord < -95 & ycoord > -100)
ggplot(wc,aes(x=xcoord,y=ycoord)) + geom_point(na.rm=TRUE)

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


wg <- wi %>% select(Responding.Officer.1.Badge.No,Year1.of.Occurrence) %>% 
    filter(is.na(Responding.Officer.1.Badge.No)==FALSE & is.na(Year1.of.Occurrence)==FALSE) %>% 
    group_by(Responding.Officer.1.Badge.No,Year1.of.Occurrence) %>% mutate(nc=n()) %>% distinct() %>%
    arrange(-Year1.of.Occurrence,-nc)

idx <- which(wg[[3]] > 150 & wg[[3]]<250)
w10 <- wg[idx,]

w10 %>% ggplot(aes(x=Responding.Officer.1.Badge.No,y=nc)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

w10 %>% ggplot(aes(x=Responding.Officer.1.Badge.No,y=nc)) + 

#top 5
{
w10 <- wg[1:5,]
}
#map incidents by location coords
{
wc <- wi %>% arrange(xcoord,ycoord)
wc <- wc %>% filter(xcoord > 30 & xcoord < 35, ycoord < -95 & ycoord > -100)
ggplot(wc,aes(x=xcoord,y=ycoord)) + geom_point(na.rm=TRUE)
}
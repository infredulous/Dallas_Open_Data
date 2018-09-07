library(tidyverse)
library(lubridate)
library(ggplot2)

#rtr <- read_rds('Datav2/RTRv2.rds')
#calls <- read_rds('Datav2/CAllsv2.rds')
#persons <- read_rds('Datav2/personsv2.rds')
#mo <- read_rds('Datav2/MOv2.rds')
#incid <- read_rds('Datav2/incidentsv2.rds')

wi <- incid %>% select(Incident.Number,Year.of.Incident,Year1.of.Occurrence,Month1.of.Occurrence,
                       Time1.of.Occurrence,Day1.of.the.Year,Call.Received.Date.Time,
                       Call.Date.Time,Call.Cleared.Date.Time,Call.Dispatch.Date.Time,Victim.Race,Victim.Ethnicity,
                       Victim.Gender,Victim.Age,Victim.Age.at.Offense,Victim.Zip.Code,Responding.Officer.1.Badge.No,
                       Responding.Officer.2.Badge.No,X.Coordinate,Y.Cordinate,Zip.Code,xcoord,ycoord,calcMonth)

saveRDS(wi,'Datav2/condensed_incid.rds')

wi <- read_rds('Datav2/condensed_incid.rds')

w2015<- filter(wi,Year.of.Incident==2015,xcoord > 32.6 & xcoord < 33.1,ycoord > -97 & ycoord < -96.4)
w2016<- filter(wi,Year.of.Incident==2016,xcoord > 32.6 & xcoord < 33.1,ycoord > -97 & ycoord < -96.4)
w2017<- filter(wi,Year.of.Incident==2017,xcoord > 32.6 & xcoord < 33.1,ycoord > -97 & ycoord < -96.4)

#w2015 <- mutate(w2015,ycoord=round(ycoord,6),xcoord=round(xcoord,6))
w2016 <- mutate(w2016,ycoord=ycoord+.6,xcoord=xcoord+.6)
w2017 <- mutate(w2017,ycoord=ycoor-.6,xcoord=xcoord-.6)

summarize(w2015,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))
summarize(w2016,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))
summarize(w2017,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))


ggplot() + geom_point(aes(x=w2015$xcoord,y=w2015$ycoord),color='red',na.rm=TRUE,shape='triangle open') + geom_quantile() +
  geom_point(aes(x=w2016$xcoord,y=w2016$ycoord),color='blue',na.rm=TRUE,shape='circle open') + geom_quantile() +
  geom_point(aes(x=w2017$xcoord,y=w2017$ycoord),color='green',na.rm=TRUE,shape='triangle open') + geom_quantile()

ggplot() + geom_point(aes(x=w2015$xcoord,y=w2015$ycoord),na.rm=TRUE,color='red') + geom_quantile() +
  geom_point(aes(x=w2016$xcoord,y=w2016$ycoord),na.rm=TRUE,position = position_dodge(.5),color='yellow') + geom_quantile() +
  geom_point(aes(x=w2017$xcoord,y=w2017$ycoord),na.rm=TRUE,position = position_dodge(.8),color='gray',shape='circle open') + geom_quantile()



wg <- wi %>% select(Responding.Officer.1.Badge.No,Year1.of.Occurrence) %>% 
  filter(is.na(Responding.Officer.1.Badge.No)==FALSE & is.na(Year1.of.Occurrence)==FALSE) %>% 
  group_by(Responding.Officer.1.Badge.No,Year1.of.Occurrence) %>% summarise(nc=n()) %>%
  arrange(Responding.Officer.1.Badge.No,Year1.of.Occurrence,nc)



#top 5
{
  w10 <- wg[1:5,]
  }


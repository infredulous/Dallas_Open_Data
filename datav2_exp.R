library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gghighlight)

#rtr <- read_rds('Datav2/RTRv2.rds')
#calls <- read_rds('Datav2/CAllsv2.rds')
#persons <- read_rds('Datav2/personsv2.rds')
#mo <- read_rds('Datav2/MOv2.rds')
#incid <- read_rds('Datav2/incidentsv2.rds')

#wi <- incid %>% select(Incident.Number,Year.of.Incident,Year1.of.Occurrence,Month1.of.Occurrence,
#                       Time1.of.Occurrence,Day1.of.the.Year,Call.Received.Date.Time,
#                       Call.Date.Time,Call.Cleared.Date.Time,Call.Dispatch.Date.Time,Victim.Race,Victim.Ethnicity,
#                       Victim.Gender,Victim.Age,Victim.Age.at.Offense,Victim.Zip.Code,Responding.Officer.1.Badge.No,
#                       Responding.Officer.2.Badge.No,X.Coordinate,Y.Cordinate,Zip.Code,xcoord,ycoord,calcMonth)

#saveRDS(wi,'Datav2/condensed_incid.rds')

wi <- read_rds('Datav2/condensed_incid.rds')

mxx <- -96.4
mnx <- -97
mxy <- 33.1
mny <- 32.6

w2015<- filter(wi,Year.of.Incident==2015,xcoord > mnx & xcoord < mxx,ycoord > mny & ycoord < mxy)
w2016<- filter(wi,Year.of.Incident==2016,xcoord > mnx & xcoord < mxx,ycoord > mny & ycoord < mxy)
w2017<- filter(wi,Year.of.Incident==2017,xcoord > mnx & xcoord < mxx,ycoord > mny & ycoord < mxy)

#w2015 <- mutate(w2015,ycoord=round(ycoord,6),xcoord=round(xcoord,6))
#w2016 <- mutate(w2016,ycoord=ycoord+.6,xcoord=xcoord+.6)
#w2017 <- mutate(w2017,ycoord=ycoord-.6,xcoord=xcoord-.6)

wi %>% select(.$xcoord==32.776328,.$ycoord==-96.797031)

summarize(w2015,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))
summarize(w2016,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))
summarize(w2017,mxx=max(xcoord),mxy=max(ycoord),mnx=min(xcoord),mny=min(ycoord),medx=median(xcoord),medy=median(ycoord),meanx=mean(xcoord),meany=mean(ycoord),sdx=sd(xcoord),sdy=sd(ycoord))

specialsy <- c(32.776328,32.783960,32.843002,32.738252,32.744253,32.703065,32.832777,32.646958,32.680523,32.717153)
specialsx <- c(-96.797031,-96.783417,-96.796746,-96.888121,-96.726645,-96.716288,-96.723129,-96.908818,-96.867250,-96.963997)
specialsn <- c('City Hall','Deep Ellum','Highland Park','Cockrell Hill','Blair Park','Trinity Greenbelt','Whiterock Lake','Duncanville','Dallas Exec Airport','Mountain Creek Lake')
#,'Cotton Bowl'  ,-96.7595458   ,32.779484



specials <- data.frame(name=specialsn,x=specialsx,y=specialsy)

p1 <- 
  ggplot() + geom_point(aes(x=w2015$xcoord,y=w2015$ycoord),color='red',na.rm=TRUE,shape='triangle open') +
    geom_text(aes(x=specials$x,y=specials$y,label=specials$name),color='blue',na.rm=TRUE)
p2 <- ggplot() + geom_point(aes(x=w2016$xcoord,y=w2016$ycoord),color='blue',na.rm=TRUE,shape='circle open')
p3 <- ggplot() + geom_point(aes(x=w2017$xcoord,y=w2017$ycoord),color='green',na.rm=TRUE,shape='triangle open')

grid.arrange(p1,p2,nrow=1)



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


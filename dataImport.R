#RStudio 1.1.453
#http://www3.dallascityhall.com/publicdata/
#Data since June 1, 2014

library(tidyverse)
library(lubridate)
import::here(get911calls, .from = "import911calls.R")
import::here(getRTR, .from = "importRTR.R")
import::here(getMO, .from = "importMO.R")
import::here(getPersons, .from = "importPersons.R")
import::here(getIncidents, .from = "importIncidents.R")

#variables
{
gollums <- c('Division','Month1 of Occurence','Month2 of Occurence','Day1 of the Week',
             'Day2 of the Week','Offense Entered Month','Offense Entered Day of the Week',
             'Complainant Race','Complainant Gender','Offense Status',
             'Race','Sex','Eyes','Hair','City','State','OffSex','OffRace','CitRace','CitSex',
             'OFF_INJURE','OFF_HOSPIT','CIT_INJURE','CIT_ARREST','Responding Officer #1 Badge No',
             'Responding Officer #2 Badge No','Reporting Officer Badge No','Assisting Officer Badge No',
             'Reviewing Officer Badge No','Person Involvement Type','Victim Type','Victim Race',
             'Victim Gender','Victim Ethnicity')
Mflds <- c('Month1.of.Occurence','Month2.of.Occurence','Offense.Entered.Month')
Dflds <- c('Day1.of.the.Week','Day2.of.the.Week','Offense.Entered.Day.of.the.Week')

dnames <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
dabb <- c('Mon','Tues','Wed','Thur','Fri','Sat','Sun')
mnames <- c('January','February','March','April','May','June','July','August','September','October','November','December')
mabb <- c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec')

nalist <- c("", "NA","N/A","Null","NULL")
}
getcoords <- function(fld){
  tts <- str_split_fixed(str_extract(fld,"\\d{2}\\.\\d{1,8}\\, \\-\\d{2}\\.\\d{1,8}"),', ',n=2)
  llb <- data.frame(xcord=as.numeric(tts[,2]),ycord=as.numeric(tts[,1]))
  return(llb)
}

factorize <- function(tmptmp){
  idx <- which(colnames(tmptmp) %in% gollums)
  cat('Uppercase..')
  for (i in idx) { 
    tmptmp[[i]] <- toupper(tmptmp[[i]])
    cat(i)  
    cat('..')
  }
  cat('\nfactor..months..')
  idx <- which(colnames(data.frame(tmptmp)) %in% Mflds)
  for (i in idx) {
    cat(i)
    cat('..')
    tmptmp[[i]] <- factor(tmptmp[[i]], levels = toupper(mnames))
  }
  cat('\nfactor..days..')
  idx <- which(colnames(data.frame(tmptmp)) %in% Dflds)
  for (i in idx) { 
    cat(i)
    cat('..')
    tmptmp[[i]] <- factor(tmptmp[[i]], levels = toupper(dnames))
  }  
  cat('\n')  
    #lintmp$Month1.of.Occurence <- factor(lintmp$Month1.of.Occurence, levels= unique(lintmp$Month1.of.Occurence[order(lintmp$calcMonth)]))
  return(tmptmp)
}


clean_col_names <- function(incols){
  print('correcting column names..')
  outcols <- c()
  for (n in incols){
    t <- gsub("  ",".",n)
    t <- gsub(" ",".",t)
    t <- gsub(".w/year","",t)
    t <- gsub(".w/.Year","",t)    
    t <- gsub("[#()/]","",t)
    t <- gsub("Istevencident","Incident",t)
    t <- gsub("Occurence","Occurrence",t)

    t <- gsub("IncidentNum","Incident.Number",t)
    t <- gsub("UpzDate","UpDate",t)
    t <- gsub("Y.Cordinate","Y.Coordinate",t)
    outcols <- append(outcols,t)
    # print(t)
  }
  return(outcols)
}

#911Calls
{
# Coordinates are in a different format and present in the data as separate values
calls <- get911calls("Datav1/911_Calls_-_Burglary.csv")
calls <- factorize(calls)
colnames(calls) <- clean_col_names(colnames(calls))
print('saving calls')
saveRDS(calls,"datav2/Callsv2.rds")
}

#RTR Response to Resistance
{
#for some reason the dates and times don't have whitespace as in previous data sets,
#and OCCURRED_T can have "NULL" as a value so converting it to NA
rtr <- getRTR("Datav1/Police_Response_to_Resistance_-_2016.csv")
rtr <- factorize(rtr)
tcoords <- getcoords(rtr$GeoLocation)
rtr <- mutate(rtr,xcoord=tcoords$xcord,ycoord=tcoords$ycord)
colnames(rtr) <- clean_col_names(colnames(rtr))
print('saving RTR..')
saveRDS(rtr,"datav2/RTRv2.rds")
}

#Persons
{
# there is no location data or factored data
persons <- getPersons("Datav1/Police_Person.csv")
persons <- factorize(persons)
colnames(persons) <- clean_col_names(colnames(persons))
print('saving persons..')
saveRDS(persons,"datav2/personsv2.rds")
}

#MO
{
#no coordinates present
mo <- getMO("Datav1/Police_MO.csv")
mo <- factorize(mo)
colnames(mo) <- clean_col_names(colnames(mo))
print('saving MO..')
saveRDS(mo, "datav2/MOv2.rds")
}

#Incidents
{
#some of the Year Fields have values that are most likely inncorrect,
#I'm leaving them because I don't think they have a large influence
#I'm skipping the name, address and phone numbers for privacy reasons
incid <- getIncidents("Datav1/Police_Incidents.csv")
incid <- factorize(incid)
tcoords <- getcoords(incid$Location1)
incid <- mutate(incid,xcoord=tcoords$xcord,ycoord=tcoords$ycord)
incid <- incid %>% mutate(calcMonth=month(.$'Date1 of Occurrence',label = FALSE))
colnames(incid) <- clean_col_names(colnames(incid))
print('saving incidents..')
saveRDS(incid,"datav2/Incidentsv2.rds")
}

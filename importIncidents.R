#library(readr)

getIncidents <- function(fpath){
print('importing incidents...')
intmp <- read_csv(fpath, trim_ws=TRUE, na = nalist,
                  col_types = cols(`Apartment Number` = col_skip(), 
                                   `Call Cleared Date Time` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Call Date Time` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Call Dispatch Date Time` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Call Received Date Time` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Date incident created` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Date of Report` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   `Date1 of Occurrence` = col_date(format = " %m/%d/%Y "), 
                                   `Date2 of Occurrence` = col_date(format = " %m/%d/%Y "), 
                                   `Family Offense` = col_logical(), 
                                   `Offense Entered  Date/Time` = col_character(), 
                                   `Offense Entered Time` = col_time(format = " %H:%M "), 
                                   `Responding Officer #1  Name` = col_skip(), 
                                   `Responding Officer #2  Name` = col_skip(),
                                   `Responding Officer #1  Badge No` = col_character(), 
                                   `Responding Officer #2 Badge No` = col_character(),
                                   `Victim Package` = col_skip(),
                                   `Time1 of Occurrence` = col_time(format = " %H:%M "), 
                                   `Time2 of Occurrence` = col_time(format = " %H:%M "), 
                                   `Update Date` = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "),
                                   'Victim Name' = col_skip(),
                                   `Victim Apartment` = col_skip(), 
                                   `Victim Business Address` = col_skip(), 
                                   `Victim Business Name` = col_skip(), 
                                   `Victim Business Phone` = col_skip(), 
                                   `Victim Home Address` = col_skip(), 
                                   `Victim Zip Code` = col_character(),
                                   'Incident Address' = col_skip(),
                                   `Zip Code` = col_character()))

 return(intmp)
}
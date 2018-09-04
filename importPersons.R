#library(readr)

getPersons <- function(fpath){
  print('importing Persons...') 
intmp <- read_csv(fpath, trim_ws=TRUE, na = nalist, 
                  col_types = cols(Address = col_skip(), 
                                   AptNum = col_skip(), DRLicNum = col_skip(), 
                                   EDate = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "), 
                                   FirstName = col_skip(), LastName = col_skip(), 
                                   MiddleName = col_skip(), Name = col_skip(), 
                                   UpDatedOn = col_datetime(format = " %m/%d/%Y %H:%M:%S %p "),
                                   WeaponMake = col_skip(),
                                   WeaponModel = col_skip(),
                                   'ServNum' = col_skip(),
                                   'ServiceNum' = col_skip(),
                                   'ETime1' = col_skip(),
                                   'ETime2' = col_skip(),
                                   'DRLicSt' = col_skip()
                  ))
 return(intmp)
}

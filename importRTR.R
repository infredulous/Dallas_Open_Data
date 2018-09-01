#library(readr)

getRTR <- function(fpath){

intmp <- read_csv(fpath,
                  trim_ws=TRUE,na=nalist,
                  col_types = cols(HIRE_DT = col_date(format = "%m/%d/%Y"), 
                                   OCCURRED_D = col_date(format = "%m/%d/%Y"), 
                                   OCCURRED_T = col_time(format = "%H:%M:%S %p"),
                                   'Y' = col_skip(),
                                   'X' = col_skip()))


return(intmp)
}
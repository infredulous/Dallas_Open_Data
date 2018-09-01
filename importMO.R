#library(readr)

getMO <- function(fpath){
  
intmp <- read_csv(fpath,trim_ws=TRUE, na = nalist,
                  col_types = cols(UpzDate = col_datetime(format = "%m/%d/%Y %H:%M:%S %p "),
                                   'servnum'= col_skip(),
                                   'servicenum' = col_skip()
                  ))
 return(intmp)
}

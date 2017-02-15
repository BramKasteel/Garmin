#Script that connects to SQL db

library(RMySQL)

CreateSQLConnection <- function(user='root',host='localhost',...){
  
  
  db <- dbConnect(MySQL(),user=user, host=host, password = 'b100=tof')
  return(db)
}
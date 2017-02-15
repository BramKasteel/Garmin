#Script that writes new activity to SQL db

initialization <- T

if (initialization){
  library(fit)
  
  path <- "FIT/"
  
  fileDetails = file.info(list.files(path,full.names=T))
  idx <- fileDetails$ctime > Sys.time()-31*24*60*60
  filenamesPath <- rownames(fileDetails)[idx]
  filenamesOrig <- substr(filenamesPath,start=5,stop=10000000L)
  fitFiles <- lapply(filenamesPath, read.fit)
  for(x in 1:length(fitFiles)){
    fitFiles[[x]]$fileName <- filenamesOrig[x] 
  }
  
  source('dataIO/CreateSQLConnection.R')
  db <- CreateSQLConnection()
  
  for (i in 1:length(fitFiles)){
    print(i)
    WriteActivityDB(db, fitFiles[[i]])
  }
}

WriteActivityDB <- function(db, fitFile, user="Bram", actTable='garmin.activity', recTable='garmin.record'){
  library(reshape2)
  
  #Check whether file is new:
  savedFiles <- unlist(dbGetQuery(db, paste("SELECT id FROM",actTable)))
  if (fitFile$fileName %in% savedFiles){
    print('warning: fitFile already saved')
    return(0)
  } else {
    #Step 1: see what info we have in the session.
    #Save as T/F in table activity
    colNm <- names(fitFile$record)
    colDB_compl <- colnames(dbGetQuery(db, paste("SELECT * FROM", actTable, "LIMIT 1")))
    colDB_TF <- setdiff(colDB_compl,c('id','user','created','trainType'))
    
    query <- paste("INSERT INTO", actTable,"(",paste(colDB_compl,collapse=','),")")
    query <- paste0(query," VALUES (",
                    "'",fitFile$fileName,"', ",
                    "'",user,"', ",
                    "'",as.POSIXct(fitFile$file_id$time_created,origin='1989-12-31 23:59:59',tz='UTC'),"', ",
                    paste(as.numeric(colDB_TF %in% colNm),collapse=","),",NULL)")
    
    dbGetQuery(db,query)
    
    #Step 2: Save the recorded activiy
    fitFile$record$timestamp <- fitFile$record$timestamp - fitFile$record$timestamp[1]
    recordMelt <- melt(fitFile$record, id.vars = 'timestamp')
    recordMelt <- data.frame(id=fitFile$fileName,recordMelt)
    recordMelt <- recordMelt[complete.cases(recordMelt),]
    #Inefficient, but works. Row-by-row:
    fileName <- fitFile$fileName
    
    values <- apply(recordMelt,MARGIN=1,FUN = function(x) {
      paste0("('",x[1],"', ",x[2],", '",x[3],"', ",x[4],")")
    })
    values <- paste(values,collapse=',')
    
    #dbGetQuery(db,paste0("INSERT INTO ",recTable," VALUES ('",fileName,"', '",recordMelt[i,2],"', ",recordMelt[i,3],")"))
    dbGetQuery(db,paste0("INSERT INTO ",recTable," VALUES ", values))
  }
  
}


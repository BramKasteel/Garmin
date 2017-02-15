#Some SQL commands that are too long to type in self

library(RMySQL)
source(file='dataIO/CreateSQLConnection.R')

db <- CreateSQLConnection()



paste(paste0("`",colNm,"`"),"BIT(1) NOT NULL,",collapse=' ')

query <- paste0("CREATE TABLE `garmin`.`activity` (`id` VARCHAR(45) NOT NULL,`user` VARCHAR(45) NOT NULL,`created` DATETIME(6) NULL,",paste(paste0("`",colNm,"`"),"BIT(1) NOT NULL,",collapse=' '),"PRIMARY KEY (`id`),UNIQUE INDEX `id_UNIQUE` (`id` ASC))")

dbGetQuery(db,query)

CREATE TABLE `garmin`.`record` (
  `id` VARCHAR(45) NOT NULL,
  `timestamp` INT(6) NULL,
  `variable` VARCHAR(45) NULL,
  `value` FLOAT NULL);
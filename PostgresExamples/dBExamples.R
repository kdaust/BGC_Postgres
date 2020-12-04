###some examples of how to connect to PostgreSQL from R
##Kiri Daust

library(data.table)
library(sf)
library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(foreach)

drv <- dbDriver("PostgreSQL")
##to connect to Will's server
con <- dbConnect(drv, user = "postgres", host = "smithersresearch.ca",
                 password = "Kiriliny41", port = 5432, dbname = "cciss_data") 
##connect to local postgres instance
con <- dbConnect(drv, user = "postgres", host = "localhost",
                 password = "Kiriliny41", port = 5432, dbname = "cciss_data") 

##there are a three main ways to retrieve data: you can send direct SQL queries with RPostgreSQL,
##you can use dplyr with dbplyr to pull data using tidyverse syntax, or you can query spatial data using sf

##Example 1: Pull data using SQL query
temp <- dbGetQuery(con,"SELECT DISTINCT siteno, tileno FROM id_atts WHERE dist_code = 'DND'")

##Example 2: Pull data using dbplyr
src_dbi(con) ##show available tables
dat_fut <- tbl(con,"cciss_400m")##create pointers to each table you want to use
dat_norm <- tbl(con, "cciss_normal")
dat_att <- tbl(con,"id_atts")

temp <- dat_att %>%
  filter(dist_code == "DND") %>%
  select(siteno,tileno) %>%
  distinct() %>%
  collect() ##have to call collect at end, otherwise only returns first 10 rows

##Example 3: Using sf
test <- st_read(con, query = "SELECT * FROM norm_comb_sf WHERE dist_code = 'DSC' AND period = 'Current91'")

###if you want to execute SQL queries that don't return results, use dbExecute
##E.g. you can create new tables:
dbExecute(con, paste0("CREATE TABLE colin_normal AS SELECT * FROM cciss_normal WHERE siteno IN (",
                      paste(colinPnts, collapse = ","),")"))
dbExecute(con, paste0("CREATE TABLE colin_future AS SELECT * FROM cciss_400m WHERE siteno IN (",
                      paste(colinPnts, collapse = ","),") AND scenario = 'rcp45' AND futureperiod IN ('2025','2055')"))
###or update values:
dbExecute(con,"UPDATE colin_normal SET var = 'Mean' WHERE var = 'mean'")

##Writing to Postgres from R:
##IMPORTANT!!! Postgresql doesn't accept captial letters or certain symbols in column names
##and it gets very awkward if you try to send bad column names. I use the following function to create safe names

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

dat <- iris
dat <- as.data.table(dat)
setnames(dat, dbSafeNames(names(dat)))

##now write to db
dbCreateTable(con, name = "test_table", fields = dat)
##can append to any table using dbAppendTable(con, name = "test_table", value = dat)

##good practice to disconnect when you're done
dbDisconnect(con)

#############################################333


###More misc examples
### user selected parameters
mod.opts <- tbl(con,"models") %>% collect()
period <- select.list(c("Normal61","Current91", "2025","2055","2085"), multiple = T, graphics = T)

##pull data using parameters
## future periods
if(any(period %in% c("2025","2055","2085"))){
  model <- select.list(mod.opts$gcm, multiple = T, graphics = T)
  scn <- select.list(c("rcp45","rcp85"), multiple = T, graphics = T)
  dat <- dat_fut %>%
    filter(siteno %in% colinPnts, gcm %in% model, scenario %in% scn, futureperiod %in% period) %>%
    select(gcm,scenario,futureperiod, siteno,bgc,bgc_pred) %>%
    collect()
  datFut <- as.data.table(dat)
}

##normal/current
if(any(period %in% c("Normal61","Current91"))){
  per2 <- period
  dat <- dat_norm %>%
    filter(siteno %in% colinPnts, period %in% per2) %>%
    select(period,siteno,bgc,bgc_pred) %>%
    collect()
  datCurr <- as.data.table(dat)
}

### now for local database, assuming you imported the sql dump...
src_dbi(con)
dat_fut <- tbl(con,"colin_future") ## predicted BGC
dat_norm <- tbl(con, "colin_normal")

##same sort of idea, e.g.
per2 <- "Current91"
dat <- dat_norm %>%
  filter(period %in% per2) %>%
  select(period,siteno,bgc,bgc_pred) %>%
  collect()

##or
dat <- dat_fut %>%
  filter(futureperiod == '2025') %>%
  select(gcm,scenario,futureperiod, siteno,bgc,bgc_pred) %>%
  collect()



##now if you want to make a map...
##would have to adjust model, scn, and period depending on what you're using
grd <- as.data.table(grd)
grd <- st_sf(grd)
setnames(grd, c("siteno","geom"))

if(exists("datFut")){
  outname <- paste0("Future_",outputName)
  foreach(mod = model, .combine = c) %:%
    foreach(s = scn, .combine = c) %:%
    foreach(per = period, .combine = c,.packages = c("sf","data.table")) %do% {
      sub <- dat[gcm == mod & scenario == s & futureperiod == per,]
      map <- merge(grd,sub, by = "siteno", all = F)
      
      mapComb <- aggregate(map[,"geometry"],  by = list(map$bgc_pred), FUN = mean)
      colnames(mapComb)[1] <- "BGC"
      st_write(mapComb, dsn = outputName, layer = paste(RegSelect,mod,s,per,sep = "_"), append = T)
      TRUE
    }
}

if(exists("datCurr")){
  outname <- paste0("Current_",outputName)
  foreach(per = period, .combine = c,.packages = c("sf","data.table")) %do% {
    sub <- dat[period == per,]
    map <- merge(grd,sub, by = "siteno", all = F)
    
    mapComb <- aggregate(map[,"geometry"],  by = list(map$bgc_pred), FUN = mean)
    colnames(mapComb)[1] <- "BGC"
    st_write(mapComb, dsn = outputName, layer = paste(RegSelect,per,sep = "_"), append = T)
    TRUE
  }
}



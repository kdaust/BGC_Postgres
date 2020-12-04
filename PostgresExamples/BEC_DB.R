## extract data from postgres, and merge with hex grid to plot
## Kiri Daust 2020
library(data.table)
library(sf)
library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(foreach)

##connect to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", host = "192.168.1.64",password = "Kiriliny41", port = 5432, dbname = "cciss_data") ### for local use
con <- dbConnect(drv, user = "postgres", host = "localhost",password = "Kiriliny41", port = 5432, dbname = "cciss_data") ## for local machine
con <- dbConnect(drv, user = "postgres", host = "smithersresearch.ca",password = "Kiriliny41", port = 5432, dbname = "cciss_data") ### for external use

##pull tables from database
src_dbi(con)
allDat <- tbl(con,"dat_comb") ## joins of the next two tables
dat <- tbl(con,"cciss_400m") ## predicted BGC
att <- tbl(con, "id_atts")##region district ID
dat_norm <- tbl(con, "dat_comb_normal")

### user selected parameters

reg.opts <- tbl(con,"regions") %>% collect()
mod.opts <- tbl(con,"models") %>% collect()
RegSelect <- select.list(reg.opts$reg_code, multiple = T, graphics = T)
period <- select.list(c("Normal61","Current91", "2025","2055","2085"), multiple = T, graphics = T)
outputName <- "MapTest91.gpkg"

##pull data using parameters

if(any(period %in% c("2025","2055","2085"))){
  model <- select.list(mod.opts$gcm, multiple = T, graphics = T)
  scn <- select.list(c("rcp45","rcp85"), multiple = T, graphics = T)
  dat <- allDat %>%
    filter(reg_code %in% RegSelect, gcm %in% model, scenario %in% scn, futureperiod %in% period) %>%
    select(reg_code,gcm,scenario,futureperiod, siteno,bgc,bgc_pred) %>%
    collect()
  datFut <- as.data.table(dat)
}
if(any(period %in% c("Normal61","Current91"))){
  per2 <- period
  dat <- dat_norm %>%
    filter(reg_code %in% RegSelect, period %in% per2) %>%
    dplyr::select(reg_code,period, siteno,bgc,bgc_pred) %>%
    collect()
  datCurr <- as.data.table(dat)
}

grd <- st_read("D:/CommonTables/HexGrids/HexGrd400.gpkg")

grd <- st_read(dsn = "/media/kiridaust/MrBig/BCGrid/HexGrd400.gpkg") ## base 400m hexgrid of province
#grd <- st_read("C:/Users/whmacken/Sync/CCISS_data/BC_400mbase_hexgrid/HexGrd400.gpkg")

grd <- as.data.table(grd)
grd <- st_sf(grd)
setnames(grd, c("siteno","geom"))

# require(doParallel)
# cl <- makePSOCKcluster(detectCores()-2)
# registerDoParallel(cl)

### this section aggregates by BGC to reduce the size of the resultant layers
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

##example despeckling
library(fasterize)
map <- as.data.table(mapComb) %>% st_as_sf()
##have to convert BGC labels to ints
legend <- data.table(BGC = unique(map$BGC))
legend[,Value := seq_along(BGC)]
map <- legend[map, on = "BGC"]
map <- st_as_sf(map) %>% st_cast("MULTIPOLYGON")
rast <- raster(map, resolution = 200) ##covert to raster with twice the resolution
rast <- fasterize(map,rast, field = "Value")

tMat <- as.matrix(values(rast))
Rcpp::sourceCpp("_RasterDespeckle.cpp")
tMat <- pem_focal(tMat, wrow = 7, wcol = 7) ##7*7 focal window to finding mode
tMat[tMat < 0] <- NA
values(rast) <- tMat
writeRaster(rast, "TestBGCRaster.tif",format = "GTiff")

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





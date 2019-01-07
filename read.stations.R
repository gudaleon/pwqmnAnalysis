# Reading stations file
### CAN'T FIND CONSERVATION AUTHORITIES NAMES
#stations <- raster::shapefile("PWQMN1/PWQMN_Stations.shp")
stations <- fread("PWQMN_CA.txt")
#stations <- foreign::read.dbf("PWQMN_CAt.dbf", as.is=TRUE)
# fixing station names
setDT(stations)
setnames(stations, tolower(names(stations)))
if (!is.character(stations$station)) {
  setnames(stations, "station", "station0")
  stations[,station:=bit64::as.character.integer64(station0)]
}
# any missing zeros?
stations[,station:=gsub(" ", "0", format(station, width=11, justify = "right"))]
# confirm: unique(stations[station %like% "^0", .(station0,station)])



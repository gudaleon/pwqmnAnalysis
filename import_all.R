# Variables to summarize : 
# - Phosphorus: PPUT (total phosphorus),  PPO4FR (phosphate, filtered reactive)
# - Nitrogen: NTOT, NNTKUR, NNHTUR, NNO2UR, NNOTUR
# - residue/TSS: RSP, RSF, RST

# Time period 2 - 1964-1999. 
# Time period1- 2000-now.
# This periods should be analyzed separately due to less reliable protocol of analysis for phosphorus. 

source("read.stations.R")
source("read.1964.R")
source("read.2000.R")

keyvars <- c("PPUT", "PPO4FR", 
             "NTOT", "NNTKUR", "NNHTUR", "NNO2UR", "NNOTUR", 
             "RSP", "RSF", "RST")

keyareas <- c("Essex Region Conservation Authority", 
              "Lower Thames Valey Conservation Authority", 
              "Kettle Creek Conservation Authority", 
              "Catfish Creek Conserservation Authority", 
              "Long Point Region Conservation Authority", 
              "St. Clair Region Conservation Authority", 
              "Upper Thames River Conservation Authority", 
              "Grand River Conservation Authority")

keyyears <- list(Dry=c(1989,1994,1999,2002,2003,2007,2015),
                 Wet=c(1996,2008,2009,2013,2014))
keyyears <- rbindlist(lapply(names(keyyears), function(x) data.table(type=x, year=keyyears[[x]])))

# Checking variable names
vars_absent_db1964 <- setdiff(keyvars, db1964$pwqmn_1964_1999$param.code)
if (length(vars_absent_db1964)) {
  message(sprintf("\nVariables absent from the database: %s\n", 
                  paste0(vars_absent_db1964, collapse=", ")))
}
vars_absent_db2000 <- setdiff(keyvars, db2000$pwqmn_2000_rawdata$param.code)
if (length(vars_absent_db2000)) {
  message(sprintf("\nVariables absent from the database: %s\n", 
                  paste0(vars_absent_db2000, collapse=", ")))
}

# Organizing the data
db0 <- rbind(cbind(dataset="1964",
                  db1964$pwqmn_1964_1999[,.(date,time,time0,param.code,station,value,
                                            value.qualifier,field.no)]),
            cbind(dataset="2000",
                  db2000$pwqmn_2000_rawdata[,.(date,time,time0,param.code,station,value,
                                               value.qualifier,field.no)]))
db0[,datetime:=ifelse(is.na(time), as.POSIXct(date),as.POSIXct(date)+time+5*60*60)]
db0[,year:=year(date)]
setkey(db0, param.code, station, year, datetime)

# Filtering data into two tables
db <- keyyears[db0, on="year"][          # linking data to years, and stations
  stations[,.(station,longitude,latitude,CA=name_1)][CA %in% keyareas], on="station"][
          param.code %in% keyvars]       # and filtering by stations and key variables
db[is.na(type), type:="Normal"]

# grouping, counting, distance
db[,by=.(dataset,param.code,station,year=year(date)),
   period:=datetime-c(NA,head(datetime,-1))]

db_stats <- db[,by=.(dataset,param.code,CA,station,longitude,latitude,
                     type,year=year(date),month=month(date)),
               .(nrows=.N, ndays=length(unique(date)),
                 firstSample=min(yday(date)),lastSample=max(yday(date)),
                 period_mean=as.numeric(mean(period, na.rm=TRUE))/60/60,
                 period_sd=as.numeric(sd(period, na.rm=TRUE))/60/60,
                 value_mean=mean(value), value_sd=mean(value))]

db_synt <- db_stats[,keyby=.(param.code,CA,dataset),
                    .(stations=length(unique(station)), 
                      yearsTotal=length(unique(year)),
                      yearsDry=length(unique(year[type=="Dry"])),
                      yearsWet=length(unique(year[type=="Wet"])),
                      FirstYear=min(year),LastYear=max(year),
                      ndays=sum(ndays),nsamples=sum(nrows),
                      firstSample=mean(.SD[,.(m=min(firstSample)),by=.(year)][,m]),
                      lastSample=mean(.SD[,by=year,.(m=max(lastSample))][,m]),
                      period_mean=mean(na.omit(period_mean))/24,
                      #period_sd=mean(na.omit(period_sd))/24  --- it may be useful, but have to be estimated in a different way
                      value_mean=mean(value_mean), value_sd=sd(value_mean))]
# Exporting
write.csv(db_stats, "db_stats.csv")
write.csv(db_synt, "db_synt.csv")
zip("db_stats.csv.zip", "db_stats.csv")





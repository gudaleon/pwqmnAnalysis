# Time period1- 2000-now.

# Importing the databases
db2000o <- Hmisc::mdb.get("pwqmn_rawdata_2000_2016/pwqmn_raw_data_2000_2016.mdb", 
                         colClasses = "character", stringsAsFactors=FALSE)


# Into data.table
invisible(lapply(db2000o, setDT))

# lower case and replace - with _.
names(db2000o) <- gsub("-", "_", tolower(names(db2000o)))
invisible(lapply(db2000o, function(x) setnames(x, gsub("-", "_", tolower(names(x))))))

# exploring the db
names(db2000o)
db_cols <- sapply(grep("pwqmn_[[:digit:]]{4}_rawdata", names(db2000o), value=TRUE), 
                  function(x) names(db2000o[[x]]), simplify=FALSE)
unique(unlist(db_cols))
invisible(lapply(grep("pwqmn_[[:digit:]]{4}_rawdata", names(db2000o), value=TRUE),
                 function(x) 
                   if ("valuqualifi" %in% names(db2000o[[x]])) 
                     setnames(db2000o[[x]],"valuqualifi","valqualifi")))
# Now into 1964 format
db2000 <- sapply(grep("pwqmn_[[:digit:]]{4}_rawdata", names(db2000o), 
                      value=TRUE, invert=TRUE), 
                 function(x) copy(db2000o[[x]]), simplify=FALSE)
db2000$pwqmn_2000_rawdata <- rbindlist(db2000o[grep("pwqmn_[[:digit:]]{4}_rawdata", 
                                                    names(db2000o), value=TRUE)], 
                                       fill=TRUE)

# Checking station codes
if (nrow(db2000$pwqmn_2000_rawdata[!stations, on="station"])) {
  message(nrow(db2000$pwqmn_2000_rawdata[!stations, on="station"]), 
          " records without stations.")
  message("Stations:")
  print(unique(db2000$pwqmn_2000_rawdata[!stations, on="station",station]))
}

# Standarizing names with db1964
names(db1964$pwqmn_1964_1999)
names(db2000$pwqmn_2000_rawdata)
setnames(db2000$pwqmn_2000_rawdata,
         c("parm", "result", "parm.description","valqualifi"), 
         c("param.code","result.value", "parameter","value.qualifier"))

# Preparing to fix values
setnames(db2000$pwqmn_2000_rawdata,c("date", "time"), c("date0","time0"))

# Transforming date
db2000$pwqmn_2000_rawdata[, date := as.Date(gsub("^([[:digit:]]{2})/([[:digit:]]{2})/([[:digit:]]{2}) 00:00:00$",
                                              "20\\3-\\1-\\2", date0), format="%Y-%m-%d")]
# If no NA values, all worked well
summary(db2000$pwqmn_2000_rawdata$date)

# Not sure what is the time format
summary(as.numeric(db2000$pwqmn_2000_rawdata$time0))
# Assuming 13:45 ==> 1345
db2000$pwqmn_2000_rawdata[, time1 := ifelse(grepl("e+", time0), 
                                            as.character(as.integer(time0)),
                                            time0)]
db2000$pwqmn_2000_rawdata[, time := as.ITime(ifelse(nchar(time1)<=2, paste0("00:",time1),
                                                    gsub("([[:digit:]]{2})$", ":\\1", time1)))]

# If no NA values, all worked well (Expected range goes from 0 - (24*60*60-1))
summary(db2000$pwqmn_2000_rawdata[,time])

# Importing the result
db2000$pwqmn_2000_rawdata[result.value=="",result.value:=NA]
db2000$pwqmn_2000_rawdata[,value:=as.numeric(result.value)]
# any NA value after or before the transformation?
if (nrow(db2000$pwqmn_2000_rawdata[is.na(value) & !is.na(result.value)])) {
  message("\nError importing values. Review the table.\n")
}

# Reviewing the remarks
## Issues with value.qualifier:
message("Issues with value.qualifier?:")
print(unique(db2000$pwqmn_2000_rawdata[value.qualifier != "",
                                    .(value.qualifier, remark.code,
                                      value.qualifier.code=gsub("^(\\+/-).*$","\\1",value.qualifier))][
                                        !db2000$remark_valqualif_codes_en,
                                        on=c(value.qualifier.code="lims.valqualif")]))

## Issues with remark.code:
message("Issues with remark.code?:")
print(unique(db2000$pwqmn_2000_rawdata[remark.code != "",
                                    .(value.qualifier, remark.code,
                                      remark.code.code=gsub("^(\\+/-).*$","\\1",remark.code))][
                                        !db2000$remark_valqualif_codes_en,
                                        on=c(remark.code.code="lims.valqualif")]))


rm(db2000o)

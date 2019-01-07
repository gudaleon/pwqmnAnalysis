# Time period 2 - 1964-1999. 

# library(Hmisc) ==> Only one function is used so far.
library(data.table)

# Importing the databases
db1964 <- Hmisc::mdb.get("pwqmn_rawdata_1964_1999/PWQMN_historical_1964_1999.mdb", 
                         colClasses = "character", stringsAsFactors=FALSE)
# Into data.table
invisible(lapply(db1964, setDT))

# lower case and replace - with _.
names(db1964) <- gsub("-", "_", tolower(names(db1964)))
invisible(lapply(db1964, function(x) setnames(x, gsub("-", "_", tolower(names(x))))))

# Exploring the database
lapply(db1964, names)
db1964$pwqmn_1964_1999[1:5]

# Transforming variables
summary(db1964$pwqmn_1964_1999$date)
# db1964$pwqmn_1964_1999$date[1]
# format::: 01/21/65 00:00:00

# is there anyone with something else than 00:00:00
if (nrow(db1964$pwqmn_1964_1999[!(date %like% 
                                  "[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{2} 00:00:00")])) {
  stop("Review date format!")
}

# Checking station codes
db1964$pwqmn_1964_1999[stations, on="station"]


# Preparing to fix values
setnames(db1964$pwqmn_1964_1999,c("date", "time"), c("date0","time0"))

# Transforming date
db1964$pwqmn_1964_1999[, date := as.Date(gsub("^([[:digit:]]{2})/([[:digit:]]{2})/([[:digit:]]{2}) 00:00:00$",
                                              "19\\3-\\1-\\2", date0), format="%Y-%m-%d")]
summary(db1964$pwqmn_1964_1999$date)

# Not sure what is the time format
summary(as.numeric(db1964$pwqmn_1964_1999$time))

# Assuming 13:45 ==> 1345
db1964$pwqmn_1964_1999[, time1 := ifelse(grepl("e+", time0), 
                                            as.character(as.integer(time0)),
                                            time0)]
db1964$pwqmn_1964_1999[, time := as.ITime(ifelse(nchar(time1)<=2, paste0("00:",time1),
                                                 gsub("([[:digit:]]{2})$", ":\\1", time1)))]

# If no NA values, all worked well (Expected range goes from 0 - (24*60*60-1))
summary(db1964$pwqmn_1964_1999[,time])

# Importing the result
db1964$pwqmn_1964_1999[,value:=as.numeric(result.value)]
# any NA value after or before the transformation?
if (nrow(db1964$pwqmn_1964_1999[is.na(value) | is.na(result.value)])) {
  message("\nError importing values. Review the table.\n")
}

# Reviewing the remarks
## Issues with value.qualifier:
message("Issues with value.qualifier:")
print(unique(db1964$pwqmn_1964_1999[value.qualifier != "",
                       .(value.qualifier, remark.code1, remark.code,
                         value.qualifier.code=gsub("^(\\+/-).*$","\\1",value.qualifier))][
  !db1964$remark_valqualif_code_en,on=c(value.qualifier.code="remark.valqualif")]))

## Issues with remark.code1:
message("Issues with remark.code1:")
print(unique(db1964$pwqmn_1964_1999[remark.code1 != "",
                              .(value.qualifier, remark.code1, remark.code,
                                remark.code1.code=gsub("^(\\+/-).*$","\\1",remark.code1))][
                                  !db1964$remark_valqualif_code_en,on=c(remark.code1.code="remark.valqualif")]))

## Issues with remark.code:
message("Issues with remark.code:")
print(unique(db1964$pwqmn_1964_1999[remark.code != "",
                              .(value.qualifier, remark.code1, remark.code,
                                remark.code.code=gsub("^(\\+/-).*$","\\1",remark.code))][
                                  !db1964$remark_valqualif_code_en,on=c(remark.code.code="remark.valqualif")]))


library(RecordLinkage)
library(plyr)
library(reshape2)
library(xlsx)

#read in timelog using base function
setwd("C:/R/workspace/shared")
source("import_functions.R")
timelog <- import_timelog()
setwd("C:/R/workspace/timelog/weekly_time/source")
daily_hours <- read.csv("daily_logs.csv", header = T, stringsAsFactors = F)
daily_hours$Date <- as.Date(daily_hours$Date, format = "%m/%d/%Y")
#remove pre Q2 2013 hours
timelog <- timelog[timelog$Date >= "2013-06-30",]
daily_hours <- daily_hours[daily_hours$Date >= "2013-06-30",]

#get role dates
setwd("C:/R/workspace/source")
role_dates <- read.csv("ps_start_dates.csv", header = T, stringsAsFactors = F)
role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))] <- 
  lapply(role_dates[,!(colnames(role_dates) %in% (c("Full.Name")))],FUN = as.Date, format = "%m/%d/%Y")

#set all time to PSM when the title says psm or sr psm
timelog$role <- NA
daily_hours$role <- NA
timelog[timelog$User.Title %in% unique(timelog$User.Title)[grep("Professional", unique(timelog$User.Title))],]$role <- "PSM"
daily_hours$role <- "PSM"

# time_started <- proc.time()
#for those with a start date, set time before to NA
for (i in 1:length(role_dates[!is.na(role_dates$Start.Date),]$Full.Name)){
  psm <- role_dates[!is.na(role_dates$Start.Date),]$Full.Name[i]
  start <- role_dates[!is.na(role_dates$Start.Date),]$Start.Date[i]
  if(length(timelog[timelog$User %in% psm & timelog$Date < start, ]$role) > 0){
    timelog[timelog$User %in% psm & timelog$Date < start, ]$role <- NA
  }
}

#for psms promoted to senior, set time forward to Sr PSM
for (i in 1:length(role_dates[!is.na(role_dates$to_senior),]$Full.Name)){
  psm <- role_dates[!is.na(role_dates$to_senior),]$Full.Name[i]
  promotion_date <- role_dates[!is.na(role_dates$to_senior),]$to_senior[i]
  if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0){
    timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "Sr PSM"
  }
  if(length(daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role) > 0){
    daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role <- "Sr PSM"
  }
}

#for srs promoted to tms, set time forward to TM
for (i in 1:length(role_dates[!is.na(role_dates$to_tm),]$Full.Name)){
  psm <- role_dates[!is.na(role_dates$to_tm),]$Full.Name[i]
  promotion_date <- role_dates[!is.na(role_dates$to_tm),]$to_tm[i]
  if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0 ){
    timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "TM"
  }
  if(length(daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role) > 0){
    daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role <- "TM"
  }
}

#for tms promoted to director, set time forward to director
for (i in 1:length(role_dates[!is.na(role_dates$to_director),]$Full.Name)){
  psm <- role_dates[!is.na(role_dates$to_director),]$Full.Name[i]
  promotion_date <- role_dates[!is.na(role_dates$to_director),]$to_director[i]
  if(length(timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role) > 0){
    timelog[timelog$User %in% psm & timelog$Date >= promotion_date, ]$role <- "Director"
  }
  if(length(daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role) > 0){
    daily_hours[daily_hours$User %in% psm & daily_hours$Date >= promotion_date, ]$role <- "Director"
  }
}

#for psms who left PS, set time forward to NA
for (i in 1:length(role_dates[!is.na(role_dates$End.Date),]$Full.Name)){
  psm <- role_dates[!is.na(role_dates$End.Date),]$Full.Name[i]
  term_date <- role_dates[!is.na(role_dates$End.Date),]$End.Date[i]
  if(length(timelog[timelog$User %in% psm & timelog$Date >= term_date, ]$role) > 0){
    timelog[timelog$User %in% psm & timelog$Date >= term_date, ]$role <- NA
  }
  if(length(daily_hours[daily_hours$User %in% psm & daily_hours$Date >= term_date, ]$role) > 0){
    daily_hours[daily_hours$User %in% psm & daily_hours$Date >= term_date, ]$role <- "TM"
  }
}

# proc.time() - time_started

#set week integer
timelog$week <- paste(year(timelog$Date),week(timelog$Date), sep = "-")
daily_hours$week <- paste(year(daily_hours$Date), week(daily_hours$Date), sep = "-")
labels <- ddply(timelog, .var = c("week"), function(x){
  min <- min(x$Date)
  max <- max(x$Date)
  count <- unique(x$User)
  label <- paste(strftime(min, '%m/%d')," - ", strftime(max, '%m/%d'), sep = "")
  data.frame(week = x$week, 
             label = label)
})
labels_dh <- ddply(daily_hours, .var = c("week"), function(x){
  min <- min(x$Date)
  max <- max(x$Date)
  label <- paste(strftime(min, '%m/%d')," - ", strftime(max, '%m/%d'), sep = "")
  count <- unique(x$User)
  data.frame(week = x$week, 
             label = label)
})
labels <- unique(labels)
labels_dh <- unique(labels_dh)
timelog <- merge(timelog, labels, by = c("week"), all.x = T)
daily_hours <- merge(daily_hours, labels_dh, by = c("week"), all.x = T)

timelog_by_week <- aggregate(Hours ~ User + week + label + role, data = timelog, FUN = sum)
daily_by_week <- aggregate(Hours ~ User + week + label + role, data = daily_hours, FUN = sum)

setwd("C:/R/workspace/timelog/weekly_time/output")
# write.csv(timelog_by_week, file = "timelog_by_week.csv", row.names = F, na = "")
# write.csv(daily_by_week, file = "daily_by_week.csv", row.names = F, na = "")

#excel output to multiple tabs
write.xlsx(x = timelog_by_week, file = "time_by_week.xlsx",sheetName = "timelog_by_week", row.names = FALSE)
write.xlsx(x = daily_by_week, file = "time_by_week.xlsx",sheetName = "daily_by_week", row.names = FALSE, append = TRUE)


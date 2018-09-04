# clear memory
rm(list = ls())

library(data.table)

sysinf <- Sys.info()
if (tolower(sysinf[['sysname']]) == "windows") {
    # windows of mac osx
    setwd("C:\\Users\\amant\\Desktop\\Fall2017\\Stats506-ComputationTools\\repo\\trunk\\personalProject") 
} else {
    # linux
    setwd("/home/ataxali/stats506/project/")
}

dat = fread('./data/TestData.csv')
dat = dat[, .("DateTime" = `Measurement Timestamp`, "Temp" = `Wet Bulb Temperature`,
        "Rain" = `Total Rain`, "Hum" = Humidity)]
dat[, "date" := as.Date(DateTime, "%m/%d/%Y")]

dat[, .("res" = mean(Hum), Temp), by = .(date)][1:10,]



nyc14 = fread('https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv')
nyc14[origin == 'LGA', .(mean = mean(dep_delay)), by = .(month, day)][1:10, ]
nyc14[order(-month), -c('dep_delay'), with = FALSE][1:10, 1:5][c(1,.N),,]

nyc14[c(1,nrow(nyc14))]
nyc14[c(1, .N)]

as.matrix(nyc14)[1:5,]

nyc14[origin=='LGA' & dest=='DTW', .(max_delay = max(dep_delay)), 
      by = .(carrier, flight)
      ][, .(carrier, flight, max_delay, max_delay_q10 = quantile(max_delay, .1))
        ][max_delay < max_delay_q10, -"max_delay_q10", with=FALSE]



nyc14[dest=='DTW', lapply(.SD, mean), by=carrier,
      .SDcols = c("dep_delay", "dep_time")]









days_in_months = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)





sf_data = fread('./data/Road_Weather_Information_Stations.csv')
sf_data = sf_data[, .(AirTemperature, DateTime)]
sf_data = na.omit(sf_data)
sf_data[, "date" := as.Date(DateTime, "%m/%d/%Y")]
sf_data[, "year" := as.numeric(format(date, "%Y"))]
sf_data[, "month" := as.numeric(format(date, "%m"))]
sf_data[, "day" := as.numeric(format(date, "%d"))]
sf_data[, "idx" := year + (month-1) + (day/days_in_months[month])]
sf_data = sf_data[, .(temp = mean(AirTemperature)), by=.(idx, date, year, month, day)]
sf_data = sf_data[, .(temp, "idx"=(idx-year), date)]
save(sf_data, file = "sf_data_date.RData")


library(dplyr)
library(readr)
ch_data = read_delim('./data/Beach_Weather_Stations_-_Automated_Sensors.csv',
                     delim=',',col_names=TRUE)
ch_data = ch_data %>%
    transmute(DateTime = ch_data$`Measurement Timestamp`,
              temp = ch_data$`Wet Bulb Temperature`) %>%
    mutate(date = as.Date(DateTime, "%m/%d/%Y")) %>%
    select(-DateTime) %>%
    na.omit() %>%
    mutate(year = as.numeric(format(date, "%Y")),
           month = as.numeric(format(date, "%m")),
           day = as.numeric(format(date, "%d"))) %>%
    mutate(idx = year + (month-1) + (day/days_in_months[month])) %>%
    group_by(idx, date, year, month, day) %>%
    summarize(temp=mean(temp)) %>%
    ungroup() %>%
    transmute(temp, idx = (idx-year))
save(ch_data, file = "ch_data.RData")


library("readr")



data_files <- list.files(path = "./data/wdlarchive/2016/", pattern = ".csv")
data_sets <- NULL
for(file in data_files) {
    dat = read.csv(file = paste0("./data/wdlarchive/2016/", file), header = TRUE)
    data_sets = rbind(data_sets, dat)
}
data_sets = as.data.table(data_sets)
wd_data = data_sets[, .("date" = Interval_End_Time, "temp" = Ambient_Temperature_Deg_C)]
wd_data = na.omit(wd_data)
wd_data[, "date" := as.Date(date, "%Y-%m-%d?")]
wd_data[, "year" := as.numeric(format(date, "%Y"))]
wd_data[, "month" := as.numeric(format(date, "%m"))]
wd_data[, "day" := as.numeric(format(date, "%d"))]
wd_data[, "idx" := year + (month-1) + (day/days_in_months[month])]
wd_data = wd_data[, .(temp = mean(temp)), by=.(idx, date, year, month, day)]
wd_data = wd_data[, .(temp, "idx"=(idx-year))]
save(wd_data, file = "wd_data.RData")



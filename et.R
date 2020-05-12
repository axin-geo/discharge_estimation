### rescale ET data from monthly to daily
### current unit million m3/month to million m3/day
### Note different month has different num of days
et_mon <- et[start_mon:end_mon,]
## Unit Conversion for ET
convert_tcm_mcm = 0.001 #convert from Thousand m3/month to million m3/month
et_mon$ET <- apply(et_mon[6:8],1 , mean)
et_mon <- et_mon %>%  select(., -c(2:8)) %>% mutate(., ET = ET*convert_tcm_mcm)

et_mon$Month <- as.Date(strptime(et_mon$Month, "%Y%m%d"))
et_day <- left_join(y, et_mon, by = c("datetime" = "Month")) %>% select(c("datetime","ET"))
et_day <- separate(et_day, 1, c("yr", "mon", "day"), convert = T)

for (i in start_yr:end_yr){
  for (j in min(et_day[et_day$yr == i,]$mon):max(et_day[et_day$yr == i,]$mon)){
    ET_daily <- et_day[et_day$yr== i & et_day$mon == j,][1,4]/nrow(et_day[et_day$yr== i & et_day$mon == j,])
    
    for (k in 1:nrow(et_day[et_day$yr== i & et_day$mon == j,])){
      et_day[et_day$yr== i & et_day$mon == j,][k,4] <- ET_daily
    }
    
  }
}
et_day <- et_day %>% unite("datetime", c("yr","mon", "day"), sep = "-"); et_day$datetime <- as.Date(et_day$datetime)
# time series conversion
# et_day <- zoo(et_day, et_day$datetime)

# installing and loading packages
# install.packages(c("dplyer", "tidyverse","ggplot2", "xts","dygraphs","imputeTS"))
library("dplyr")
library("rio")
library("tidyverse")
library("readxl")
library("xts")
library("dygraphs")
library("imputeTS")

s_name <- "Possum Kingdom Lk_09_10"
R <- 10 # sampling gap / temporal resolution
et_GRAND_ID <- 1176
et <- read.table(paste0("C:\\Users\\axin\\OneDrive - Kansas State University\\SWOT_from_Aote\\Supporting data\\ET_candidates\\", et_GRAND_ID, ".txt"), header = T, stringsAsFactors = FALSE) 
start_mon <- which(et$Month == 20081001, arr.ind = TRUE)
end_mon <- which(et$Month == 20100901, arr.ind = TRUE)
start_yr <- 2008; end_yr <- 2010 # move to the top

# sheet conversion ############################################################################################
###############################################################################################################
###############################################################################################################
## convert multiple sheets from Excel into one sheet
y <- excel_sheets(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/", s_name, ".xlsx")) %>% 
  map(~read_xlsx(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/", s_name, ".xlsx"),.)) %>%
  data.frame()

## tidy data
n = c("V", "Q_out", "Q_in"); m = c("site_V", "site_out", "site_in")

tidy <- function(s){ 
  s <- s %>% select(., datetime, contains("site_no"), ends_with(c("32400","30800","30600")), contains("00003"),-contains("cd")) %>%
             filter(., site_no != "15s") %>% rename_at(vars(c(5,6,7)), ~ n) %>% rename_at(vars(contains("site")), ~ m) 
  s$datetime <- as.Date(as.numeric(s$datetime), origin = "1899-12-30")
  return(s)}

y <- tidy(y); #View(y)

# daily dV vs dQ ##############################################################################################
###############################################################################################################
###############################################################################################################
## Unit Conversion
convert_af_mcm = 1233.48/10.0^6 #convert from Thousand acre feet to million m3
convert_cfs_mcmd = 3600.0*24.0*0.0283168/10.0^6 #convert from cubic feet per second to million m3 per day;
y$V <- as.numeric(y$V) * convert_af_mcm; 
y$Q_out <- as.numeric(y$Q_out) * convert_cfs_mcmd; 
y$Q_in <- as.numeric(y$Q_in) * convert_cfs_mcmd; 

## calculate mass balance
dV <- y$V %>% append(.,NA, 0); length(dV) <- nrow(y); dV <- y$V - dV
y <- y %>% mutate(.,dQ = Q_in - Q_out) %>% cbind(., dV)

## correlation of dV and dQ
cor(y$dV, y$dQ, use = "complete.obs")

## Plot dV vs dQ
#range_limit <- max(abs(min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE))), 
#                   abs(max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))))
         
#ggplot(y) +
#  geom_point(mapping = aes(x = dQ, y = dV), color="darkgreen", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) +
#  labs(title ="Daily dQ vs dV", x = "dQ (m^3)", y = "dV (m^3)") +
#  xlim(-range_limit, range_limit) +  ylim(-range_limit, range_limit) +
#  geom_segment(aes(x = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)),  y = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)), xend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE)), yend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))),
#               linetype = "dashed")

# Rescale/Aggregate for R days#################################################################################
###############################################################################################################
###############################################################################################################
## rescale ET data from monthly to daily
## current unit million m3/month to million m3/day
## Note different month has different num of days
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
## time series conversion
## et_day <- zoo(et_day, et_day$datetime)
###############################################################################################################
## Rescale daily dQ and dV by a time period of R (representing SWOT time scale)
## dQ in a unit of millin m3/day, dV in a unit of million m3
N <- rep(NA, R-1)
dy <- y %>% select(., datetime, dQ, dV) %>% left_join(., et_day) %>% mutate(., dV_et =  dV + ET, dV_R = NA, dQ_R = NA, dV_et_R = NA) 

## dV_R == V(R*(n+1))-V(R*n)
for (i in 1:(nrow(dy)%/%R)){dy$dV_R[R*i+1] <- sum(dy$dV[(2+i*R-R):(i*R+1)], na.rm = T)}
## dQ_R
for (i in 1:(nrow(dy)%/%R)){dy$dQ_R[R*i+1] <- sum(dy$dQ[(2+i*R-R):(i*R+1)], na.rm = T)}
## dV_et_R
for (i in 1:(nrow(dy)%/%R)){dy$dV_et_R[R*i+1] <- sum(dy$dV_et[(2+i*R-R):(i*R+1)], na.rm = T)}

## Plotting dQ_R vs dV_R & dV_et_R
range_limit <- max(abs(min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE))), abs(max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE))))
ggplot(dy) +
  geom_point(mapping = aes(x = dQ_R, y = dV_R), color="darkgreen", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) +
  labs(title =paste0("Storage-discharge balance for \n", s_name, " \n(Sampling Gap: ", R, " days)"), x = "dQ (million m^3)", y = "dV (million m^3)") +
  xlim(-range_limit, range_limit) + ylim(-range_limit, range_limit) +
  geom_segment(aes(x = min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE)), y = min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE)), xend = max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE)), yend = max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE))),
              linetype = "dashed") +
  geom_point(mapping = aes(x = dQ_R, y = dV_et_R), color="green", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) 

dV_et_R <- data.frame(datetime = y$datetime, dV_et_R = dy$dV_et_R) %>% xts(., order.by = .$datetime) %>% subset(., select = -datetime)

# Uncertainty Analysis ########################################################################################
###############################################################################################################
###############################################################################################################
# Uncertainty Analysis due to sampling gaps by SWOT
## A loop to create a matrix (mx) of sampling days in different scenarios
mx <- matrix(nrow = nrow(dy))
for (i in 1:R) {
  ### adding R-1 rows of NA of sequence
  r <- rbind(seq(from = i, to = nrow(y), by = R), matrix(ncol = length(seq(from = i, to = nrow(y), by = R)), nrow = R-1)) 
  ### adding NA before the first value from the vector
  r_NA <- c(rep(NA,i-1), r)
  ### length correction 
  length(r_NA) <- nrow(y)
  
  mx <- as.data.frame(cbind(mx, r_NA)); names(mx)[i+1] <- paste0(c("dQ"), i)
}
### drop defaut column from df
mx <- subset(mx, select = -V1) # View(mx)
### convert mx to time series object
mx <- zoo(mx, dy$datetime) 
dy <- dy  %>% xts(., order.by = .$datetime) %>% subset(., select = -c(datetime,ET,dV_et,dV_et_R))
### add # of days as a new column
day <- seq(from = 1, to = nrow(dy)); dy <- cbind(dy, day)  
### join indices of sampling days (mx) to dy 
dy <- merge(dy, mx)

## Due to sampling gaps, dQ is remotely measured every R days by SWOT.
## Here we simulate the effects of sampling gaps and assume dQ on days without measurements, 
## takes on a linear distribution btw sampling days.

### add dQ to sampling days
for (i in 1:R){
  for (j in 1:nrow(dy)){
    if (!is.na(dy[j, i+5]))
      dy[j, i+5] <- dy[j, 1]
  }}
### Interpolate the discharge differences dQ on days without measurements (linear)
### Assume the daily volumes of discharge take on a linear variation
dy <- na_interpolation(dy, option = "linear") %>% subset(., select = -c(day,dQ,dV))
### Rescale/Aggregate daily dQ and dV by a time period of R (representing SWOT time scale)
### dQ in a unit of millin m3/day
for (i in 1:R){
  for (j in 1:((nrow(dy)-i)%/%R)){
    a <- R*(j - 1) + i + 1; b <- j*R + i
    dy[j*R + i, i + 2] <- sum(dy[,i+2][a:b])
    dy[,i+2][a:(b-1)] <- NA
  }
}
### interpolate the volume variation from discharge difference
### Here according to our assumption, the daily volume of discharge take on a linear variation
dy <- na_interpolation(dy, option = "linear") %>% merge(., dV_et_R)
###############################################################################################################
## upper and lower error bars
dy_error <- dy %>% subset(., select = -c(dQ_R,dV_R,dV_et_R)) %>% fortify.zoo() %>% select(-Index)
  
### calculate highest and lowest values for each scenarios/row
dy_error$max <- apply(X = dy_error, MARGIN=1, FUN=max)
dy_error$min <- apply(X = dy_error, MARGIN=1, FUN=min)

### dy_2 
 dy_error <- dy_error %>% select(max, min) %>% zoo(., order.by = y$datetime)
 dy_2 <- dy %>% subset(., select= c(dQ_R,dV_R,dV_et_R)) %>% merge(., dy_error)
###############################################################################################################
# Plot hydrograph with upper/lower bars
 dygraph(dy_2, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, " days)"), ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>% dyLegend(width = 600) %>%
  dySeries(c("min", "dQ_R", "max"), label = "Observed dQ", color = "red") %>% dySeries("dV_R", label = "Observed dV", color = "seagreen") %>% 
  dySeries("dV_et_R", color = "green", drawPoints = TRUE, pointShape = "triangle", pointSize = "3", label = "dV corrected by reservoir evaporation") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) 

# Plot hydrograph showing uncertainty as lines
 dygraph(dy, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, " days)"), ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>% dyLegend(width = 600) %>%
  dySeries("dV_R", color = "seagreen",strokeWidth = 2, label = "Observed dV") %>% dySeries("dQ_R", color = "red",strokeWidth = 2, label = "Observed dQ") %>%
  dySeries("dV_et_R", color = "green", drawPoints = TRUE, pointShape = "triangle", pointSize = "3", label = "dV corrected by reservoir evaporation") %>% 
  dyGroup(c(paste0(c("dQ"), 1:R)), color = rep("pink", R)) 
 
#Daily to Monthly data Manipulation############################################################################
###############################################################################################################
###############################################################################################################
#rescale in situ discharge and storage data from daily to monthly 
dy_mon <- y %>% select(., datetime, dQ, dV) %>%  mutate(., dV_R = NA, dQ_R = NA); dy_mon <- separate(dy_mon, 1, c("yr", "mon", "day"), convert = T)
 
for (i in start_yr:end_yr){
 for (j in min(dy_mon[dy_mon$yr == i,]$mon):max(dy_mon[dy_mon$yr == i,]$mon)){
    dy_mon[dy_mon$yr== i & dy_mon$mon == j,][1,6] <- sum(dy_mon$dV[dy_mon$yr == i & dy_mon$mon == j], na.rm = T)
    dy_mon[dy_mon$yr== i & dy_mon$mon == j,][1,7] <- sum(dy_mon$dQ[dy_mon$yr == i & dy_mon$mon == j], na.rm = T)
  }}

dy_mon <- dy_mon %>% unite("datetime", c("yr","mon", "day"), sep = "-"); dy_mon$datetime <- as.Date(dy_mon$datetime)
dy_mon <- left_join(dy_mon, et_mon, by = c("datetime"="Month")) %>% mutate(., dV_et_R = dV_R + ET)
dV_et_mon <- data.frame(dV_et_R = dy_mon$dV_et_R) 

# Plot dV vs dQ
range_limit <- max(abs(min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R,na.rm = TRUE))), abs(max(max(dy_mon$dV_R,na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE))))
ggplot(dy_mon) +
  geom_point(mapping = aes(x = dQ_R, y = dV_R), color="darkgreen", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) +
  labs(title =paste0("Storage-discharge balance for \n", s_name, " \n(Sampling Gap: monthly)"), x = "dQ (million m^3)", y = "dV (million m^3)") +
  xlim(-range_limit, range_limit) + ylim(-range_limit, range_limit) +
  geom_segment(aes(x = min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R, na.rm = TRUE)), y = min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R, na.rm = TRUE)), xend = max(max(dy_mon$dV_R, na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE)), yend = max(max(dy_mon$dV_R, na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE))),linetype = "dashed") +
  geom_point(mapping = aes(x = dQ_R, y = dV_et_R), color="green", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE)

### Plot hydrograph
dy_mon <- dy_mon %>% select(., -c(dQ, dV, ET, dV_et_R)) %>% na_interpolation(., option = "linear") 
xx <- xts(dy_mon[,1], order.by = dy_mon$datetime); xx <- cbind(xx, dy_mon$dV_R,dy_mon$dQ_R, dV_et_mon$dV_et_R) %>% subset(., select = -xx); colnames(xx) <- c("dV_R", "dQ_R", "dV_et_R")
dygraph(xx, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: monthly)"), ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>% dyLegend(width = 600) %>% 
  dySeries("dV_R", color = "seagreen",strokeWidth = 1) %>% dySeries("dQ_R", color = "red",strokeWidth = 1) %>% dySeries("dV_et_R", color = "green", pointShape = "triangle", pointSize = "3") 

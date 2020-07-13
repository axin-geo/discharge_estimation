# Code from Aote Xin, up to July 11, 2020
# SWOT reservoir-river mass balance

# [NOTES: THESE CODES WERE DEVELOPED FOR 'TESTING A PROPOSED ALGORITHM FOR ESTIMATING WATER DISCHARGE AT RIVER-RESERVOIR INTERFACE: POTENTIAL APPLICATIONS FOR THE SURFACE WATER AND OCEAN TOPOGRAPHY SATELLITE MISSION' PROJECT]

# installing and loading packages that are used in the following script
# install.packages(c("dplyer", "tidyverse","ggplot2", "xts","dygraphs","imputeTS"))
library("rio"); # Import, Export, and Convert Data Files
library("tidyverse"); # for common data minipulation(dplyr), plotting(ggplot2)
library("readxl"); # for reading excel files
library("xts"); # for time-series data object; an extension of ZOO package
library("dygraphs"); # plotting package for time-series objects
library("imputeTS"); # Time Series Missing Value Imputation
library("zoo") # for time-series data object
library("grid") # for adding annotation in ggplot

#############################
# Read and convert raw data #
#############################

# Here we will read the long-term monthly evaportranspiration data developed by Dr. Gao Huilin for Possum Kingdom lake
s_name <- "Possum Kingdom Lk_09_10"
R <- 11 # sampling gap / temporal resolution
et_GRAND_ID <- 1176; et <- read.table(paste0("D:/Aote/OneDrive - Kansas State University/SWOT_from_Aote/Supporting data/ET/Reservoir_evaporation721/", et_GRAND_ID, ".txt"), header = T, stringsAsFactors = FALSE) 
start_mon <- which(et$Month == 20081001, arr.ind = TRUE); end_mon <- which(et$Month == 20100901, arr.ind = TRUE)
start_yr <- 2008; end_yr <- 2010 

head(et, 6)

# In the next chunk, the in situ data are read which includes the discharge measurements for inflow and outflow rivers and reservoir storage measurements all at a daily basis. Data acquired from USGS NWIS.
y <- excel_sheets(paste0("D:/Aote/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/", s_name, ".xlsx")) %>% 
  map(~read_xlsx(paste0("D:/Aote/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/", s_name, ".xlsx"),.)) %>% ## convert multiple sheets from Excel into one sheet
  data.frame()

# tidy data
n = c("V", "Q_out", "Q_in"); m = c("site_V", "site_out", "site_in")

# function "tidy" extracts useful columns from the raw data frame and returns a single dataframe with date, site numbers, discharge measurements for inflow and outflow rivers and reservoir storage measurements
# 32400: observation at midnight; 00003: daily mean observation; 30600: observation at 6:00am; 30800: observation at 8:00am
tidy <- function(s){ 
  s <- s %>% select(., datetime, contains("site_no"), ends_with(c("32400","30800","30600")), contains("00003"),-contains("cd")) %>%
             filter(., site_no != "15s") %>% rename_at(vars(c(5,6,7)), ~ n) %>% rename_at(vars(contains("site")), ~ m) 
  s$datetime <- as.Date(as.numeric(s$datetime), origin = "1899-12-30")
  return(s)}

y <- tidy(y); head(y, 6)


###################################
# Derive and plot daily dV and dQ #
###################################

# Unit Conversion
convert_af_mcm = 1233.48/10.0^6 ## convert from Thousand acre feet to million m3
convert_cfs_mcmd = 3600.0*24.0*0.0283168/10.0^6 ## convert from cubic feet per second to million m3 per day;
y$V <- as.numeric(y$V) * convert_af_mcm; 
y$Q_out <- as.numeric(y$Q_out) * convert_cfs_mcmd; 
y$Q_in <- as.numeric(y$Q_in) * convert_cfs_mcmd; 

# calculate mass balance
dV <- y$V %>% append(.,NA, 0); length(dV) <- nrow(y); dV <- y$V - dV
y <- y %>% mutate(.,dQ = Q_in - Q_out) %>% cbind(., dV)

# correlation of dV and dQ
correlation <- cor(y$dV, y$dQ, use = "complete.obs")

# Plot dV vs dQ
range_limit <- max(abs(min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE))), 
                   abs(max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))))
         
ggplot(y) +
  geom_point(mapping = aes(x = dQ, y = dV), color="darkgreen", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) +
  labs(title ="Daily dQ vs dV", x = "dQ (m^3)", y = "dV (m^3)") +
  xlim(-range_limit, range_limit) +  ylim(-range_limit, range_limit) +
  geom_segment(aes(x = min(min(dV, na.rm = TRUE), min(dQ,na.rm = TRUE)),  y = min(min(dV, na.rm = TRUE), min(dQ,na.rm = TRUE)), xend = max(max(dV,na.rm = TRUE), max(dQ, na.rm = TRUE)), yend = max(max(dV,na.rm = TRUE), max(dQ, na.rm = TRUE))),
               linetype = "dashed")


####################
# Rescale: ET data #
####################

# This code chunk rescales the monthly average ET data to a daily time scale.
head(et, 6)

# current unit million m3/month to million m3/day
# Note: different month has different num of days
et_mon <- et[start_mon:end_mon,]

# Unit Conversion for ET
convert_tcm_mcm = 0.001 ## convert from Thousand m3/month to million m3/month
et_mon$ET <- apply(et_mon[,6:8], 1, mean)
et_mon <- et_mon %>%  select(., -c(2:8)) %>% mutate(., ET = ET*convert_tcm_mcm)
et_mon$Month <- as.Date(strptime(et_mon$Month, "%Y%m%d"))

et_day <- left_join(y, et_mon, by = c("datetime" = "Month")) %>% select(c("datetime","ET"))
et_day <- separate(et_day, 1, c("yr", "mon", "day"), convert = T)

# A loop converting monthly ET data to daily ET data based on an assumption that ET remains unchanged every day for each month
for (i in start_yr:end_yr){
  for (j in min(et_day[et_day$yr == i,]$mon):max(et_day[et_day$yr == i,]$mon)){
    ET_daily <- et_day[et_day$yr == i & et_day$mon == j,][1,4]/nrow(et_day[et_day$yr == i & et_day$mon == j,])
    for (k in 1:nrow(et_day[et_day$yr== i & et_day$mon == j,])){
      et_day[et_day$yr== i & et_day$mon == j,][k,4] <- ET_daily
    }
  }
}

et_day <- et_day %>% unite("datetime", c("yr","mon", "day"), sep = "-"); et_day$datetime <- as.Date(et_day$datetime)
head(et_day, 6)


#########################
# Rescale: in situ data #
#########################

# This code chunk rescales daily discharge difference and storage variation derived from previous codes by a time period of R (a variable representing SWOT time scale)
# Note: Using daily V and V_et rather than daily dV to calculate rescaled dV and V_et (in case some daily dV has missing in situ observations)
# Note: dQ in a unit of millin m3/day, dV in a unit of million m3

# Define a function 'mx' that returns a matrix simulating sampling scenarios. ***Required input: 'colname_mx' as column names in the returned matrix
mx <- function(colname_mx){
  mx <- matrix(nrow = nrow(y));
  for (i in 1:R) {
    ## adding R-1 rows of NA of sequence
    r <- rbind(seq(from = i, to = nrow(y), by = R), matrix(ncol = length(seq(from = i, to = nrow(y), by = R)), nrow = R - 1)); 
    ## adding i-1 of NA before the first value of the vector
    r_NA <- c(rep(NA, i - 1), r);
    ## length correction 
    length(r_NA) <- nrow(y);
    
    mx <- as.data.frame(cbind(mx, r_NA)); 
    names(mx)[i + 1] <- paste0(c(colname_mx), i)
  }
  
  # drop defaut column from df
  mx <- subset(mx, select = -V1); 
  
  return(mx) 
}

###################################
# Rescale: discharge data ('Q_est')
Q <- y %>% select(., datetime, dQ) 

# A matrix object simulating sampling scenarios
Q_est <- mx('Q')

Q_est <- cbind(Q, Q_est)

# Aggregate daily discharge difference to volumeric unit
# NOTE: dQ in a unit of millin m3/day, Q in a unit of million m3
for (i in 1:R){
  for (j in 1:((nrow(Q_est) - i) %/% R)){
    a <- R*(j - 1) + i + 1; b <- j*R + i;
    
    if(j == 1){
      Q_est[i, i + 2] <- sum(Q_est[, 2][1:i]);
      Q_est[j*R + i, i + 2] <- sum(Q_est[, 2][a:b]);
      Q_est[,i + 2][a:(b - 1)] <- NA;
      } 
    else{
      Q_est[j*R + i, i + 2] <- sum(Q_est[, 2][a:b]);
      Q_est[,i + 2][a:(b - 1)] <- NA;
      }
  }
}

#################################
# Rescale: storage data ('V_obs' and 'V_et_obs')
V <- y %>% select(., datetime, dV, V) %>% left_join(., et_day)

# A matrix object simulating sampling scenarios
V_obs <- mx('V')

V_obs <- V %>% select(., -ET) %>% cbind(., V_obs)

# Assignment of storage observations in different sampling scenarios

# Calculate the storage variation in different sampling scenarios

# Aggregate daily discharge difference to volumeric unit
# NOTE: dQ in a unit of millin m3/day, Q in a unit of million m3
for (i in 1:R){
  for (j in 1:((nrow(Q_est) - i) %/% R)){
    a <- R*(j - 1) + i + 1; b <- j*R + i;
    
    if(j == 1){
      Q_est[i, i + 2] <- sum(Q_est[, 2][1:i]);
      Q_est[j*R + i, i + 2] <- sum(Q_est[, 2][a:b]);
      Q_est[,i + 2][a:(b - 1)] <- NA;
    } 
    else{
      Q_est[j*R + i, i + 2] <- sum(Q_est[, 2][a:b]);
      Q_est[,i + 2][a:(b - 1)] <- NA;
    }
  }
}













dy <- y %>% select(., datetime, dQ, dV, V) %>% left_join(., et_day) %>% mutate(., V_R = NA, dV_R = NA, V_et = V + ET, V_et_R = NA, dV_et_R = NA) 
head(dy,15)

# dQ_R (when i = 1, 12<-[2:12])
#i <- 1; 
#while (i*R < nrow(dy)){
#  dy$dQ_R[R*i + 1] <- sum(dy$dQ[(2 + i*R - R):(i*R + 1)], na.rm = T); 
#  i <- i + 1
#  }

# V_R (Observation of storage every R days)
dy$V_R[1] <- dy$V[1]; 
for (i in 2:(nrow(dy) - 1)){
  if(i %% R == 0) 
    dy$V_R[i + 1] <- dy$V[i + 1]
  }

# dV_R (Storage variation every R days)
i <- 1; 
while (i*R < nrow(dy)){
  dy$dV_R[i*R + 1] <- (dy$V_R[i*R + 1] - dy$V_R[(i - 1)*R + 1]); 
  i <- i + 1
  }

# V_et_R ((Observation of storage added with ET every R days))
dy$V_et_R[1] <- dy$V_et[1];
for (i in 2:(nrow(dy) - 1)){
  if(i %% R == 0) 
    dy$V_et_R[i + 1] <- dy$V_et[i + 1]
  }

# dV_et_R
i <- 1;
while (i*R < nrow(dy)){
  dy$dV_et_R[i*R + 1] <- (dy$V_et_R[i*R + 1] - dy$V_et_R[(i - 1)*R + 1]); 
  i <- i + 1
  }

# Q1:QR (Estimated discharge difference derived from daily discharge difference)
## Binding 'mx_est' to 'dy'
dy <- dy %>% select(-c())
dy <- cbind(dy, mx_est)

# deleting columns with daily observations
dy <- dy %>%
  select(datetime, dQ, dV, dQ_R, dV_R, dV_et_R)

head(dy,20)
head(y)

# Plotting dQ_R vs dV_R & dV_et_R
range_limit <- max(abs(min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE))), abs(max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE))))
ggplot(dy) +
  geom_point(mapping = aes(x = dQ_R, y = dV_R), color="darkgreen", shape= 17, alpha = 1/2, size = 2, na.rm = TRUE) +
  geom_segment(aes(x = min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE)), y = min(min(dy$dV_R, na.rm = TRUE), min(dy$dQ_R, na.rm = TRUE)), xend = max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE)), yend = max(max(dy$dV_R, na.rm = TRUE), max(dy$dQ_R, na.rm = TRUE))),linetype = "dashed") +
  geom_point(mapping = aes(x = dQ_R, y = dV_et_R), color="red", shape= 17, alpha = 1/2, size = 2, na.rm = TRUE) +
  labs(title =paste0("Storage-discharge balance for \n", s_name, " \n(Sampling Gap: ", R, " days)"), x = "dQ (million m^3)", y = "dV (million m^3)") +
  xlim(-range_limit, range_limit) + ylim(-range_limit, range_limit) +
  stat_smooth(aes(dy$dQ_R, dy$dV_et_R), method = "lm", col = "red", se = FALSE)

# Create a dataframe object 'dy_R' with observations at a R-day timescale
dy_R <- dy %>% xts(., order.by = .$datetime) %>% subset(., select = -c(datetime, dQ, dV))


#############################################
# Uncertainty analysis due to sampling gaps #
#############################################

# A loop to create a matrix (mx) of sampling days in different scenarios
mx <- matrix(nrow = nrow(dy));
for (i in 1:R) {
  ## adding R-1 rows of NA of sequence
  r <- rbind(seq(from = i, to = nrow(y), by = R), matrix(ncol = length(seq(from = i, to = nrow(y), by = R)), nrow = R - 1)); 
  ## adding i-1 of NA before the first value of the vector
  r_NA <- c(rep(NA, i - 1), r);
  ## length correction 
  length(r_NA) <- nrow(y);
  
  mx <- as.data.frame(cbind(mx, r_NA)); 
  names(mx)[i + 1] <- paste0(c("dQ"), i)
}

# drop defaut column from df
mx <- subset(mx, select = -V1) 

# convert mx to time series object
mx <- zoo(mx, dy$datetime) 
dy <- dy  %>% xts(., order.by = .$datetime) %>% subset(., select = -c(datetime, dV_et_R))

# add number of days as a new column
day <- seq(from = 1, to = nrow(dy)); dy <- cbind(dy, day)  

# join indices of sampling days (mx) to dy 
dy <- merge(dy, mx)

###############################################################
# Unit conversion of discharge difference from flow to volume #
###############################################################

# Due to sampling gaps, discharge difference can only be calculated every R days by SWOT.
# Here we simulate the effects of sampling gaps and assume discharge difference on days without measurements takes on a linear distribution btw sampling days.

# Assign discharge difference values in previously derived sequences of R scenarios
for (i in 1:R){
  for (j in 1:nrow(dy)){
    if (!is.na(dy[j, i + 5]))
      dy[j, i + 5] <- dy[j, 1]
  }}

# Linearly interpolate the discharge differences on days without measurements
dy <- na_interpolation(dy, option = "linear") %>% subset(., select = -c(day,dQ,dV))

# Aggregate daily dQ and dV by a time period of R (representing SWOT time scale)
# dQ in a unit of millin m3/day
for (i in 1:R){
  for (j in 1:((nrow(dy)-i)%/%R)){
    a <- R*(j - 1) + i + 1; b <- j*R + i
    dy[j*R + i, i + 2] <- sum(dy[,i+2][a:b])
    dy[,i+2][a:(b-1)] <- NA
  }
}


# unify the time steps for discharge simulation with #R scenarios
QR <- dy %>%
  subset(., select = -c(dV_R, dQ_R))


# plot the hydrograph

# interpolate the volume variation from discharge difference
# Here according to our assumption, the daily volume of discharge take on a linear variation
dy <- na_interpolation(dy, option = "linear") %>% merge(., dV_et_R)


## upper and lower error bars
#dy_error <- dy %>% subset(., select = -c(dQ_R,dV_R,dV_et_R)) %>% fortify.zoo() %>% select(-Index)
  
### calculate highest and lowest values for each scenarios/row
#dy_error$max <- apply(X = dy_error, MARGIN=1, FUN=max)
#dy_error$min <- apply(X = dy_error, MARGIN=1, FUN=min)

### dy_2 
 #dy_error <- dy_error %>% select(max, min) %>% zoo(., order.by = y$datetime)
 #dy_2 <- dy %>% subset(., select= c(dQ_R,dV_R,dV_et_R)) %>% merge(., dy_error)

# Plot hydrograph with upper/lower bars
# dygraph(dy_2, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, " days)"), ylab = "Storage change (km^3 / day)") %>% 
#  dyRangeSelector() %>% dyLegend(width = 600) %>%
#  dySeries(c("min", "dQ_R", "max"), label = "Observed dQ", color = "red") %>% dySeries("dV_R", label = "Observed dV", color = "seagreen") %>% 
#  dySeries("dV_et_R", color = "green", drawPoints = TRUE, pointShape = "triangle", pointSize = "3", label = "dV corrected by reservoir evaporation") %>%
#  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) 

# Plot hydrograph showing uncertainty as lines
 dygraph(dy, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, " days)"), ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>% dyLegend(width = 600) %>%
  dySeries("dV_R", color = "seagreen",strokeWidth = 2, label = "Observed dV") %>% dySeries("dQ_R", color = "red",strokeWidth = 2, label = "Observed dQ") %>%
  dySeries("dV_et_R", color = "green", drawPoints = TRUE, pointShape = "triangle", pointSize = "3", label = "dV corrected by reservoir evaporation") %>% 
  dyGroup(c(paste0(c("dQ"), 1:R)), color = rep("pink", R)) 
 


# Daily to Monthly data Manipulation
# rescale in situ discharge and storage data from daily to monthly 
#dy_mon <- y %>% select(., datetime, dQ, dV) %>%  mutate(., dV_R = NA, dQ_R = NA); dy_mon <- separate(dy_mon, 1, c("yr", "mon", "day"), convert = T)
 
#for (i in start_yr:end_yr){
# for (j in min(dy_mon[dy_mon$yr == i,]$mon):max(dy_mon[dy_mon$yr == i,]$mon)){
#    dy_mon[dy_mon$yr== i & dy_mon$mon == j,][1,6] <- sum(dy_mon$dV[dy_mon$yr == i & dy_mon$mon == j], na.rm = T)
#    dy_mon[dy_mon$yr== i & dy_mon$mon == j,][1,7] <- sum(dy_mon$dQ[dy_mon$yr == i & dy_mon$mon == j], na.rm = T)
#  }}

#dy_mon <- dy_mon %>% unite("datetime", c("yr","mon", "day"), sep = "-"); dy_mon$datetime <- as.Date(dy_mon$datetime)


#dy_mon <- left_join(dy_mon, et_mon, by = c("datetime"="Month")) %>% mutate(., dV_et_R = dV_R + ET)
#dV_et_mon <- data.frame(dV_et_R = dy_mon$dV_et_R) 

# Plot dV vs dQ
#range_limit <- max(abs(min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R,na.rm = TRUE))), abs(max(max(dy_mon$dV_R,na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE))))
#ggplot(dy_mon) +
#  geom_point(mapping = aes(x = dQ_R, y = dV_R), color="darkgreen", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE) +
#  labs(title =paste0("Storage-discharge balance for \n", s_name, " \n(Sampling Gap: monthly)"), x = "dQ (million m^3)", y = "dV (million m^3)") +
#  xlim(-range_limit, range_limit) + ylim(-range_limit, range_limit) +
#  geom_segment(aes(x = min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R, na.rm = TRUE)), y = min(min(dy_mon$dV_R, na.rm = TRUE), min(dy_mon$dQ_R, na.rm = TRUE)), xend = max(max(dy_mon$dV_R, na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE)), yend = max(max(dy_mon$dV_R, na.rm = TRUE), max(dy_mon$dQ_R, na.rm = TRUE))),linetype = "dashed") +
#  geom_point(mapping = aes(x = dQ_R, y = dV_et_R), color="green", shape= 17, alpha = 1/2, size = 3, na.rm = TRUE)

### Plot hydrograph
#dy_mon <- dy_mon %>% select(., -c(dQ, dV, ET, dV_et_R)) %>% na_interpolation(., option = "linear") 
#xx <- xts(dy_mon[,1], order.by = dy_mon$datetime); xx <- cbind(xx, dy_mon$dV_R,dy_mon$dQ_R, dV_et_mon$dV_et_R) %>% subset(., select = -xx); colnames(xx) <- c("dV_R", "dQ_R", "dV_et_R")
#dygraph(xx, main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: monthly)"), ylab = "Storage change (km^3 / day)") %>% 
#  dyRangeSelector() %>% dyLegend(width = 600) %>% 
#  dySeries("dV_R", color = "seagreen",strokeWidth = 1) %>% dySeries("dQ_R", color = "red",strokeWidth = 1) %>% dySeries("dV_et_R", color = "green", pointShape = "triangle", pointSize = "3") 



# --------------------------------------------------------
# JW: next step, calculate error statistics for different temporal aggregation scales. 
# --------------------------------------------------------
##################
# Error Analysis #
##################
 
# Error due to samping gaps, dQ_R vs. dQ (1:11)
dy.t <- fortify.zoo(dy) 
dy.long <- dy.t %>%
  select(., 3:(3+R)) %>%
  pivot_longer(-dQ_R, names_to = "variable", values_to = "value") 

# bias, sd, rrmse
rr_gap <- (dy.long$dQ_R - dy.long$value)/dy.long$value
b_gap <- mean(rr_gap)
sd_gap <- sd(rr_gap)
rrmse_gap <- sqrt(b_gap^2 + sd_gap^2)
b_gap; sd_gap; rrmse_gap

# plot
# Create a text
anno <- paste0("MRR:",round(b_gap, 3), "\nSDRR:", round(sd_gap, 3),"\nrRMSE:", round(rrmse_gap, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

range_limit <- max(max(abs(range(dy.long$dQ_R))), max(abs(range(dy.long$value))))
ggplot(dy.long, aes(value, dQ_R, colour = variable)) + geom_point()+ xlim(-50,50) + ylim(-50,50) + annotation_custom(grob) + xlab("dQ (1:11)")
ggplot(dy.long, aes(value, dQ_R)) + geom_point()+ xlab("dQ (1:11)") + xlim(-50,50) + ylim(-50,50) + annotation_custom(grob) + xlab("dQ (1:11)")




# Physical error dV_R vs. dQ (1:11) 

# w/o ET loss
dy.long_p <- dy.t %>%
  select(., c(2, 4:(3+R))) %>%
  pivot_longer(-dV_R, names_to = "variable", values_to = "value") %>%
  filter(dV_R != dy.t[1,2])

# bias, sd, rrmse
rr_p <- (dy.long_p$dV_R - dy.long_p$value)/dy.long_p$value
b_p <- mean(rr_p)
sd_p <- sd(rr_p)
rrmse_p <- sqrt(b_p^2 + sd_p^2)
b_p; sd_p; rrmse_p

# plot
# Create a text
anno <- paste0("MRR:",round(b_p, 3), "\nSDRR:", round(sd_p, 3),"\nrRMSE:", round(rrmse_p, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

ggplot(dy.long_p, aes(value, dV_R, colour = variable)) + geom_point() + xlab("dQ (1:11)") + xlim(-50,50) + ylim(-50,50) + annotation_custom(grob)


# w/ ET loss
dy.long_t <- na.interpolation(dy.t)
dy.long_p_et <- dy.t %>%
  na.interpolation(.,) %>%
  select(., c(4:(4+R))) %>%
  pivot_longer(-dV_et_R, names_to = "variable", values_to = "value")

# bias, sd, rrmse
rr_p_et <- (dy.long_p_et$dV_et_R - dy.long_p_et$value)/dy.long_p_et$value
b_p_et <- mean(rr_p_et)
sd_p_et <- sd(rr_p_et)
rrmse_p_et <- sqrt(b_p_et^2 + sd_p_et^2)
b_p_et; sd_p_et; rrmse_p_et

# plot
# Create a text
anno <- paste0("MRR:",round(b_p_et, 3), "\nSDRR:", round(sd_p_et, 3),"\nrRMSE:", round(rrmse_p_et, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

ggplot(dy.long_p_et, aes(value, dV_et_R, colour = variable)) + geom_point() + xlab("dQ (1:11)")+ xlim(-50,50) + ylim(-50,50) + annotation_custom(grob)

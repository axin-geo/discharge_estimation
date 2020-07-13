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
# Note: dQ in a unit of millin m3/day, dV in a unit of million m3

# Define a function 'mx' that returns a matrix simulating sampling scenarios. ***Required input as a charactor string: 'colname_mx' as column names in the returned matrix
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

###############################################################################################
# Rescale: discharge data ('Q_est') ###########################################################
# NOTE:We used daily discharge difference to derive discharge difference in a R-day timescale #
###############################################################################################

Q <- y %>% select(., datetime, dQ) 

# A matrix object simulating sampling scenarios
Q_est <- mx('dQ')

Q_est <- cbind(Q, Q_est)

# Aggregate daily discharge difference to a volumeric unit
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

###################################################################################################################################
# Rescale: storage data ('V_obs') ##################################################################################
# NOTE: We used daily storage observations rather than daily storage variations to derive storage variations in a R-day timescale #
###################################################################################################################################

V <- y %>% select(., datetime, V) %>% left_join(., et_day)

# A matrix object simulating sampling scenarios
V_obs <- mx('dV')

V_obs <- V %>% select(., -ET) %>% cbind(., V_obs)

# Calculate the storage variation in different sampling scenarios
for (i in 1:R){
  for (j in 1:nrow(V_obs)){
    if(j == i){
      V_obs[j, i + 2] <- NA
    }
    else if((j - i) %% R == 0){ 
      V_obs[j, i + 2] <- V_obs[j, 2] - V_obs[j - R, 2]
    }
  }
}

###################################################################################################################################
# Rescale: storage data added with ET ('V_et_obs') ##################################################################################
# NOTE: We used daily storage observations rather than daily storage variations to derive storage variations in a R-day timescale #
###################################################################################################################################

# A matrix object simulating sampling scenarios
V_et_obs <- mx('dV_et')

V_et_obs <- V %>% mutate(., V_et = V + ET) %>% select(., datetime, V_et) %>% cbind(., V_et_obs)

# Calculate the storage variation in different sampling scenarios
for (i in 1:R){
  for (j in 1:nrow(V_et_obs)){
    if(j == i){
      V_et_obs[j, i + 2] <- NA
    }
    else if((j - i) %% R == 0){ 
      V_et_obs[j, i + 2] <- V_et_obs[j, 2] - V_et_obs[j - R, 2]
    }
  }
}

#############################################################################
# Simulating observed discharge differences by a SWOT's timescale ('Q_obs') #
#############################################################################
# Here we simulate the effects of sampling gaps and assumed discharge difference on days without measurements takes on a linear distribution btw sampling days.
# A matrix object simulating sampling scenarios
Q_obs <- mx('RdQ');
Q_obs <- cbind(Q, Q_obs)

# Assign discharge difference values in different sampling scenarios
for (i in 1:R){
  for (j in 1:nrow(Q_obs)){
    if (!is.na(Q_obs[j, i + 2])){
      Q_obs[j, i + 2] <- Q_obs[j, 2]
      }
    }
}

# Linearly interpolate the discharge differences on days without measurements
Q_obs <- na_interpolation(Q_obs, option = "linear") %>% select(., -dQ)

# Aggregate daily dQ and dV by a time period of R (representing SWOT time scale)
# dQ in a unit of millin m3/day
for (i in 1:R){
  for (j in 1:((nrow(Q_obs)-i) %/% R)){
    a <- R*(j - 1) + i + 1; b <- j*R + i;
    Q_obs[j*R + i, i + 1] <- sum(Q_obs[, i + 1][a:b]);
    Q_obs[, i + 1][a:(b - 1)] <- NA
  }
}

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

# Code from Aote Xin, up to Aug 1 , 2020
# SWOT reservoir-river mass balance

# [NOTES: THESE CODES WERE DEVELOPED FOR 'TESTING A PROPOSED ALGORITHM FOR ESTIMATING WATER DISCHARGE AT RIVER-RESERVOIR INTERFACE: POTENTIAL APPLICATIONS FOR THE SURFACE WATER AND OCEAN TOPOGRAPHY SATELLITE MISSION' PROJECT]

##########################################################
# install and load packages that are used in this script #
##########################################################

# install.packages(c("dplyer", "tidyverse","ggplot2", "xts","dygraphs","imputeTS", "plotly"))
library("rio");       # Import, Export, and Convert Data Files
library("tidyverse"); # for common data minipulation(dplyr), plotting(ggplot2)
library("readxl");    # for reading excel files
library("xts");       # for time-series data object; an extension of ZOO package
library("dygraphs");  # plotting package for time-series objects
library("imputeTS");  # Time Series Missing Value Imputation
library("zoo")        # for time-series data object
library("grid")       # for adding annotation in ggplot
library("plotly")     # fpr building a interactive scatterplot

#############################
# Read and convert raw data #
#############################

# candidate's filename and sampling gap 
s_name <- "Allatoona Lake_13_14"; R <- 11; Th <- 1000

# Read the long-term monthly evaportranspiration data developed by Dr. Gao Huilin
et_GRAND_ID <- 616; 
loc_ET <- paste0("D:/Aote/OneDrive - Kansas State University/SWOT_from_Aote/Supporting data/ET/Reservoir_evaporation721/", et_GRAND_ID, ".txt");
et <- read.table(loc_ET, header = T, stringsAsFactors = FALSE); head(et, 6)

# Define two vectors 'start/end_mon' storing the positions of duration of interest in ET table, used for slicing the original ET table
# For instance: 'start_mon' being 296 indicates the starting month of is situ data is at 296th row in original ET table
start_yr <- 2012; end_yr <- 2014
start_mon <- which(et$Month == paste0(start_yr, "1001"), arr.ind = TRUE); 
end_mon <- which(et$Month == paste0(end_yr, "0901"), arr.ind = TRUE);

# read in situ data of daily discharge observations for inflow and outflow and reservoir storage observations. Data acquired from USGS NWIS.
loc_insitu <- paste0("D:/Aote/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/", s_name, ".xlsx")

y <- excel_sheets(loc_insitu) %>% map(~read_xlsx(loc_insitu,.)) %>% # 'map' function converts multiple sheets from Excel into one sheet
  data.frame()

# define a function 'tidy' extracts columns from the raw data and returns a single dataframe with date, site numbers, discharge for inflow and outflow and reservoir storage 
# NOTE: 32400: observation at midnight; 00003: daily mean observation; 30600: observation at 6:00am; 30800: observation at 8:00am

# column names for newly derived data frame 'y'
n = c("V", "Q_out", "Q_in"); m = c("site_V", "site_out", "site_in")

tidy <- function(s){ 
  s <- s %>% select(., datetime, contains("site_no"), ends_with(c("32400", "30800", "30600")), contains("00003"),-contains("cd")) %>% filter(., site_no != "15s") %>% 
             rename_at(vars(c(5, 6, 7)), ~ n) %>% rename_at(vars(contains("site")), ~ m);
  
  s$datetime <- as.Date(as.numeric(s$datetime), origin = "1899-12-30");
  
  return(s)
  }

y <- tidy(y); head(y, 6)


###################################
# Derive and plot daily dV and dQ #
###################################

# Imperial to Metric system conversion
convert_af_mcm <- Th * 1233.4818375 / 10.0^6; ## convert from (thousand) acre feet to million m3
convert_cfs_mcmd <- 3600.0*24.0*0.0283168 / 10.0^6; ## convert from cubic feet per second to million m3 per day;

y$V <- as.numeric(y$V) * convert_af_mcm; 
y$Q_out <- as.numeric(y$Q_out) * convert_cfs_mcmd; 
y$Q_in <- as.numeric(y$Q_in) * convert_cfs_mcmd

# calculate daily discharge difference 'dQ' and daily storage variation 'dV'
dV <- y$V %>% append(., NA, 0); length(dV) <- nrow(y); dV <- y$V - dV
y <- y %>% mutate(., dQ = Q_in - Q_out) %>% cbind(., dV)

# correlation of dV and dQ
correlation <- cor(y$dV, y$dQ, use = "complete.obs")

# Plot daily discharge difference 'dQ' vs. daily storage variation 'dV'
range_limit <- max(abs(min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE))), 
                   abs(max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))))

anno <- paste0("correlation:", round(correlation, 3))
cor_grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))
         
ggplot(y) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 0, color = "black", lty = 2, alpha = 0.3) +
  geom_vline(xintercept = 0, color = "black", lty = 2, alpha = 0.3)  +
  geom_point(mapping = aes(x = dQ, y = dV), color="darkblue", shape= 19, alpha = 0.15, size = 2.2, na.rm = TRUE) +
  labs(title =paste0("Daily Storage Variation vs Discharge Difference \n", s_name), x = "dQ (mcm/day)", y = "dV (mcm)") +
  xlim(-range_limit, range_limit) +  ylim(-range_limit, range_limit) +
  annotation_custom(cor_grob) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 9.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.3, linetype = "solid"),
        axis.line = element_line(color = "black"))


####################
# Rescale: ET data #
####################

# rescale the monthly average ET data to a daily timescale, assuming ET remains unchanged every day for each month
# Unit conversion from thousand m3/month to million m3/day
# CAUTION: different month has different num of days

# Slice ET data based on in situ data's temporal duration
et_mon <- et[start_mon:end_mon,]

# Unit conversion from thousand m3/month to million m3/month
convert_tcm_mcm = 0.001; 
et_mon$ET <- apply(et_mon[,6:8], 1, mean);
et_mon <- et_mon %>%  select(., -c(2:8)) %>% mutate(., ET = ET*convert_tcm_mcm)

# Standardize date's format. For instance, from '20081001' to '2008-10-01'
et_mon$Month <- as.Date(strptime(et_mon$Month, "%Y%m%d"))

# define a data frame object 'et_day' with date index at a daily time step
et_day <- left_join(y, et_mon, by = c("datetime" = "Month")) %>% select(c("datetime","ET"));

# Use a loop to convert monthly ET data to daily ET data
et_day <- separate(et_day, 1, c("yr", "mon", "day"), convert = T);

for (i in start_yr:end_yr){
  for (j in min(et_day[et_day$yr == i,]$mon):max(et_day[et_day$yr == i,]$mon)){
    
    ET_daily <- et_day[et_day$yr == i & et_day$mon == j,][1,4]/nrow(et_day[et_day$yr == i & et_day$mon == j,]);
    
    for (k in 1:nrow(et_day[et_day$yr== i & et_day$mon == j,])){
      et_day[et_day$yr== i & et_day$mon == j,][k,4] <- ET_daily
    }
  }
}

et_day <- et_day %>% unite("datetime", c("yr","mon", "day"), sep = "-"); et_day$datetime <- as.Date(et_day$datetime);
head(et_day, 6)

#########################
# Rescale: in situ data #
#########################

# Rescale daily discharge difference and storage variation derived from previous codes by a time period of R days
# NOTE: discharge difference in an unit of millin m3/day (volumetric flow rate), storage variation in an unit of million m3 (volume)

# Define a function 'mx' that returns a matrix simulating sampling scenarios. 
# ***Required input: a charactor string 'colname_mx' specifying column names in the returned matrix
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
# Rescale: discharge data ('Q_obs') ########################################################### 
# NOTE:We used daily discharge difference to derive discharge difference in a R-day timescale #
###############################################################################################

Q <- y %>% select(., datetime, dQ) %>% left_join(., et_day)

# A matrix object simulating sampling scenarios
Q_obs <- mx('dQ')

Q_obs <- Q %>% select(., -ET) %>% cbind(., Q_obs)

# Aggregate daily discharge difference to a unit of  per day per R days
for (i in 1:R){
  for (j in 1:((nrow(Q_obs) - i) %/% R)){
    a <- R*(j - 1) + i ; b <- j*R + i - 1;
    
    if(j == 1){
      Q_obs[i, i + 2] <- sum(Q_obs[1:(i-1), 2]);
      Q_obs[j*R + i, i + 2] <- sum(Q_obs[a:b, 2]);
      } 
    else{
      Q_obs[j*R + i, i + 2] <- sum(Q_obs[a:b, 2]);
      Q_obs[(a+1):b,i + 2] <- NA;
      }
  }
}

Q_obs <- Q_obs[ ,-2]


###################################################################################################################################
# Rescale: storage data ('V_obs') #################################################################################################
# NOTE: We used daily storage observations rather than daily storage variations to derive storage variations in a R-day timescale #
###################################################################################################################################

V <- y %>% select(., datetime, V) 

# A matrix object simulating sampling scenarios
V_obs <- mx('dV')

V_obs <- V %>% cbind(., V_obs)

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

V_obs <- V_obs[ ,-2]


################################################################################################
# Rescale: storage data added with ET ('Q_et_obs') #############################################
# NOTE:We used daily discharge difference to derive discharge difference in a R-day timescale  #
################################################################################################

# A matrix object simulating sampling scenarios
Q_et_obs <- mx('dQ_et');

Q_et_obs <- Q %>% mutate(., dQ_et = dQ - ET) %>% select(., datetime, dQ_et) %>% cbind(., Q_et_obs)

# Aggregate daily discharge difference w/ ET to a unit of mcm per R days
for (i in 1:R){
  for (j in 1:((nrow(Q_et_obs) - i) %/% R)){
    a <- R*(j - 1) + i ; b <- j*R + i - 1;
    
    if(j == 1){
      Q_et_obs[i, i + 2] <- sum(Q_et_obs[1:(i-1), 2]);
      Q_et_obs[j*R + i, i + 2] <- sum(Q_et_obs[a:b, 2]);
    } 
    else{
      Q_et_obs[j*R + i, i + 2] <- sum(Q_et_obs[a:b, 2]);
      Q_et_obs[(a+1):b,i + 2] <- NA;
    }
  }
}

Q_et_obs <- Q_et_obs[ ,-2]

#############################################################################
# Simulating observed discharge differences by a SWOT's timescale ('Q_est') # 
#############################################################################

# We simulate the effects of sampling gaps and assume discharge differences on days without observations take on a linear distribution btw sampling days.
# A matrix object simulating sampling scenarios
Q_est <- mx('RdQ');
Q_est <- cbind(Q, Q_est)

# Assign discharge difference values in different sampling scenarios
for (i in 1:R){
  for (j in 1:nrow(Q_est)){
    if (!is.na(Q_est[j, i + 2])){
      Q_est[j, i + 2] <- Q_est[j, 2]
      }
    }
}

Q_est_m <- Q_est;

# Linearly interpolate the discharge differences on days without measurements
Q_est <- na_interpolation(Q_est, option = "linear") %>% select(., -dQ)

# Aggregate imputed discharge differences by a time period of R days
Q_est_2 <- mx('RdQ')
for (i in 1:R){
  for (j in 1:((nrow(Q_est) - i) %/% R)){
    a <- R*(j - 1) + i; b <- j*R + i - 1;
    
    if(j == 1){
      Q_est_2[i, i] <- sum(Q_est[1:(i-1), i + 1]);
      Q_est_2[j*R + i, i] <- sum(Q_est[a:b, i + 1]);
      Q_est_2[(a+1):b,i] <- NA;
      
    } 
    else{    
      Q_est_2[j*R + i, i] <- sum(Q_est[a:b, i + 1]);
      Q_est_2[(a+1):b, i] <- NA;
    }
  }
}

Q_est <- Q_est_2 %>%
  mutate(., datetime = Q_obs$datetime) %>% select(., datetime, everything())


####################
# Error Statistics #
####################

##############################################
# Error due to samping gaps, Q_obs vs. Q_est #
##############################################

# Create a data frame object 'sa_rr' of Relative Residuals (RR)
sa_rr <- data.frame(datetime = y$datetime, stringsAsFactors = FALSE);

rr <- vector()
for(i in 1:R){
  sa_rr[, i + 1] <- (Q_obs[, i + 1] - Q_est[, i + 1]) / Q_obs[, i + 1]
  for(j in 1:nrow(y)){
    if(!is.na(sa_rr[j, i + 1]))
      rr<- append(rr, sa_rr[j,i +1])
    }
}

# mean of RR (MRR or bias)
b_gap <- mean(rr)

# standard deviation of RR (SDRR)
sd_gap <- sd(rr)

# relative Root Mean Square Errors (rRMSE)
rrmse_gap <- sqrt(b_gap^2 + sd_gap^2)

# return values for each statistical metrics
b_gap; sd_gap; rrmse_gap

# Create a mutated dataframe object 'Q_mu' from Q_est and Q_obs
Q_est_mu <- Q_est %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>%
  # 'pivot_longer' is a pre-defined function from {tidyverse} that "lengthens" data, increasing the number of rows and decreasing the number of columns.
  na.omit()

Q_obs_mu <- Q_obs %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>% 
  select(., -2) %>%
  na.omit() # deleting rows containing NA

Q_mu <- left_join(Q_est_mu, Q_obs_mu, by="datetime"); 
names(Q_mu)[3:4] <- c('Q_est', 'Q_obs');

# Plot Q_est vs. Q_obs
# Define annotations of statistical metrics and their position on the plot 
anno <- paste0("MRR:",round(b_gap, 3), "\nSDRR:", round(sd_gap, 3),"\nrRMSE:", round(rrmse_gap, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0, gp=gpar(col="red", fontsize = 10, fontface="italic")))

# Set up the boundary on both dimensions
r_lim <- max(max(abs(range(Q_mu$Q_est))), max(abs(range(Q_mu$Q_obs))))

# Use 'ggplot' function to plot our results
sa_p <- ggplot(Q_mu, aes(x = Q_obs, y = Q_est, colour = variable)) + 
          geom_abline(intercept = 0, slope = 0, color = "black", lty = 2, alpha = 0.3) +
          geom_vline(xintercept = 0, color = "black", lty = 2, alpha = 0.3) +          
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.3) +
          xlim(-r_lim,r_lim) + ylim(-r_lim,r_lim) + annotation_custom(grob) +
          labs(title = paste0("Estimated Discharge Diff vs Observed Discharge Diff"), subtitle = paste0("R=", R, ", ", s_name), x = "Q_obs (million m3)", y = "Q_est (million m3)") +
          theme(plot.title = element_text(size = 11, hjust = 0.5),
                plot.subtitle = element_text(size = 9, hjust = 1),
                axis.title = element_text(size = 9.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.3, linetype = "solid"),
                axis.line = element_line(color = "black"))  


sa_plot <- sa_p + geom_point()

# return static and interactive plot
# sa_plot; #ggplotly(sa_plot)

# density plot with hollow circles 
sa_den_plot <- sa_p +
  geom_point(color="#DC143C", shape= 19, alpha = 0.15, size = 2.2) 

# return static and interactive plot
sa_den_plot; #ggplotly(sa_den_plot)

###################################
# Physical error, V_obs vs. Q_obs # 
###################################

######################
# w/o ET loss, Q_obs #
######################

# Create a data frame object 'phy_rr' of Relative Residuals (RR)
phy_rr <- data.frame(datetime = y$datetime, stringsAsFactors = FALSE);

rr <- vector()
for(i in 1:R){
  phy_rr[, i + 1] <- (V_obs[, i + 1] - Q_obs[, i + 1]) / V_obs[, i + 1];
  phy_rr[,i+1][!is.finite(phy_rr[,i+1])] <- NA
  for(j in 1:nrow(y)){
    if(!is.na(phy_rr[j, i + 1]))
      rr <- append(rr, phy_rr[j,i +1])
  }
}

# mean of RR (MRR or bias)
b_phy <- mean(rr)

# standard deviation of RR (SDRR)
sd_phy <- sd(rr)

# relative Root Mean Square Errors (rRMSE)
rrmse_phy <- sqrt(b_phy^2 + sd_phy^2)

# return values for each statistical metrics
b_phy; sd_phy; rrmse_phy

# Create a mutated dataframe object 'phy_mu' from V_obs and Q_obs
V_obs_mu <- V_obs %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>%
  # 'pivot_longer' is a pre-defined function from {tidyverse} that "lengthens" data, increasing the number of rows and decreasing the number of columns.
  na.omit()

Q_obs_mu <- Q_obs %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>% 
  select(., -2) %>%
  na.omit() # deleting rows containing NA

phy_mu <- left_join(V_obs_mu, Q_obs_mu, by="datetime"); 
names(phy_mu)[3:4] <- c('V_obs', 'Q_obs');

# Plot V_obs vs. Q_obs
# Define annotations of statistical metrics and their position on the plot 
anno <- paste0("MRR:",round(b_phy, 3), "\nSDRR:", round(sd_phy, 3),"\nrRMSE:", round(rrmse_phy, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))

# Set up the boundary on both dimensions
r_lim <- max(max(abs(range(phy_mu$V_obs))), max(abs(range(phy_mu$Q_obs))))

# Use 'ggplot' function to plot our results
phy_p <- ggplot(phy_mu, aes(x = Q_obs, y = V_obs, colour = variable)) + 
          geom_abline(intercept = 0, slope = 0, color = "black", lty = 2, alpha = 0.3) +
          geom_vline(xintercept = 0, color = "black", lty = 2, alpha = 0.3) +
          xlim(-r_lim,r_lim) + ylim(-r_lim,r_lim) + annotation_custom(grob) +
          labs(title = paste0("Observed Storage Variation vs Observed Discharge Diff"), subtitle = paste0("R=", R, ", ", s_name), x = "Q_obs (million m3)", y = "V_obs (million m3)") +
          theme(plot.title = element_text(size = 11, hjust = 0.5),
                plot.subtitle = element_text(size = 9, hjust = 1),
                axis.title = element_text(size = 9.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.3, linetype = "solid"),
                axis.line = element_line(color = "black"))  +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.3)

phy_plot <- phy_p + geom_point()

# return static and interactive plot
# phy_plot; #ggplotly(phy_plot)

# density plot as circles with no fill
phy_den_plot <- phy_p +
  geom_point(color="seagreen", shape= 19, alpha = 0.3, size = 2.2) 

# return static and interactive plot
phy_den_plot; #ggplotly(phy_den_plot)

########################
# w/ ET loss, Q_et_obs #
########################

# Create a data frame object 'phy_rr_et' of Relative Residuals (RR)
phy_rr_et <- data.frame(datetime = y$datetime, stringsAsFactors = FALSE);

rr <- vector()
for(i in 1:R){
  phy_rr_et[, i + 1] <- (V_obs[, i + 1] - Q_et_obs[, i + 1])/V_obs[, i + 1] 
  phy_rr_et[,i+1][!is.finite(phy_rr_et[,i+1])] <- NA
  for(j in 1:nrow(y)){
    if(!is.na(phy_rr_et[j, i + 1]))
      rr <- append(rr, phy_rr_et[j,i +1])
  }
}

# mean of RR (MRR or bias)
b_phy_et <- mean(rr)

# standard deviation of RR (SDRR)
sd_phy_et <- sd(rr)

# relative Root Mean Square Errors (rRMSE)
rrmse_phy_et <- sqrt(b_phy_et^2 + sd_phy_et^2)

# return values for each statistical metrics
b_phy_et; sd_phy_et; rrmse_phy_et

# Create a mutated dataframe object 'phy_mu_et' from V_et_obs and Q_obs
V_obs_mu <- V_obs %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>%
  # 'pivot_longer' is a pre-defined function from {tidyverse} that "lengthens" data, increasing the number of rows and decreasing the number of columns.
  na.omit()

Q_et_obs_mu <- Q_et_obs %>%
  pivot_longer(-datetime, names_to = "variable", values_to = "value") %>% 
  select(., -2) %>%
  na.omit() # deleting rows containing NA

phy_mu_et <- left_join(V_obs_mu, Q_et_obs_mu, by="datetime"); 
names(phy_mu_et)[3:4] <- c('V_obs', 'Q_et_obs');

# Plot V_et_obs vs. Q_obs
# Define annotations of statistical metrics and their position on the plot 
anno <- paste0("MRR:",round(b_phy_et, 3), "\nSDRR:", round(sd_phy_et, 3),"\nrRMSE:", round(rrmse_phy_et, 3))
grob <- grobTree(textGrob(anno, x=0.1,  y=0.9, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))

# Set up the boundary on both dimensions
r_lim <- max(max(abs(range(phy_mu_et$V_obs))), max(abs(range(phy_mu_et$Q_et_obs))))

# Use 'ggplot' function to plot our results
phy_et_p <- ggplot(phy_mu_et, aes(x = Q_et_obs, y = V_obs, colour = variable)) + 
              geom_abline(intercept = 0, slope = 0, color = "black", lty = 2, alpha = 0.3) +
              geom_vline(xintercept = 0, color = "black", lty = 2, alpha = 0.3) +
              xlim(-r_lim,r_lim) + ylim(-r_lim,r_lim) + 
              annotation_custom(grob) +
              labs(title = paste0("Observed Storage Variation&ET vs Observed Discharge Diff"), subtitle = paste0("R=", R, ", ", s_name), x = "Q_et_obs (million m3)", y = "V_obs (million m3)") + 
              theme(plot.title = element_text(size = 10.5, hjust = 0.5),
                    plot.subtitle = element_text(size = 9, hjust = 1),
                    axis.title = element_text(size = 9.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.3, linetype = "solid"),
                    axis.line = element_line(color = "black"))  +
              geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.3)

phy_et_plot <- phy_et_p + geom_point()

# return static and interactive plot
#phy_et_plot; #ggplotly(phy_et_plot)

# density plot as circles with no fill
phy_et_den_plot <- phy_et_p +
  geom_point(color="#66C2A5", shape= 19, alpha = 0.3, size = 2.2) 

# return static and interactive plot
phy_et_den_plot; #ggplotly(phy_et_den_plot)


######################################################################################
# density plot with hexbin chart
# phy_et_den_plot <- ggplot(phy_mu_et, aes(x = Q_obs, y = V_et_obs)) + 
#  geom_abline(intercept = 0, slope = 0, color = "black", lty = 2) +
#  geom_vline(xintercept = 0, color = "black", lty = 2) +
#  geom_hex(bins = 140) +
#  scale_fill_gradientn(colours=c("blue","red"), 
#                       name = "Frequency", 
#                       na.value=NA) +
#  theme_bw() + 
#  xlim(-r_lim,r_lim) + ylim(-r_lim,r_lim) + 
#  annotation_custom(grob) +
#  labs(title = paste0("V_et_obs vs. Q_obs for ",s_name), x = "Q_obs", y = "V_et_obs")

# density plot with 2d Histogram 
#phy_et_den_plot <- ggplot(phy_mu_et, aes(x = Q_obs, y = V_et_obs)) + 
#  geom_abline(intercept = 0, slope = 0, color = "black", lty = 2) +
#  geom_vline(xintercept = 0, color = "black", lty = 2) +
#  geom_bin2d(bins = 220) +
#  scale_fill_gradientn(colours=c("blue","red"), 
#                       name = "Frequency", 
#                       na.value=NA) +
#  theme_bw() + 
#  xlim(-r_lim,r_lim) + ylim(-r_lim,r_lim) + 
#  annotation_custom(grob) +
#  labs(title = paste0("V_et_obs vs. Q_obs for ",s_name), x = "Q_obs", y = "V_et_obs")
######################################################################################


############################
# Monthly level hydrograph#
############################

####################
# Observed Q and V #
####################

# Daily to Monthly data Manipulation:
# rescale in situ discharge difference and storage data from daily to monthly 
QV_obs_mon <- y %>% select(., datetime, dQ, V) %>%  mutate(., dV_m = NA, dQ_m = NA); 

QV_obs_mon <- separate(QV_obs_mon, 1, c("yr", "mon", "day"), convert = T); last_v <- NA

for (i in start_yr:end_yr){
 for (j in min(QV_obs_mon[QV_obs_mon$yr == i,]$mon):max(QV_obs_mon[QV_obs_mon$yr == i,]$mon)){
   
    # daily storage measurements to monthly storage variations
    QV_obs_mon[QV_obs_mon$yr== i & QV_obs_mon$mon == j,][1,6] <- QV_obs_mon$V[QV_obs_mon$yr == i & QV_obs_mon$mon == j & QV_obs_mon$day == 1] - last_v
    
    # discharge difference
    if (j < 12){
      QV_obs_mon[QV_obs_mon$yr== i & QV_obs_mon$mon == j + 1,][1,7] <- sum(QV_obs_mon$dQ[QV_obs_mon$yr == i & QV_obs_mon$mon == j], na.rm = T)
      
    } else{
      QV_obs_mon[QV_obs_mon$yr== i + 1 & QV_obs_mon$mon == 1,][1,7] <- sum(QV_obs_mon$dQ[QV_obs_mon$yr == i & QV_obs_mon$mon == j], na.rm = T)
    }
    
    last_v <- QV_obs_mon$V[QV_obs_mon$yr == i & QV_obs_mon$mon == j & QV_obs_mon$day == 1]   
  }
}

QV_obs_mon <- QV_obs_mon %>% unite("datetime", c("yr","mon", "day"), sep = "-"); QV_obs_mon$datetime <- as.Date(QV_obs_mon$datetime)

QV_obs_mon <- left_join(QV_obs_mon, et_mon, by = c("datetime"="Month")) %>% mutate(., dQ_et_m = dQ_m - ET) %>% select(., -c(dQ, V, ET))

obs_mon <- na_interpolation(QV_obs_mon) %>% select(., - datetime)

###############
# Estimated Q #
###############

# Linearly interpolate the discharge differences on days without measurements
Q_est_m <- na_interpolation(Q_est_m, option = "linear") %>% select(., -dQ)

Q_est_m <- separate(Q_est_m, 1, c("yr", "mon", "day"), convert = T); 

x <- data.frame();
n <- data.frame(datetime = y$datetime)
indices <- which(Q_est_m$day == 1)

for (i in start_yr:end_yr){
  for (j in min(Q_est_m[Q_est_m$yr == i,]$mon):max(Q_est_m[Q_est_m$yr == i,]$mon)){
    x <-rbind(x,apply(Q_est_m[Q_est_m$yr == i & Q_est_m$mon == j, 4:(3+R)], FUN = sum, MARGIN = 2))
  }
}

for(k in 1:(length(indices)-1)){
  n[indices[k+1],2:(R+1)]<- x[k,]
}

names(n) <- names(Q_est)
est_mon <- na_interpolation(n) %>% select(., - datetime)

# Plot hydrograph showing uncertainty as lines
obs_mon <- xts(obs_mon, order.by = y$datetime)
est_mon <- xts(est_mon, order.by = y$datetime)
mon_hy <- merge(obs_mon, est_mon)

dygraph(mon_hy, main = paste0("Hydrograph showing monthly time series for ", s_name, " (sampling gap: ", R, " days)"), ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>% dyLegend(width = 600) %>%  
  dyGroup(c(paste0(c("RdQ"), 1:R)), color = rep("#C0C0C0", R))  %>%
  dySeries("dV_m", color = "#DC143C",strokeWidth = 1, label = "Observed dV") %>% 
  dySeries("dQ_m", color = "seagreen",strokeWidth = 1, label = "Observed dQ") %>%
  dySeries("dQ_et_m", color = "#66C2A5",strokeWidth = 1, label = "dQ corrected by surface evaporation") 


mcm_R_cms <- 10^6/24/3600/R
mon_hy_cms <- mon_hy*mcm_R_cms

dygraph(mon_hy_cms, main = paste0("Hydrograph showing monthly time series for ", s_name, " (sampling gap: ", R, " days)"), ylab = "m^3/s") %>% 
#  dyRangeSelector() %>% 
  dyLegend(width = 600, show = "onmouseover") %>% 
  dyGroup(c(paste0(c("RdQ"), 1:R)), color = rep("#C0C0C0", R))  %>%
  dySeries("dV_m", color = "#DC143C",strokeWidth = 1, label = "Observed dV") %>% 
  dySeries("dQ_m", color = "seagreen",strokeWidth = 1, label = "Observed dQ") %>%
  dySeries("dQ_et_m", color = "#66C2A5",strokeWidth = 1, label = "dQ corrected by surface evaporation") %>%
  dyAnnotation(x=paste0(end_yr, "-04-05"), text="dV", series = "Observed dV", width = 20, attachAtBottom = T) %>%
  dyAnnotation(x=paste0(end_yr, "-05-05"), text="dQ", series = "Observed dQ", width = 20, attachAtBottom = T) %>%
  dyAnnotation(x=paste0(end_yr, "-06-15"), text="dQ+ET", series = "dQ corrected by surface evaporation", width = 45, attachAtBottom = T)

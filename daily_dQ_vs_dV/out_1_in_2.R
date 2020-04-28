#install.packages(c("dplyer", "tidyverse","ggplot2"))
library("dplyr")
library("rio")
library("tidyverse")
library("readxl") #read excel files out_1_in_2
library("xts")

s_name <- "Wilson Lake_19_2in" # sample name

#cleaning spreadsheets

##convert multiple sheets from Excel into one sheet
y <- excel_sheets(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_2/", s_name, ".xlsx")) %>% 
  map(~read_xlsx(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_2/", s_name, ".xlsx"),.)) %>%
  data.frame()

##tidy data
n = c("V", "Q_out", "Q_in1", "Q_in2")
m = c("site_V", "site_out", "site_in1", "site_in2")

tidy <- function(s){ 
  s <- s %>%
    select(., datetime, contains("site_no"), ends_with(c("32400","30800")), contains("00003"),-contains("cd")) %>%
    filter(., site_no != "15s") %>%
    rename_at(vars(c(6,7,8,9)), ~ n) %>%
    rename_at(vars(contains("site")), ~ m) 
  
  s$datetime <- as.Date(as.numeric(s$datetime), origin = "1899-12-30")
  
  return(s)
}

y <- tidy(y)
View(y)


##convert xlsx to csv
##convert("Possum Kingdom Lk_09_10.xlsx","Possum Kingdom Lk_09_10.csv")

#dV vs dQ

##Unit Conversion
convert_af_mcm = 1233.48/10.0^6 #convert from acre feet to million m3
convert_cfs_mcmd = 3600.0*24.0*0.0283168/10.0^6 #convert from cubic feet per second to million m3 per day;
y$V <- as.numeric(y$V) * convert_af_mcm; 
y$Q_out <- as.numeric(y$Q_out) * convert_cfs_mcmd; 
y$Q_in1 <- as.numeric(y$Q_in1) * convert_cfs_mcmd; 
y$Q_in2 <- as.numeric(y$Q_in2) * convert_cfs_mcmd; 

##calculate mass balance
dV <- y$V %>%
  append(.,NA, 0); length(dV) <- nrow(y); dV <- y$V - dV

y <- y %>%
  mutate(.,dQ = Q_in1 + Q_in2 - Q_out) %>% 
  cbind(., dV)

#figure dQ vs dV
range_limit <- max(abs(min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE))), 
                   abs(max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))))

ggplot(y) +
  geom_point(mapping = aes(x = dQ, y = dV), 
             color="darkgreen", shape= 17, 
             alpha = 1/2, size = 3, na.rm = TRUE) +
  labs(title ="Daily dQ vs dV", 
       x = "dQ (m^3)", 
       y = "dV (m^3)") +
  xlim(-range_limit, range_limit) +
  ylim(-range_limit, range_limit) +
  geom_segment(aes(x = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)), 
                   y = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)), 
                   xend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE)), 
                   yend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))
  ),
  linetype = "dashed"
  )

#correlation
cor(y$dV, y$dQ, use = "complete.obs")


ggplot(y) +
  geom_point(mapping = aes(x = dQ, y = dV), 
             color="darkgreen", shape= 17, 
             alpha = 1/2, size = 3, na.rm = TRUE) +
  labs(title ="Daily dQ vs dV", 
       x = "dQ (m^3)", 
       y = "dV (m^3)") +
  xlim(-3, 3) +
  ylim(-5, 5) +
  geom_segment(aes(x = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)), 
                   y = min(min(y$dV, na.rm = TRUE), min(y$dQ,na.rm = TRUE)), 
                   xend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE)), 
                   yend = max(max(y$dV,na.rm = TRUE), max(y$dQ, na.rm = TRUE))
  ),
  linetype = "dashed"
  )
###############################################################################################################
###############################################################################################################
###############################################################################################################
# Uncertainty Analysis
R <- 10 # sampling gap/ temporal resolution

N <- rep(NA, R-1)
dy <- y %>%
  select(., datetime, dQ, dV)
mx <- matrix(nrow = nrow(dy))

# y is the results we have 
for (i in 1:R) {
  # create a sequence of days that are sampled
  r <- rbind(seq(from = i, to = nrow(y), by = R), matrix(ncol = length(seq(from = i, to = nrow(y), by = R)), nrow = R-1)) # adding R-1 rows of NA of sequence
  
  r_NA <- c(rep(NA,i-1), r) # adding NA before the first value from the vector
  length(r_NA) <- nrow(y) # length correction 
  
  mx <- as.data.frame(cbind(mx, r_NA)); names(mx)[i+1] <- paste0(c("dQ"), i)
}
mx <- subset(mx, select = -V1) # dropping defaut column from df


###############################################################################################################
# joining sampling days (mx) to the dy (here we are testing on t)
mx <- zoo(mx, dy$datetime) # convert mx to time series object

t <- dy  %>% xts(., order.by = .$datetime) %>% subset(., select = -datetime)


day <- seq(from = 1, to = nrow(dy)); t <- cbind(t, day)  # adding # of days as a new column


t <- merge(t, mx)

# adding values for sampling days in different scenarios
for (i in 1:R){
  
  for (j in 1:nrow(t)){
    
    if (!is.na(t[j, i+3]))
      t[j, i+3] <- t[j, 1]
    
  }
}


# interpolate
t <- na_interpolation(t, option = "linear") %>%
  subset(., select = -day)


t_error <- t %>%
  subset(., select = -c(dQ,dV)) %>%
  fortify.zoo() %>%
  select(-Index)

t_error$max <- apply(X = t_error, MARGIN=1, FUN=max)
t_error$min <- apply(X = t_error, MARGIN=1, FUN=min)

t_error <- t_error %>%
  select(max, min) %>%
  zoo(., order.by = dy$datetime)

t_2 <- t %>%
  subset(., select= c(dQ,dV)) %>%
  merge(., t_error)

# Plot with upper/lower bars
# dygraph(t_2, 
#        main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, "days)"),
#        ylab = "Storage change (km^3 / day)") %>% 
#  dyRangeSelector() %>%
#  dySeries(c("min", "dQ", "max"), label = "Observed dQ", color = "red") %>%
#  dySeries("dV", label = "Observed dV", color = "seagreen") %>%
#  dyHighlight(highlightCircleSize = 5, 
#              highlightSeriesBackgroundAlpha = 0.2,
#              hideOnMouseOut = TRUE)

# Plot showing uncertainty as lines
dygraph(t, 
        main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, "days)"),
        ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>%
  dySeries("dV", color = "seagreen") %>%
  dySeries("dQ", color = "red") %>%
  dyGroup(c(paste0(c("dQ"), 1:R)), color = rep("pink", R)) %>%
  dyLegend(width = 600) 


#install.packages(c("dplyer", "tidyverse","ggplot2"))
library("dplyr")
library("rio")
library("tidyverse")
library("readxl") 

s_name <- "Lake Melwin - Yale Lake - Swift Reservoir_15_16" # sample name

#cleaning spreadsheets

##convert multiple sheets from Excel into one sheet
y <- excel_sheets(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/Multiple_lakes/", s_name, ".xlsx")) %>% 
  map(~read_xlsx(paste0("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/Multiple_lakes/", s_name, ".xlsx"),.)) %>%
  data.frame()


##tidy data
n = c("V1","V2","V3", "Q_out", "Q_in1", "Q_in2", "Q_in3", "Q_in4")
m = c("site_V1", "site_V2", "site_V3", "site_out", "site_in1", "site_in2", "site_in3", "site_in4")

tidy <- function(s){ 
  s <- s %>%
    select(., datetime, contains("site_no"), ends_with(c("32400","30800")), contains("00003"),-contains("cd")) %>%
    filter(., site_no != "15s") %>%
    rename_at(vars(c(10:17)), ~ n) %>%
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
y$V1 <- as.numeric(y$V1) * convert_af_mcm; 
y$V2 <- as.numeric(y$V2) * convert_af_mcm; 
y$V3 <- as.numeric(y$V3) * convert_af_mcm; 

y$Q_out <- as.numeric(y$Q_out) * convert_cfs_mcmd; 
y$Q_in1 <- as.numeric(y$Q_in1) * convert_cfs_mcmd; 
y$Q_in2 <- as.numeric(y$Q_in2) * convert_cfs_mcmd; 
y$Q_in3 <- as.numeric(y$Q_in3) * convert_cfs_mcmd; 
y$Q_in4 <- as.numeric(y$Q_in4) * convert_cfs_mcmd; 


##calculate mass balance
dV1 <- y$V1 %>%
  append(.,NA, 0); length(dV1) <- nrow(y); dV1 <- y$V1 - dV1
dV2 <- y$V2 %>%
  append(.,NA, 0); length(dV2) <- nrow(y); dV2 <- y$V2 - dV2
dV3 <- y$V3 %>%
  append(.,NA, 0); length(dV3) <- nrow(y); dV3 <- y$V3 - dV3

dV <- dV1 + dV2 +dV3

y <- y %>%
  mutate(.,dQ = Q_in1 + Q_in2 + Q_in3 + Q_in4 - Q_out) %>% cbind(., dV)

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
###############################################################################################################
###############################################################################################################
###############################################################################################################
# Uncertainty Analysis
R <- 10 # sampling gap/ temporal resolution
N <- rep(NA, R-1)
dy <- y %>% select(., datetime, dQ, dV)

## A loop to create a matrix of sampling days in different scenarios
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
### dropping defaut column from df
mx <- subset(mx, select = -V1) # View(mx)
###############################################################################################################
# joining sampling days (mx) to the dy (here we are testing on t)
## convert mx to time series object
mx <- zoo(mx, dy$datetime) 
dy <- dy  %>% xts(., order.by = .$datetime) %>% subset(., select = -datetime)
## adding # of days as a new column
day <- seq(from = 1, to = nrow(dy)); dy <- cbind(dy, day)  
dy <- merge(dy, mx)

## adding values for sampling days columns in different scenarios
for (i in 1:R){
  for (j in 1:nrow(dy)){
    if (!is.na(dy[j, i+3]))
      dy[j, i+3] <- dy[j, 1]
  }
}

## interpolate in substitue of NA values
dy <- na_interpolation(dy, option = "linear") %>% subset(., select = -day)

## upper and lower error bars
dy_error <- dy %>% subset(., select = -c(dQ,dV)) %>% fortify.zoo() %>% select(-Index)

### calculation highest and lowest values for each scenarios/row
dy_error$max <- apply(X = dy_error, MARGIN=1, FUN=max)
dy_error$min <- apply(X = dy_error, MARGIN=1, FUN=min)

### dy_2 
dy_error <- dy_error %>% select(max, min) %>% zoo(., order.by = y$datetime)
dy_2 <- dy %>% subset(., select= c(dQ,dV)) %>% merge(., dy_error)
###############################################################################################################
# Plot with upper/lower bars
# dygraph(dy_2, 
#        main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, "days)"),
#        ylab = "Storage change (km^3 / day)") %>% 
#  dyRangeSelector() %>%
#  dySeries(c("min", "dQ", "max"), label = "Observed dQ", color = "red") %>%
#  dySeries("dV", label = "Observed dV", color = "seagreen") %>%
#  dyHighlight(highlightCircleSize = 5, 
#              highlightSeriesBackgroundAlpha = 0.2,
#              hideOnMouseOut = TRUE)

# Plot showing uncertainty as lines
dygraph(dy, 
        main = paste0("Uncertainty Analysis for SWOT Observations of ", s_name, " (Sampling Gap: ", R, "days)"),
        ylab = "Storage change (km^3 / day)") %>% 
  dyRangeSelector() %>%
  dySeries("dV", color = "seagreen") %>%
  dySeries("dQ", color = "red") %>%
  dyGroup(c(paste0(c("dQ"), 1:R)), color = rep("pink", R)) %>%
  dyLegend(width = 600) 

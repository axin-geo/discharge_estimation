#install.packages(c("dplyer", "tidyverse","ggplot2", "xts"))
library("dplyr")
library("rio")
library("tidyverse")
library("readxl") #read excel files #only 1 Q_in

#cleaning spreadsheets

##convert multiple sheets from Excel into one sheet
y <- excel_sheets("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/MILFORD LK_13_14.xlsx") %>% 
  map(~read_xlsx("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_1_in_1/MILFORD LK_13_14.xlsx",.)) %>%
  data.frame()

##tidy data
n = c("V", "Q_out", "Q_in")
m = c("site_V", "site_out", "site_in")

tidy <- function(s){ 
  s <- s %>%
    select(., datetime, contains("site_no"), ends_with(c("32400","30800", "30600")), contains("00003"),-contains("cd")) %>%
    filter(., site_no != "15s") %>%
    rename_at(vars(c(5,6,7)), ~ n) %>%
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
y$Q_in <- as.numeric(y$Q_in) * convert_cfs_mcmd; 

##calculate mass balance
dV <- y$V %>%
  append(.,NA, 0); length(dV) <- nrow(y); dV <- y$V - dV

y <- y %>%
  mutate(.,dQ = Q_in - Q_out) %>% cbind(., dV)

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


# SWOT time scale

# Uncertainty Analysis



gap <- 7 # sampling gap/ temporal resolution

u1 <- seq(from = 1, to = nrow(y), by = gap)
u2 <- seq(from = 2, to = nrow(y), by = gap)
u3 <- seq(from = 3, to = nrow(y), by = gap)
u4 <- seq(from = 4, to = nrow(y), by = gap)
u5 <- seq(from = 5, to = nrow(y), by = gap)
u6 <- seq(from = 6, to = nrow(y), by = gap)
u7 <- seq(from = 7, to = nrow(y), by = gap)

a <- 1
N <- rep(NA, 6)
# adding NA to u. starting from 1
r <- rbind(u1, matrix(ncol = length(u1), nrow = 6))
r1_NA <- c(rep(NA,1-1),r)
length(r_NA) <- nrow(y)

# u2
r2 <- rbind(u2, matrix(ncol = length(u2), nrow = 6))
r2_NA <- c(rep(NA,2-1), r2)
length(r2_NA) <- nrow(y)

# u3
r3 <- rbind(u3, matrix(ncol = length(u3), nrow = 6))
r3_NA <- c(rep(NA,3-1), r3)
length(r3_NA) <- nrow(y)

# u4
r4 <- rbind(u4, matrix(ncol = length(u4), nrow = 6))
r4_NA <- c(rep(NA,4-1), r4)
length(r4_NA) <- nrow(y)

# joining u to the y

t <- y  
t_1 <- zoo(as.numeric(t$dQ), t$datetime)
# View(t_1)
row <- seq(from = 1, to = nrow(y))
t_1 <- cbind(t_1, row)  # adding rownumber as a new column


ct <- zoo(r1_NA, t$datetime)
# View(ct)

t_row1 <- merge(t_1, ct)
# View(t_row1)
t_in <- na.approx(t_row1)




library(zoo)
library(xts)

z <- zoo(c(2,NA,1,4,5,2), c(1,3,4,6,7,8))

## use underlying time scale for interpolation
na.approx(z) 
  
t <- zoo(t, order.by = t$datetime)
na.approx(t) 



library(imputeTS)
# set original data as TS file 


# assign NA to non-7-multiple


gap2 <- seq(from = 2, to = nrow(y), by = gap)
w <- data.frame(c(NA, gap2, NA))
names(w)[1] <- "w"




# interpolate NA

x[c(1:6,11)]<- NA
x <- na_interpolation(x, option = "linear")



## get every column out 



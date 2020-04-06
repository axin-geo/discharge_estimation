#install.packages(c("dplyer", "tidyverse","ggplot2"))
library("dplyr")
library("rio")
library("tidyverse")
library("readxl") #read excel files #only 1 Q_in

#cleaning spreadsheets

##convert multiple sheets from Excel into one sheet
y <- excel_sheets("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_substraction/Lake Livingston_14_15.xlsx") %>% 
  map(~read_xlsx("C:/Users/axin/OneDrive - Kansas State University/SWOT_from_Aote/raw_data_by_num_of_streams/out_substraction/Lake Livingston_14_15.xlsx",.)) %>%
  data.frame()

##tidy data
n = c("V", "Q1", "_Q2","Q_in")
m = c("site_V", "site_out_1","site_out_2", "site_in")

tidy <- function(s){ 
  s <- s %>%
    select(., datetime, contains("site_no"), ends_with(c("32400","30800")), contains("00003"), -contains("cd")) %>%
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
y$Q1 <- as.numeric(y$Q1) * convert_cfs_mcmd; 
y$`_Q2` <- as.numeric(y$`_Q2`) * convert_cfs_mcmd; 
y$Q_in <- as.numeric(y$Q_in) * convert_cfs_mcmd; 

##calculate mass balance
dV <- y$V %>%
  append(.,NA, 0); length(dV) <- nrow(y); dV <- y$V - dV

y <- y %>%
  mutate(.,dQ = Q_in - Q1 + `_Q2`) %>% cbind(., dV)

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
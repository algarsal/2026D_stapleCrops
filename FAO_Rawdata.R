#try to run the URL of: Rice, Wheat, Corn, Potato, Soy_bean, Sugar_cane
library(dplyr)

#0# Selected countires
countires <- c("Armenia", "Azerbaijan", "Bangladesh", "India", "Indonesia", "Iraq", "Japan", "Kazakhstan", "Lebanon", "Malaysia", "Nepal", "Pakistan", "Quatar", "Saudi Arabia", "Sri Lanka", "Tajikistan", "Thailand")
years <- 1990:2020
#year_start <- 1990
#year_stop <- 2020
#1# Corn
  Corn_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Corn_Raw_FAO.csv")
  #1.a# remove countries we do not need
   Corn_country_clean <- Corn_Raw %>% filter(Country %in% countires)
   View(Corn_country_clean)
  #1.b# sellect years 1990 until 2020
   Corn_year_clean <- Corn_country_clean %>% filter(Year %in% years)
#2# Potato
   Potatoe_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Potatoe_Raw_FAO.csv")
     #2.a# remove countries we do not need
     Potatoe_country_clean <- Potatoe_Raw %>% filter(Country %in% countires)
#3# Rice
     Rice_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Rice_Raw_FAO.csv")
     #3.a# remove countries we do not need
     Rice_country_clean <- Rice_Raw %>% filter(Country %in% countires)
#4# Soy Bean
     Soy_Bean_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Soy_bean_Raw_FAO.csv")
     #4.a# remove countries we do not need
     Soy_Bean_country_clean <- Soy_Bean_Raw %>% filter(Country %in% countires)
#5# Sugarcane 
     Sugarcane_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Sugar_Cain_Raw_FAO.csv")
     #5.a# remove countries we do not need
     Sugarcane_country_clean <- Sugarcane_Raw %>% filter(Country %in% countires)
#6# Wheat
     Wheat_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Wheat_Raw_FAO.csv")
     #6.a# remove countries we do not need
     Wheat_country_clean <- Wheat_Raw %>% filter(Country %in% countires)
     
     #not finished, frist clean up of the crops, by counztry and year.
     


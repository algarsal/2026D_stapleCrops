#try to run the URL of: Rice, Wheat, Corn, Potato, Soy_bean, Sugar_cane
library(dplyr)
#A#
#0# Selected countires
countires <- c("Armenia", "Azerbaijan", "Bangladesh", "India", "Indonesia", "Iraq", "Japan", "Kazakhstan", "Lebanon", "Malaysia", "Nepal", "Pakistan", "Quatar", "Saudi Arabia", "Sri Lanka", "Tajikistan", "Thailand")
years <- 1990:2020
#year_start <- 1990
#year_stop <- 2020

#1# Corn
Corn_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Corn_Raw_FAO.csv")
#1.a# remove countries we do not need
Corn_country_clean <- Corn_Raw %>% filter(Country %in% countires)
#View(Corn_country_clean)
#1.b# sellect years 1990 until 2020
Corn_year_clean <- Corn_country_clean %>% filter(Year %in% years)

#2# Potato
Potatoe_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Potatoe_Raw_FAO.csv")
#2.a# remove countries we do not need
Potatoe_country_clean <- Potatoe_Raw %>% filter(Country %in% countires)
#2.b# sellect years 1990 until 2020
Potatoe_year_clean <- Potatoe_country_clean %>% filter(Year %in% years)

#3# Rice
Rice_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Rice_Raw_FAO.csv")
#3.a# remove countries we do not need
Rice_country_clean <- Rice_Raw %>% filter(Country %in% countires)
#3.b# sellect years 1990 until 2020
Rice_year_clean <- Rice_country_clean %>% filter(Year %in% years)

#4# Soy Bean
Soy_Bean_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Soy_bean_Raw_FAO.csv")
#4.a# remove countries we do not need
Soy_Bean_country_clean <- Soy_Bean_Raw %>% filter(Country %in% countires)
#4.b# sellect years 1990 until 2020
Soy_Bean_year_clean <- Soy_Bean_country_clean %>% filter(Year %in% years)

#5# Sugarcane 
Sugarcane_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Sugar_Cain_Raw_FAO.csv")
#5.a# remove countries we do not need
Sugarcane_country_clean <- Sugarcane_Raw %>% filter(Country %in% countires)
#5.b# sellect years 1990 until 2020
Sugarcane_year_clean <- Sugarcane_country_clean %>% filter(Year %in% years)

#6# Wheat
Wheat_Raw <- read.csv("C:/Users/koeni/Desktop/Data_Semester.1_CSV_Files_FAO/Wheat_Raw_FAO.csv")
#6.a# remove countries we do not need
Wheat_country_clean <- Wheat_Raw %>% filter(Country %in% countires)
#6.b# sellect years 1990 until 2020
Wheat_year_clean <- Wheat_country_clean %>% filter(Year %in% years)



# Combination of all crops:
all_crops <- bind_rows(
  Corn_year_clean,
  Potatoe_year_clean,
  Rice_year_clean,
  Soy_Bean_year_clean,
  Sugarcane_year_clean,
  Wheat_year_clean
) %>%
  arrange(Item, Year)

#View(all_crops)
#########




#########
#B_0# opening WDI to introduce variables: GDP...
  
  install.packages("WDI")
library(WDI)

#B_1# define indicators of World Bank (give them usable names)
  wdi_variables <- c(
    "GDP_per_capita" = "NY.GDP.PCAP.CD", #in US$
    "Population" = "SP.POP.TOTL",#citizens number
    "Life_expectancy" = "SP.DYN.LE00.IN", #life expectancy
    "Secondary_education_LV" = "SE.SEC.ENRR" #enrollment in secondary school
  )
  #B_2#
  wdi_data <- WDI(
    country   = c("AM", "AZ", "BD", "IN", "ID", "IQ", "JP", "KZ", "LB", 
                  "MY", "NP", "PK", "QA", "SA", "LK", "TJ", "TH"),
    indicator = wdi_variables,
    start     = min(years),
    end       = max(years)
  ) %>%
    mutate(country = ifelse(country == "Qatar", "Quatar", country)) %>% # adding ISO2 Code to the countries (ensuring intact variables), to prevent different spelling of country names or mistakes
    select(country, year, GDP_per_capita, Population, Life_expectancy, Secondary_education_LV)
  
  # 3. Join WDI columns onto your existing 'all_crops' data frame
  all_crops_WDI <- all_crops %>%
    left_join(wdi_data, by = c("Country" = "country", "Year" = "year"))
  
  #View(all_crops_WDI)

# removing the 3 variables form "all_crops" ['faostat', 'm49.code', 'Item.code'], as they don't serve any higher function in our data!
  cleaned_crops_WDI <- all_crops_WDI %>%
    select(-faostat, -m49.code, -Item.Code)
#View(cleaned_crops_WDI)
  
#########

#C#
#table has to be ordered properly to ensure better overview
#using tidyr. Each row is Country and year and columns always variables (yield, area...)
  library(tidyr)

#C_1# Replacing column name 'Value' with name of yield column.
FAO_WDI_ordered_data <- cleaned_crops_WDI %>%
 
   select(
     Country, 
     Year, 
     Item, 
     Yield = Yield..kg.ha., 
     Production = Production..tonnes.,
     GDP_per_capita, 
     Population, 
     Life_expectancy, 
     Secondary_education_LV) %>% #this only selects metric variables, we thereby remove 'flag' columns (which only informs us on how the data was obtained)
  
#C_2# replacing spaces with '_', easier to read (for program)
  mutate(Item = gsub(" ", "_", Item)) %>%
  
#C_3# the crops are moved into eparate columns
  pivot_wider(
    names_from  = Item,
    values_from = c(Yield, Production),
    names_glue = "{Item}_{.value}" # new column name after this scheme 
  ) %>%
  arrange(Country, Year)

#View(FAO_WDI_ordered_data)




#####
#Correlation between the variables#

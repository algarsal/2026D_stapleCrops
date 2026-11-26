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
  
  #install.packages("WDI")
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

write.table(FAO_WDI_ordered_data, file = "CSV_Files_FAO/FAO_WDI_ordered_data.csv")

 
#View(FAO_WDI_ordered_data)




#####
#Correlation between the variables#

#D_0# correlation between:
    #excluding non numerical variables (Country and Year), by creating Values with Variables we exclude
D_0_wdi_variables <- c("GDP_per_capita", "Population", "Life_expectancy", "Secondary_education_LV")

D_0_crop_variables <- FAO_WDI_ordered_data %>%
  select(-Country, -Year, -all_of(D_0_wdi_variables)) %>%
  names()


#D_1# GDP_per_capita and 'Crop'
    #only GDP--> Pearson's correlation GDP and crops Yield, Production 
D.1_gdp_crop_correlation <- tibble(Crop_Variables = D_0_crop_variables) %>%
  rowwise() %>% # setting up looped workflow, inside new table (D.1_gdp_crop_correlation), with one column for each variable (Rice_Yield/Production, Corn_Yield/...)
  mutate(       #adding new columns with the following names (r, p_value...)
    test      = list(cor.test(FAO_WDI_ordered_data$GDP_per_capita, 
                              FAO_WDI_ordered_data[[Crop_Variables]], 
                              method = "pearson")),
    r = round(test$estimate, 2),              #takes the result for 'r' from the actual test (test), and rounding it up to 2 decimals 
    CI_95_low = round (test$conf.int[1], 2),  #confidence interval, if it croses 0, we seethat this variable does not linearly predict the yield (variable)
    CI_95_high = round(test$conf.int [2], 2), #confidence interval for high relation
    p_value = round(test$p.value, 4),         #round p_value to 4 decimals 
    Signif = case_when(
      p_value < 0.001 ~ "high Sig.",
      p_value < 0.01  ~ "medium Sig.",
      p_value < 0.05  ~ "low Sig.",
      TRUE            ~ "no Sig."
    )
  ) %>%
  ungroup() %>% #sort and "clean" results
select(Crop_Variables, r, CI_95_low, CI_95_high, p_value, Signif) %>%
  arrange(desc(abs(r))) #sorting strongest correlation to weakest (easy overview of "useful" variables) 

print (D.1_gdp_crop_correlation, n = Inf)



#D_2# Population and 'Crop'
    #only "Population"
D.2_population_crop_correlation <- tibble(Crop_Variables = D_0_crop_variables) %>%
  rowwise() %>% # setting up looped workflow, inside new table (D.1_gdp_crop_correlation), with one column for each variable (Rice_Yield/Production, Corn_Yield/...)
  mutate(       #adding new columns with the following names (r, p_value...)
    test      = list(cor.test(FAO_WDI_ordered_data$Population, 
                              FAO_WDI_ordered_data[[Crop_Variables]], 
                              method = "pearson")),
    r = round(test$estimate, 2),              #takes the result for 'r' from the actual test (test), and rounding it up to 2 decimals 
    CI_95_low = round (test$conf.int[1], 2),  #confidence interval, if it croses 0, we seethat this variable does not linearly predict the yield (variable)
    CI_95_high = round(test$conf.int [2], 2), #confidence interval for high relation
    p_value = round(test$p.value, 4),         #round p_value to 4 decimals 
    Signif = case_when(
      p_value < 0.001 ~ "high Sig.",
      p_value < 0.01  ~ "medium Sig.",
      p_value < 0.05  ~ "low Sig.",
      TRUE            ~ "no Sig."
    )
  ) %>%
  ungroup() %>% #sort and "clean" results
  select(Crop_Variables, r, CI_95_low, CI_95_high, p_value, Signif) %>%
  arrange(desc(abs(r))) #sorting strongest correlation to weakest (easy overview of "useful" variables) 

print (D.2_population_crop_correlation, n = Inf)

#D_3# Life_expectancy and 'Crop'
  #only "Life_expectancy"

D.3_Life_expectancy_crop_correlation <- tibble(Crop_Variables = D_0_crop_variables) %>%
  rowwise() %>% # setting up looped workflow, inside new table (D.1_gdp_crop_correlation...all different names), with one column for each variable (Rice_Yield/Production, Corn_Yield/...)
  mutate(       #adding new columns with the following names (r, p_value...)
    test      = list(cor.test(FAO_WDI_ordered_data$Life_expectancy, 
                              FAO_WDI_ordered_data[[Crop_Variables]], 
                              method = "pearson")),
    r = round(test$estimate, 2),              #takes the result for 'r' from the actual test (test), and rounding it up to 2 decimals 
    CI_95_low = round (test$conf.int[1], 2),  #confidence interval, if it croses 0, we seethat this variable does not linearly predict the yield (variable)
    CI_95_high = round(test$conf.int [2], 2), #confidence interval for high relation
    p_value = round(test$p.value, 4),         #round p_value to 4 decimals 
    Signif = case_when(
      p_value < 0.001 ~ "high Sig.",
      p_value < 0.01  ~ "medium Sig.",
      p_value < 0.05  ~ "low Sig.",
      TRUE            ~ "no Sig."
    )
  ) %>%
  ungroup() %>% #sort and "clean" results
  select(Crop_Variables, r, CI_95_low, CI_95_high, p_value, Signif) %>%
  arrange(desc(abs(r))) #sorting strongest correlation to weakest (easy overview of "useful" variables) 

print (D.3_Life_expectancy_crop_correlation, n = Inf) 

#D_4# Secondary_education_LV and 'Crop'
  #only "Secondary_education_LV"

D.4_Secondary_education_LV_crop_correlation <- tibble(Crop_Variables = D_0_crop_variables) %>%
  rowwise() %>% # setting up looped workflow, inside new table (D.1_gdp_crop_correlation), with one column for each variable (Rice_Yield/Production, Corn_Yield/...)
  mutate(       #adding new columns with the following names (r, p_value...)
    test      = list(cor.test(FAO_WDI_ordered_data$Secondary_education_LV, 
                              FAO_WDI_ordered_data[[Crop_Variables]], 
                              method = "pearson")),
    r = round(test$estimate, 2),              #takes the result for 'r' from the actual test (test), and rounding it up to 2 decimals 
    CI_95_low = round (test$conf.int[1], 2),  #confidence interval, if it croses 0, we seethat this variable does not linearly predict the yield (variable)
    CI_95_high = round(test$conf.int [2], 2), #confidence interval for high relation
    p_value = round(test$p.value, 4),         #round p_value to 4 decimals 
    Signif = case_when(
      p_value < 0.001 ~ "high Sig.",
      p_value < 0.01  ~ "medium Sig.",
      p_value < 0.05  ~ "low Sig.",
      TRUE            ~ "no Sig."
    )
  ) %>%
  ungroup() %>% #sort and "clean" results
  select(Crop_Variables, r, CI_95_low, CI_95_high, p_value, Signif) %>%
  arrange(desc(abs(r))) #sorting strongest correlation to weakest (easy overview of "useful" variables) 

print (D.4_Secondary_education_LV_crop_correlation, n = Inf)


#put all in files:
write.table(D.1_gdp_crop_correlation, file = "CSV_Files_FAO/D.1_gdp_crop_correlation.csv")
write.table(D.2_population_crop_correlation, file = "CSV_Files_FAO/D.2_population_crop_correlation.csv")
write.table(D.3_Life_expectancy_crop_correlation, file = "CSV_Files_FAO/D.3_Life_expectancy_crop_correlation.csv")
write.table(D.4_Secondary_education_LV_crop_correlation, file = "CSV_Files_FAO/D.4_Secondary_education_LV_crop_correlation.csv")

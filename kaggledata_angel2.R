install.packages("dplyr")
library(dplyr)
df <- read.csv("Data Original/yield_df.csv") #read the csv file "FILENAME"
View(df) #see if it's properly imported
countr=c("Armenia", "Azerbaijan", "Bangladesh", "India", "Indonesia", "Iraq", 
         "Japan", "Kazakhstan", "Lebanon", "Malaysia", "Nepal", "Pakistan", 
         "Qatar", "Saudi Arabia", "Sri Lanka", "Tajikistan", "Thailand")
df2 <- filter(df, "Area" == values(countr)) #filtro va mal?? ver como cambiarlo
View(df2)

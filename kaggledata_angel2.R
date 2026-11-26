# install.packages("dplyr")
library(dplyr)
dfra <- read.csv("Data Original/yield_df.csv") #read the csv file "FILENAME"
#View(df) #see if it's properly imported

# dfra$Area %in% countr #a filtering vector (true/false)
# sum(dfra$Area %in% countr) #see if the vector works

dfra3 <- dfra[dfra$Area %in% countr, ] #new dataframe with selected rows
# View(dfra3) #see if it's properly filtered
c3 <- c(unique(dfra$Item))

cropss <- c("Maize","Potatoes","Rice, paddy","Soybeans","Wheat")

dfra4 <- dfra3 [dfra3$Item %in% cropss, ] #2nd filter for selected crops
#c4 <- c(unique(dfra4$Item)) #check for 2nd filter


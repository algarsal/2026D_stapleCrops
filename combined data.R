
#install.packages("tidyverse")
library(tidyverse)

fao <- read.csv("Data/cleaned data staple crops/FAO cleaned data.csv", sep = " ")
kaggle <- read.csv("Data/cleaned data staple crops/kaggle cleaned data.csv", sep = " ")
glimpse(fao)
glimpse(kaggle)

colnames(kaggle)[colnames(kaggle) == "Area"] <- "Country"
fao2 <- fao[,1:6]

joindf <- left_join(kaggle, fao2, by = c("Country", "Year"))

save(joindf, file = "Data/combineddata.Rdata")

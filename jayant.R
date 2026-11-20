df <- read.csv("Data Original/yield_df.csv")
View(df)
summary(df)
colSums(is.na(df))
unique_areas <- sort(unique(df$Area))
print(unique_areas, max = length(unique_areas))
length(unique_areas)

library(tidyr)
df <- df[-1]
yield <- pivot_wider(df, names_from = Item, values_from = hg.ha_yield, values_fn = mean)

save(yield, file = "Data/Yield.rdata")

df |>
  dplyr::summarise(n = dplyr::n(), .by = c(Area, Year,
                                           average_rain_fall_mm_per_year, pesticides_tonnes, avg_temp, Item))
|>
  dplyr::filter(n > 1L) 
levels(as.factor(df$Item))

df$Item <- as.factor(df$Item)
levels(df$Item)[5] <- 'rice'


test <- df |>
  dplyr::summarise(n = dplyr::n(), .by = c(Area, Year,
                                           average_rain_fall_mm_per_year, pesticides_tonnes, avg_temp, Item))
|>
  dplyr::filter(n > 1L) 
yield <- pivot_wider(df, names_from = Item, values_from = hg.ha_yield, values_fn = mean)

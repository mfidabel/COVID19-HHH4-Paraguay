if (!require("readxl")) install.packages("readxl")
# Loading
library("readxl")
library(dplyr)
# read file
precipitation <- read_excel("data/original/weather/precipitacion.xlsx")
# rename columns
colnames(precipitation) <- c("code", "station", "year", "month","day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9", "day10", "day11", "day12", "day13", 
                             "day14", "day15", "day16", "day17", "day18", "day19", "day20", "day21", "day22", "day23", "day24", "day25", "day26", "day27", "day28", "day29", "day30", "day31")
# select only 2021 data 
precipitation <- filter(precipitation, year == 2021)
# convert string values to numeric values
# get non-numeric indexes
index <- !sapply(precipitation, is.numeric)
# leave station as string
index[2] <- FALSE
# convert string to numeric
precipitation[index] <- lapply(precipitation[index], as.numeric)
# replace NA values
precipitation[is.na(precipitation)] <- 0

#precipitation$mean <- rowMeans(precipitation[,5:6])
print(precipitation)

print(precipitation)
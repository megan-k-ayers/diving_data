rm(list = ls())
library(pdftools)

combined <- read.csv("./data/diving_world_cup_2021_combined.csv", as.is = T)
combined$judge_name <- trimws(combined$judge_name)
judge_info <- read.csv("./Part-Ia.csv", as.is = T)
names(judge_info)[1] <- c("judge_name")
combined <- merge(combined, judge_info)
combined <- combined[complete.cases(combined), ]

summary(combined)
hist(combined$Score)

library(reshape2)

all_results <- read.csv("detailed_results/all_results.csv", as.is = T)
all_judges <- read.csv("judges/all_judges.csv", as.is = T)
judge_info <- read.csv("Part-Ia.csv", as.is = T)


all_judges <- melt(all_judges, 
                 id=colnames(all_judges)[!grepl("J", colnames(all_judges))], 
                 variable.name = "judge_id",
                 value.name = "judge_name")

names(judge_info)[1] <- c("judge_name")
combined <- merge(all_judges, judge_info)


all_results <- melt(all_results, 
     id=colnames(all_results)[!grepl("J", colnames(all_results))], 
     variable.name = "judge_id",
     value.name = "judges_score")

combined <- merge(all_results, combined)
combined <- combined[!is.na(combined$judges_score), ]
combined$pointsbehind[is.na(combined$pointsbehind)] <- 0
combined <- combined[combined$judges_score <= 10, ]
combined <- combined[complete.cases(combined), ]

summary(combined)
hist(combined$judges_score)

write.csv(df, "all_combined.csv", row.names = F)

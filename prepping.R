rm(list = ls())
r <- read.csv("./data/diving_world_cup_2021_results.csv")[, -c(13:17)]
j <- read.csv("./data/diving_world_cup_2021_judges.csv")

# Rename column referring to prelims vs semifinals vs finals as level to avoid
# confusion with "rounds" referring to repeated dives within each competition
names(r)[13] <- "level"

# Temporarily get a unique identifier for diver-event-level combo, use to 
# number the rounds of dives for each individual during a specific competition
r$id <- paste(r$Name, r$level, r$event)
id_map <- data.frame(id = unique(r$id), num = 1:length(unique(r$id)))
r <- merge(r, id_map, by = "id")
r <- r[, -1]
names(r)[15] <- "id"
r$round <- ave(r$Dive.No., r$id, FUN = seq_along)

# Reshape so that we have a row for each score for each judge for each dive
r <- r[, c(13, 14, 16, 1, 2, 3, 5:12)]
r <- reshape(r, varying = list(8:14), v.names = "Score",
                direction = "long")[, -10]
names(r)[8] <- "judge_num"

# Attach judge information
names(j) <- c("judge_num", "judge_name", "round", "level", "event")
x <- merge(r, j, by = c("judge_num", "round", "level", "event"))
x <- x[order(x$event, x$level, x$Rank, x$round, x$judge_num),
       c(4, 3, 5, 6, 7, 2, 8, 9, 1, 10)]

# write.csv(x, "./data/diving_world_cup_2021_combined.csv", row.names = FALSE)


rm(list = ls())
library(pdftools)
source("./utils.R")

# User provided inputs
pdf_path <- "./data/omega_complete_results_book_2021_diving_world_cup.pdf"
events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
            " Men's 10m Platform", "Women's 10m Platform")
rounds <- c("Preliminary", "Semifinal", "Final")
col_info <- read.csv("./data/manual_column_info.csv")
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")

# Get the relevant page numbers for these events/rounds
pages <- collect_pgs(events, rounds, pdf_path)

# Concatenate results and judges into one big table, adding columns for event
# and round (renamed level to avoid confusion with judging round)
results <- data.frame()
judges <- data.frame()
for (i in 1:length(pages)) {
  
  pages_name <- names(pages)[i]
  ids <- unlist(strsplit(pages_name, "\\."))
  ids <- gsub("_", " ", ids)
  
  print(paste("Tabulating", pages_name))
  
  x <- get_pdf_data(pdf_path, pages[[i]])
  
  if (ids[3] == "results") {
    x <- tabulate_results(x, col_info)
    x$level <- ids[1]
    x$event <- ids[2]
    results <- rbind(results, x)
  }
  
  if (ids[3] == "judges") {
    x <- tabulate_judges(x)
    x$level <- ids[1]
    x$event <- ids[2]
    judges <- rbind(judges, x)
  }
  
}

test <- aggregate(round ~ num + level + event, data = judges, length)

# write.csv(results, "./data/diving_world_cup_2021_results.csv", row.names = FALSE)
# write.csv(judges, "./data/diving_world_cup_2021_judges.csv", row.names = FALSE)


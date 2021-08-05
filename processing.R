rm(list = ls())
library(pdftools)
source("./utils.R")

# User provided inputs
pdf_path <- "./omega_complete_results_book_2021_diving_world_cup.pdf"
events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
            " Men's 10m Platform", "Women's 10m Platform")
rounds <- c("Preliminary", "Semifinal", "Final")
col_info <- read.csv("./manual_column_info.csv") # TODO: automate this step
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")

# Get the page numbers within the results book for the specified events and
# rounds for both the detailed results and the panel of judges pages
pages <- collect_pgs(events, rounds, pdf_path)

results <- data.frame()
for (i in 1:length(pages)) {
  
  pages_name <- names(pages)[i]
  print(pages_name)
  ids <- unlist(strsplit(pages_name, "\\."))
  ids <- gsub("_", " ", ids)
  
  x <- get_pdf_data(pdf_path, pages[[i]])
  
  if (ids[3] == "results") {
    x <- tabulate_results(x, col_info)
    
    x$round <- ids[1]
    x$event <- ids[2]
    
    results <- rbind(results, x)
  }
  
}




# write.csv(results, "diving_world_cup_2021_results.csv", row.names = FALSE)

# TODO: What do strike-throughs mean on original sheet? Is it bad that that info
# is lost?


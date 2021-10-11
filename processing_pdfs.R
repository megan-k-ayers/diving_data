rm(list = ls())
library(pdftools)
source("./utils.R")

# User provided inputs
# pdf_path <- "./data/FINA_past/2018_FINA_World_Series_Montreal.pdf"
# events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
#             " Men's 10m Platform", "Women's 10m Platform")
# rounds <- c("Semifinal A", "Semifinal B", "Final")

# pdf_path <- "./data/FINA_past/2015_FINA_World_Series_Dubai.pdf"
# events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
#             " Men's 10m Platform", "Women's 10m Platform")
# rounds <- c("Semifinal A", "Semifinal B", "Final")

pdf_path <- "./data/omega_complete_results_book_2021_diving_world_cup.pdf"
events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
            " Men's 10m Platform", "Women's 10m Platform")
rounds <- c("Preliminary", "Semifinal", "Final")


# Get the relevant page numbers for these events/rounds
pages <- collect_pgs(events, rounds, pdf_path)

# Concatenate results and judges into one big table, adding columns for event
# and round
results <- data.frame()
judges <- data.frame()
for (i in 1:length(pages)) {
  
  pages_name <- names(pages)[i]
  ids <- unlist(strsplit(pages_name, "\\."))
  ids <- gsub("_", " ", ids)
  names(ids) <- c("round", "event", "type")
  
  print(paste("Tabulating", paste(ids, collapse = " ")))
  
  x <- get_pdf_data(pdf_path, pages[[i]])
  
  if (ids["type"] == "results") {
    x <- tabulate_results(x, ids["event"], ids["round"])
    results <- rbind(results, x)
  }
  
  if (ids[3] == "judges") {
    x <- tabulate_judges(x, ids["event"], ids["round"])
    judges <- rbind(judges, x)
  }
  
}




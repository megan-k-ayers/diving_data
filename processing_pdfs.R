rm(list = ls())
library(pdftools)
source("./utils.R")

# ----------------------------------- SETUP -----------------------------------

# Main function to collect all detailed results and judging data from one 
# diving PDF. Utilizes functions in utils.R, and writes CSV files to `results`
# and `judges`.
run <- function(pdf_path, events, rounds) {
  
  print(paste("Tabulating results and judges for", pdf_path))
  
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
  
  # Write results and judges csv's
  print(paste("Saving results and judges df's for", pdf_path))
  
  comp_name <- gsub("./data/FINA_past/|.pdf|FINA_", "", pdf_path)
  write.csv(results,
            paste("./detailed_results/", comp_name, "_RESULTS.csv", sep = ""),
            row.names = FALSE)
  write.csv(judges,
            paste("./judges/", comp_name, "_JUDGES.csv", sep = ""),
            row.names = FALSE)
  
}


# ------------------------------ GENERATING FILES -----------------------------

# Names of rounds when we have prelims
rounds_p <- c("Preliminary", "Semifinal", "Final")
# Names of rounds when we have Semis A and B
rounds_sf <- c("Semifinal A", "Semifinal B", "Final")

# Events to grab
events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
            " Men's 10m Platform", "Women's 10m Platform")


# TOKYO WORLD CUP
run(pdf_path = "./data/FINA_past/2021_FINA_World_Cup.pdf",
    events = events, rounds = rounds_p)

# MONTREAL 2018
run(pdf_path = "./data/FINA_past/2018_FINA_World_Series_Montreal.pdf",
    events = events, rounds = rounds_sf)

# DUBAI 2015
run(pdf_path = "./data/FINA_past/2015_FINA_World_Series_Dubai.pdf",
    events = events, rounds = rounds_sf)

# BEIJING 2017
run(pdf_path = "./data/FINA_past/2017_FINA_World_Series_Beijing.pdf",
    events = events, rounds = rounds_sf)

# FUJI 2018
run(pdf_path = "./data/FINA_past/2018_FINA_World_Series_Fuji.pdf",
    events = events, rounds = rounds_sf)







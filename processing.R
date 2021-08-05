rm(list = ls())
library(pdftools)
source("./utils.R")

# User provided inputs
pdf_path <- "./omega_complete_results_book_2021_diving_world_cup.pdf"
event <- " Men's 3m Springboard"
rounds <- c("Preliminary", "Semifinal", "Final")
col_info <- read.csv("./manual_column_info.csv") # TODO: automate this step
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")

# Get the page numbers within the results book for the specified events and
# rounds
pages <- collect_all_event_pgs(event, rounds, pdf_path)

# Get the PDF data from those pages 
x <- get_pdf_data(pdf_path, pages$semifinal_results)

x <- tabulate_pages(x, col_info)


# TODO: What do strike-throughs mean on original sheet? Is it bad that that info
# is lost?


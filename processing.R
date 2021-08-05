rm(list = ls())
library(pdftools)
source("./utils.R")

pdf_path <- "./omega_complete_results_book_2021_diving_world_cup.pdf"
event <- " Men's 3m Springboard"
rounds <- c("Preliminary", "Semifinal", "Final")

pages <- collect_all_event_pgs(event, rounds, pdf_path)


# --------------------- Testing with final round results ---------------------
x <- get_pdf_data(pdf_path, pages$final_results)

# TODO: Currently expecting this to be manually generated - try to automate
col_info <- read.csv("./manual_column_info.csv")
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")

x <- tabulate_pages(x, col_info)





# TODO: What do strike-throughs mean on original sheet? Is it bad that that info
# is lost?


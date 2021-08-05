rm(list = ls())
library(pdftools)
source("./utils.R")

pdf_path <- "./omega_complete_results_book_2021_diving_world_cup.pdf"
event <- " Men's 3m Springboard"
rounds <- c("Preliminary", "Semifinal", "Final")

pages <- collect_all_event_pgs(event, rounds, pdf_path)


# ------------------------- Testing with final round -------------------------
x_r <- get_pdf_data(pdf_path, pages$final_results)
x <- x_r[[1]]


# Filtering out header/footer
x <- x[which(x$y > 132), ]  # hard-coded y value of "Detailed Results" text
x <- x[which(x$y < 762), ]  # hard-coded y value of "Official Timekeeping" text


# Group text into y-ranges (approximately the rows)
x <- get_page_row_ranges(x)


# Group text into x-ranges (approximately the columns)
# TODO: Currently expecting this to be manually generated - it seems like the
# layout of these sheets is very consistent but automating it would be ideal
col_info <- read.csv("./manual_column_info.csv")
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")
x <- get_page_col_ranges(x, col_info)


# Pivot data using x/y ranges to recreate the table structure
x <- pivot_to_table(x)


# CLEANING UP
# First two rows have column names
x[1, ] <- ifelse(is.na(x[1, ]), "", paste(x[1, ], " ", sep = ""))
names(x) <- paste(x[1, ], x[2, ], sep = "")
x <- x[-c(1, 2), ]
rownames(x) <- NULL

x <- extend_name_rank_nat(x)

# Handle case where "GARCIA BOISSIER" header text was in a range above first
# row of table
# TODO: See how robust this is with other sheets
for (i in 1:nrow(x)) {
  if(sum(is.na(x[i, 4:17])) == 14) {
    x <- x[-i, ]
  }
}

# Combine names that spilled over onto multiple lines
diver_names <- x[!duplicated(x[, c("Name", "Rank")]), c("Name", "Rank")]
diver_names <- aggregate(Name ~ Rank, data = diver_names, paste, collapse = " ")

x <- merge(x[, -2], diver_names, by = "Rank")[, c(1, 17, 2:16)]

# TODO: What do strike-throughs mean on original sheet? Is it bad that that info
# is lost?


rm(list = ls())
library(pdftools)
source("./utils.R")

# User provided inputs
pdf_path <- "./omega_complete_results_book_2021_diving_world_cup.pdf"
events <- c(" Men's 3m Springboard", "Women's 3m Springboard",
            " Men's 10m Platform", "Women's 10m Platform")
rounds <- c("Preliminary", "Semifinal", "Final")
col_info <- read.csv("./manual_column_info.csv") # TODO: automate this step?
col_info$x_range <- paste(col_info$min, " - ", col_info$max, sep = "")

# Get the page numbers within the results book for the specified events and
# rounds for both the detailed results and the panel of judges pages
pages <- collect_pgs(events, rounds, pdf_path)

results <- data.frame()
for (i in 1:length(pages)) {
  
  pages_name <- names(pages)[i]
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



# ------------------- Testing with panel of judges pages -------------------
# NOTE: The rounds that judges are assigned to does not seem to be consistent
# in any particular way...

page <- pages[[14]]
x <- get_pdf_data(pdf_path, c(page))[[1]]

pages_name <- names(pages)[14] # !! generalize when you make this formal
ids <- unlist(strsplit(pages_name, "\\."))
ids <- gsub("_", " ", ids)

# "Panel of Judges" text is at a little above y = 135, footer a little below
# y = 700, filter out
x <- x[which(x$y > 150 & x$y < 700),]

# Looking for "Panels" so we know which rounds had which judges
panels <- x[which(x$text == "Panel"), c(2, 4)]
panels$min_y <- panels$y - panels$height/2
panels$max_y <- panels$y + panels$height/2
panels <- panels[order(panels$y), ]

judges_table <- data.frame()
for (i in 1:nrow(panels)) {
  
  if (i == nrow(panels)) {
    x_panel <- x[which(x$y > as.numeric(panels[i, "min_y"])), ]
  } else {
    x_panel <- x[which(x$y > as.numeric(panels[i, "min_y"]) &
                         x$y < as.numeric(panels[i + 1, "min_y"])), ]
  }
  
  x_panel <- get_page_row_ranges(x_panel)
  x_panel <- aggregate(text ~ y_range, data = x_panel, paste, collapse = " ")
  
  rounds <- x_panel[which(grepl("rounds", x_panel$text)), 2]
  rounds <- gsub("[^0-9-]", "", rounds)
  rounds <- as.integer(unlist(strsplit(rounds, "-")))
  
  judges <- x_panel[which(grepl("Judge", x_panel$text)), 2]
  judges <- data.frame(num = gsub("[^0-9]", "", judges),
                       name = gsub("[^a-zA-Z ]", "", judges))
  judges$name <- gsub("Judge ", "", judges$name)
  
  rounds <- data.frame(round = rounds)
  judges <- merge(judges, rounds, by = c())
  
  judges_table <- rbind(judges_table, judges)

}

judges_table$level <- ids[1]
judges_table$event <- ids[2]


# TODO: For now am ignoring alternate judges - when/where do they come up
# in the detailed results page?

# TODO: Manually obtain some of the judges nationalities



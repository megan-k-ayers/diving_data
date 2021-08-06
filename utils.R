# -----------------------------------------------------------------------------
# Helper functions for converting PDF to data frame using pdftools
# -----------------------------------------------------------------------------

# TODO: Functions need to be properly commented and in some cases cleaned and/or
# separated into multiple functions

# TODO: Make tabulate_judges robust to multiple pages of judges

# TODO: For now am ignoring alternate judges - when/where do they come up
# in the detailed results page?

# TODO: Manually obtain some of the judges nationalities

# -----------------------------------------------------------------------------
# Expects all_pages to be list of strings, each string containing all text from
# the page
get_results_pgs <- function(event, round, all_pages) {
  
  pgs <- c()
  for (i in 1:length(all_pages)) {
    
    flag <- grepl(paste(event, round, "Detailed Results", sep = ".*"),
                  all_pages[[i]])
    if (flag) { pgs <- c(pgs, i) }
    
  }
  
  return(pgs)
}


# -----------------------------------------------------------------------------
# Expects all_pages to be list of strings, each string containing all text from
# the page
get_judges_pgs <- function(event, round, all_pages) {
  
  pgs <- c()
  for (i in 1:length(all_pages)) {
    
    flag <- grepl(paste(event, round, "Panel of Judges", sep = ".*"),
                  all_pages[[i]]) &
      !grepl("For more detail on the judges see", all_pages[[i]])
    if (flag) { pgs <- c(pgs, i) }
    
  }
  
  return(pgs)
}


# -----------------------------------------------------------------------------
collect_pgs <- function(events, rounds, pdf_path){
  
  x <- pdf_text(pdf_path)
  pages_r <- sapply(rounds, function(r) { sapply(events, function(e) {
      get_results_pgs(e, r, x)}, simplify = FALSE)}, simplify = FALSE)
  pages_r <- unlist(pages_r, recursive = FALSE)
  
  pages_j <- sapply(rounds, function(r) { sapply(events, function(e) {
    get_judges_pgs(e, r, x)}, simplify = FALSE)}, simplify = FALSE)
  pages_j <- unlist(pages_j, recursive = FALSE)
  
  pages <- c(pages_r, pages_j)
  
  names(pages) <- c(paste(tolower(names(pages_r)), "results", sep = "."),
                    paste(tolower(names(pages_j)), "judges", sep = "."))
  names(pages) <- gsub(" ", "_", names(pages))
  
  return(pages)
  
}


# -----------------------------------------------------------------------------
get_pdf_data <- function(pdf_path, pages) {
  
  x <- pdf_data(pdf_path)
  return(x[pages])
  
}


# -----------------------------------------------------------------------------
get_page_row_ranges <- function(page_data) {
  
  # **Using fact that heights are always same height**
  # (would want to generalize that ideally)
  if (length(unique(page_data$height)) != 1) {
    return("Error: varying text heights, expected height to be constant.")
  }
  
  min_height <- min(page_data$height)
  
  y_positions <- table(page_data$y)
  # only take into account positions with frequency > 1
  y_positions <- y_positions[which(y_positions > 1)]
  y_positions <- as.numeric(names(y_positions))
  y_positions <- y_positions - min_height/2
  
  y_ranges <- seq(min(y_positions), max(y_positions) + min_height, min_height)
  y_ranges <- data.frame(min = y_ranges, max = y_ranges + min_height)
  y_ranges$name <- paste(y_ranges$min, " - ", y_ranges$max, sep = "")
  
  page_data$y_range <- ""
  page_data$y <- as.numeric(page_data$y)
  for (i in 1:nrow(y_ranges)){
    
    page_data$y_range <- ifelse(page_data$y > y_ranges[i, 1] &
                                  page_data$y < y_ranges[i, 2],
                        y_ranges[i, 3], page_data$y_range)
    
  }
  
  if(sum(page_data$y_range == "") > 0) {
    return("Error: some text values could not be placed in a row range.")
  } 
  
  return(page_data)
  
}


# -----------------------------------------------------------------------------
# Earlier attempt to automatically figure out the column ranges, can use to 
# aid setting the ranges manually. Ideally could get this to work for all
# entries, maybe can think of something clever using width? But alignment is
# changing and I think this method will require some sort of user management...
get_approx_page_col_ranges <- function(page_data, n_ranks) {
  
  # Grouping horizontal positions into rows
  min_width <- min(page_data$width)
  
  x_positions <- table(page_data$x) # see frequencies of horizontal text positions
  x_positions <- x_positions[which(x_positions >= n_ranks)]
  x_positions <- as.numeric(names(x_positions))
  
  x_ranges <- data.frame(min = x_positions - min_width/2,
                         max = x_positions + min_width/2)
  x_ranges$name <- paste(x_ranges$min, " - ", x_ranges$max, sep = "")
  
  page_data$x_range <- ""
  page_data$x <- as.numeric(page_data$x)
  for (i in 1:nrow(x_ranges)){
    
    page_data$x_range <- ifelse(page_data$x > x_ranges[i, 1] &
                                  page_data$x < x_ranges[i, 2],
                        x_ranges[i, 3], page_data$x_range)
    
  }
  
  # Issue with those remaining is going to be deciding when it is appropriate
  # to extend ranges and in which direction... requires more info?
  
  return(page_data)
  
}


# -----------------------------------------------------------------------------
# Not loving this right now because it requires manually specified estimates of
# the column ranges - will try other ways to automate this
get_page_col_ranges <- function(x, col_info) {
  
  x$x_range <- ""
  for (i in 1:nrow(col_info)){
    
    x$x_range <- ifelse(x$x > col_info[i, "min"] & x$x < col_info[i, "max"],
                        col_info[i, "x_range"], x$x_range)
    
  }
  
  return(x)
  
}


# -----------------------------------------------------------------------------
pivot_pg_data <- function(x) {
  
  # Only missing range for "Judge's Score" heading which is extraneous anyways
  # (this was in my initial test PDF - could be filtering more out later)
  x <- x[which(x$x_range != ""), c(6, 7, 8)]
  
  # Concatenate any text that is in the same row/col combination
  x <- aggregate(text ~ x_range + y_range, data = x, paste, collapse = " ")
  x <- merge(x, col_info, by = "x_range")
  x <- x[, c(3, 2, 5)]
  names(x) <- c("text", "y_range", "x_min")
  x <- x[order(x$x_min), ]
  
  x <- reshape(x, timevar = "x_min", idvar = "y_range", direction = "wide")
  x$y_min <- as.numeric(gsub("- [0-9.]+", "", x$y_range))
  x <- x[order(x$y_min), c(2:18)]
  row.names(x) <- NULL
  
  return(x)
  
}


# -----------------------------------------------------------------------------
# Populate name, rank, and nationality fields to all relevant rows
extend_diver_info <- function(y) {
  
  name <- NA
  rank <- NA
  NAT <- NA
  for (i in 1:(nrow(y)-1)) {
    
    # If the name at this row is not NA, this is the new name going forward
    if (!is.na(y[i, "Name"])) { 
      name <- y[i, "Name"]
    }
    
    # If the next name is NA, populate it with the current name
    if (is.na(y[i + 1, "Name"])) {
      y[i + 1, "Name"] <- name
    }
    
    # Repeat logic with rank, nationality
    if (!is.na(y[i, "Rank"])) { 
      rank <- y[i, "Rank"]
    }
    if (is.na(y[i + 1, "Rank"])) {
      y[i + 1, "Rank"] <- rank
    }
    
    if (!is.na(y[i, "NAT"])) { 
      nat <- y[i, "NAT"]
    }
    if (is.na(y[i + 1, "NAT"])) {
      y[i + 1, "NAT"] <- nat
    }
    
  }
  
  return(y)
  
}


# -----------------------------------------------------------------------------
# Cleaning up pivoted page table
# TODO: Evaluate how robust this is to other pages/files
clean_page_table <- function(x) {
  # First two rows have column names
  x[1, ] <- ifelse(is.na(x[1, ]), "", paste(x[1, ], " ", sep = ""))
  names(x) <- paste(x[1, ], x[2, ], sep = "")
  x <- x[-c(1, 2), ]
  rownames(x) <- NULL
  
  x <- extend_diver_info(x)
  
  # Handle case where "GARCIA BOISSIER" header text was in a range above first
  # row of table
  # TODO: See how robust this is with other sheets
  for (i in 1:nrow(x)) {
    if(sum(is.na(x[i, 4:17])) == 14) {
      x <- x[-i, ]
    }
  }
  
  # Combine names that spilled over onto multiple lines
  diver_names <- x[!duplicated(x[, c("Name", "Rank", "NAT")]),
                   c("Name", "Rank", "NAT")]
  diver_names <- aggregate(Name ~ Rank + NAT,
                           data = diver_names, paste, collapse = " ")
  
  x <- merge(x[, -2], diver_names, by = c("Rank", "NAT"))[, c(1, 17, 2:16)]
  
  return(x)
}


# -----------------------------------------------------------------------------
tabulate_results <- function(x_full, col_info) {
  
  n_pages <- length(x_full)
  x_table <- data.frame()
  for (i in 1:n_pages){
    x <- x_full[[i]]
  
    # Filtering out header/footer 700 if last pg
    if (i == n_pages){
      x <- x[which(x$y > 132), ]  # hard-coded y val of bottom of header text
      x <- x[which(x$y < 690), ]  # hard-coded y val of top of footer text
    } else {
      x <- x[which(x$y > 132), ]  # hard-coded y val of bottom of header text
      x <- x[which(x$y < 762), ]  # hard-coded y val of top of footer text
    }
    
    # Group text into y-ranges (approximately the rows)
    x <- get_page_row_ranges(x)
    
    # Group text into x-ranges (approximately the columns)
    # TODO: Currently expecting col_info to be manually generated - it seems
    # like the layout of these sheets is very consistent but automating it would
    # be ideal. Maybe could use pdf_text output to get approx widths? 
    x <- get_page_col_ranges(x, col_info)
    
    # Pivot data using x/y ranges to recreate the table structure
    x <- pivot_pg_data(x)
    
    # This would be a blank page
    if (nrow(x) <= 2) {
      break
    }
    
    # Clean table
    x <- clean_page_table(x)
    
    x_table <- rbind(x_table, x)
    
  }
  
  return(x_table)
  
}


# -----------------------------------------------------------------------------
# TODO: This function could be split up into pieces and made more efficient/
# readable
tabulate_judges <- function(x) {
  
  if (length(x) > 1) {
    print("Error: multiple pages of judges.")
    return(NULL)
  }
  
  x <- x[[1]]
  
  # "Panel of Judges" text is at a little above y = 135, footer a little below
  # y = 700, filter out
  x <- x[which(x$y > 150 & x$y < 700),]
  
  # Looking for "Panels" so we know which rounds had which judges
  panels <- x[which(x$text == "Panel"), c(2, 4)]
  
  if (nrow(panels) > 0) {
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
      
      if (length(rounds) == 2 & nrow(panels) < 3) { # we have a range
        rounds <- seq(rounds[1], rounds[2])
      } # otherwise leave it - judges skip rounds
      
      judges <- x_panel[which(grepl("Judge", x_panel$text)), 2]
      judges <- data.frame(num = gsub("[^0-9]", "", judges),
                           name = gsub("[^a-zA-Z ]", "", judges))
      judges$name <- gsub("Judge ", "", judges$name)
      
      rounds <- data.frame(round = rounds)
      judges <- merge(judges, rounds, by = c())
      
      judges_table <- rbind(judges_table, judges)
      
    }
    
    return(judges_table)
    
  } else {
    
    x <- get_page_row_ranges(x)
    x <- aggregate(text ~ y_range, data = x, paste, collapse = " ")
    
    judges <- x[which(grepl("Judge", x$text)), 2]
    judges <- data.frame(num = gsub("[^0-9]", "", judges),
                         name = gsub("[^a-zA-Z ]", "", judges))
    judges$name <- gsub("Judge ", "", judges$name)
    
    rounds <- data.frame(round = 1:7)
    judges <- merge(judges, rounds, by = c())
    
    return(judges)
    
  }
  
}


# -----------------------------------------------------------------------------






# -----------------------------------------------------------------------------
# Helper functions for converting PDF to data frame using pdftools
# !!! Need to be commented properly and cleaned up
# -----------------------------------------------------------------------------


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
collect_all_event_pgs <- function(event, rounds, pdf_path){
  
  x <- pdf_text(pdf_path)
  pages <- sapply(rounds, function(r) {get_results_pgs(event, r, x)})
  pages <- c(pages, sapply(rounds, function(r) {get_judges_pgs(event, r, x)}))
  
  names(pages) <- c(paste(tolower(rounds), "results", sep = "_"),
                    paste(tolower(rounds), "judges", sep = "_"))
  
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
pivot_to_table <- function(x) {
  
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
extend_name_rank_nat <- function(y) {
  
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





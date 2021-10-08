# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# HELPER FUNCTIONS FOR CONVERTING PDF DIVING DATA TO DATA FRAMES USING PDFTOOLS
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Expects all_pages to be list of strings, each string containing all text from
# the page
get_results_pgs <- function(event, round, all_pages) {
  
  pgs <- c()
  for (i in 1:length(all_pages)) {
    
    # Search for any text like "[event name]...[round name]..."Detailed Results"
    # which signals that this is a detailed results page that we want
    if (grepl(paste(event, round, "Detailed Results", sep = ".*"),
              all_pages[[i]])) {
      pgs <- c(pgs, i) }
    
  }
  
  return(pgs)
}


# -----------------------------------------------------------------------------
# Expects all_pages to be list of strings, each string containing all text from
# the page
get_judges_pgs <- function(event, round, all_pages) {
  
  pgs <- c()
  for (i in 1:length(all_pages)) {
    
    # Search for any text like "[event name]...[round name]..."Panel of Judges"
    # which signals that this is a judge panel page that we want
    
    # Separated flag from if statement because the logical statement is so long
    flag <- grepl(paste(event, round, "Panel of Judges", sep = ".*"),
                  all_pages[[i]]) &
      !grepl("For more detail on the judges see", all_pages[[i]])
    if (flag) { pgs <- c(pgs, i) }
    
  }
  
  return(pgs)
}


# -----------------------------------------------------------------------------
# Uses get_results_pgs and get_judges_pgs to return a list of pages containing
# all detailed results and panel of judges pages
collect_pgs <- function(events, rounds, pdf_path){
  
  # The pdf_text function just grabs the concatenated text from each page -
  # using it only to identify which pages we want to keep for detailed results/
  # panel of judges
  x <- pdf_text(pdf_path)
  
  # For each round and for each event, extract the pages containing detailed
  # results. Flatten into one list with one item per event-round combo
  # which that is the list of indices which map the results pages
  pages_r <- sapply(rounds, function(r) { sapply(events, function(e) {
      get_results_pgs(e, r, x)}, simplify = FALSE)}, simplify = FALSE)
  pages_r <- unlist(pages_r, recursive = FALSE)
  
  # For each round and for each event, extract the pages containing judges
  # info Flatten into one list with one item per event-round combo
  # which that is the list of indices which map the judges pages
  pages_j <- sapply(rounds, function(r) { sapply(events, function(e) {
    get_judges_pgs(e, r, x)}, simplify = FALSE)}, simplify = FALSE)
  pages_j <- unlist(pages_j, recursive = FALSE)
  
  # Combine results and judges page lists
  pages <- c(pages_r, pages_j)
  
  # Want to keep identifying info of which event/round/page type these belong
  # to
  names(pages) <- c(paste(tolower(names(pages_r)), "results", sep = "."),
                    paste(tolower(names(pages_j)), "judges", sep = "."))
  names(pages) <- gsub(" ", "_", names(pages))
  
  return(pages)
  
}


# -----------------------------------------------------------------------------
# Given page indexes, get their pdf_data output
get_pdf_data <- function(pdf_path, pages) {
  
  x <- pdf_data(pdf_path)
  return(x[pages])
  
}


# -----------------------------------------------------------------------------
#' Function that combines list of pages from pdf_data output (adapted from
#' Jay's code in the first part of this assignment)
#' @param event Name of the diving event ("W3mSB", "M3mSB", "W10mPF", or
#' "M10mPF")
#' @param x pdf_data output
#' @return One list, where each item is a character array representing a "row"
#' of text in the original file 
combine_pages <- function(event, round, x) {
  
  if (length(x) > 1) {
    
    # Fake the y-coordinate just in case it matters, combine to
    # a data frame; work in a general way that will help later on I bet:
    for (i in 2:length(x)) x[[i]]$y <- x[[i]]$y + 1000 * (i - 1)
    
  }
  
  x <- as.data.frame(do.call(rbind, x))
  
  # Turn x into a list, each item corresponding to df subset with the same `y` 
  # values (so each x item is roughly a row of data)
  x <- split(x, x$y)
  
  # We really only need the `text` column now
  x <- lapply(x, function(a) a$text)
  
  # Tack on the event name and competition round for future reference
  x <- lapply(x, function(a) c(a, event, round))
  
  return(x)
  
}


#' Function that builds a data frame of diving results information, relying
#' on consistent structure of the table of results. A few values are hard coded
#' such the column names, but because these sheets are very standardized I
#' think it is justified, and deducing the information other ways would be less
#' reliable and more complex than necessary in my opinion.
#' Similarly, a bit of code is repeated between the three cases within the
#' function, but I think it would be overly complex to write new functions
#' just for those cases (eg. filling in constant columns).
#' @param x List where each item is a character array representing a "row"
#' of text in an original file (output of combine_pages)
#' @return Data frame of diving results - each row is an individual dive within
#' a competition round, within an event, for a given diver.
get_results_df <- function(x) {
  
  # Find where results tables start/end, use "Detailed Results" text as anchor
  # for top and "Official Timekeeping" for the bottom
  tops <- as.numeric(names(x[sapply(x, function(a){
    grepl("Detailed Results", paste(a, collapse = " "))})]))
  bottoms <- as.numeric(names(x[sapply(x, function(a){
    grepl("Official Timekeeping", paste(a, collapse = " "))})]))
  
  keep_inds <- c()
  for (i in 1:length(tops)) {
    
    # Get indices of pages in this chunk (between this top and bottom)
    pg_inds <- which(as.numeric(names(x)) > tops[i] &
                       as.numeric(names(x)) < bottoms[i])
    
    # Add to list of inds we want to keep
    keep_inds <- c(keep_inds, pg_inds)
        
  }

  # Filter to x items that were within table bounds
  x <- x[keep_inds]

  
  # Search for any text containing "J" followed by a number - this would be part
  # of the column headers, so remove it
  x <- x[!sapply(x, function(a) any(grep("J[0-9]", a)))]
  
  # TODO: More elegant way to search for these "bad" rows?
  # Checking for any rows that are all text (all table rows include some
  # numbers) OR that are the page footers (which have a million hyphens in
  # them) OR that contain astrixes WITH equals signs, or colons and removing
  # them, ignoring the added text pieces for event or competition round
  x <- x[!sapply(x, function(a) grepl("^[^0-9]+$|-{2,}|\\*.*\\=|:",
                                      paste(a[-c(length(a) - 1, length(a))],
                                            collapse = " ")))]
  
  # Initialize the data frame that we want to sort this information into
  z <- data.frame(rank = numeric(),
                  name = character(),
                  dcountry = character(),
                  divenum = numeric(),
                  divecode = character(), # note that this is not the divenum
                  DD = numeric(),
                  J1 = numeric(),
                  J2 = numeric(),
                  J3 = numeric(),
                  J4 = numeric(),
                  J5 = numeric(),
                  J6 = numeric(),
                  J7 = numeric(),
                  divepts = numeric(),
                  diverank = numeric(),
                  totalpts = numeric(),
                  overallrank = numeric(),
                  pointsbehind = numeric(),
                  event = character(),
                  round = character())
  
  # We observe that these columns are always filled
  const_cols <- c("divecode", "DD", "J1", "J2", "J3", "J4", "J5", "J6", 
                  "J7", "divepts", "diverank", "totalpts", "overallrank")
  
  for (i in 1:length(x)) { # Iterate through the "rows" of this table
    
    row <- x[[i]]
    
    # CASE 1: Row begins with rank
    if (grepl("^[0-9]{1,2}$", row[1])) { # Check if row item 1 is just a number
      
      # Then that number is the rank
      z[i, "rank"] <- row[1] 
      
      # We then know that the name follows the rank, but the number of parts of
      # the name vary (sometimes just last name, or first and last, potentially
      # names with multiple short words on same first line?). So anchor the end
      # of the name using the NOC code
      
      # Last name is at least row[2], and don't want to check the very last
      # entry which is the event round
      noc_ind <- which(sapply(row[3:(length(row) - 1)],
                              function(a) grepl("^[A-Z]{3}$", a))) + 2
      noc_ind <- noc_ind[length(noc_ind)] # Take last match (others may be name)
      
      # I'm going to assume that the last 3 letter capitalized word is the NOC,
      # and that everything between it and rank is part of the diver name
      z[i, "name"] <- paste(row[2:(noc_ind - 1)], collapse = " ")
      
      # Followed by NOC code...
      z[i, "dcountry"] <- row[noc_ind]
      # Followed by the const_cols
      z[i, const_cols] <- row[(noc_ind + 1):(noc_ind + length(const_cols))]
      # We know the last two items in row are the event and competition round,
      # because I put them there
      z[i, c("event", "round")] <- row[(length(row) - 1):length(row)]
      
      # Remove the items from row that we have used up
      row <- row[-c(1:(noc_ind + length(const_cols)),
                    (length(row) - 1):length(row))]
      
      # "Points Behind" is populated if row has an additional column
      z[i, "pointsbehind"] <- ifelse(length(row) > 0, row, "")
      
      # We know this is the first dive because the row starting with rank 
      # represents the first dive always
      z[i, "divenum"] <- 1 
      
    }
    
    
    # CASE 2: Row begins with spill-over name (could be multiple text chunks)
    if (grepl("^[A-Za-z]+$", row[1])) { # Check if row item 1 is just letters
      
      # Find where the Dive No. (DN) code starts (so that we know where the name
      # ends)
      dn_ind <- grep("^[0-9]{3,4}[A-Z]$", row) # pattern uniquely matches DN
      
      # Append the rest of the name to the row above, where the name starts
      z[i - 1, "name"] <- paste(c(z[i - 1, "name"], row[1:(dn_ind - 1)]),
                                collapse = " ")
      
      # Now fill in the const_cols
      z[i, const_cols] <- row[dn_ind:(dn_ind + length(const_cols) - 1)]
      
      # We know the last two items in row are the event and competition round,
      # because I put them there
      z[i, c("event", "round")] <- row[(length(row) - 1):length(row)]
      
      # Remove the items from row that we have used up
      row <- row[-c(1:(dn_ind + length(const_cols) - 1),
                    (length(row) - 1):length(row))]
      
      # "Points Behind" is populated if row has an additional column
      z[i, "pointsbehind"] <- ifelse(length(row) > 0, row, "")
      
      # Finally, inherit the rank, name, and nationality from the previous row
      z[i, c("rank", "name", "dcountry")] <- z[i - 1,
                                               c("rank", "name", "dcountry")]
      
      # Use the previous dive number to deduce which dive number is next
      z[i, "divenum"] <- z[i - 1, "divenum"] + 1
      
    }
    
    
    # CASE 3: Row begins with Dive No. code
    if (grepl("^[0-9]{3,4}[A-Z]$", row[1])) { # Same pattern above for dn_ind
      
      # We can populate the const_cols right away 
      z[i, const_cols] <- row[1:length(const_cols)]
      
      # We know the last two items in row are the event and competition round,
      # because I put them there
      z[i, c("event", "round")] <- row[(length(row) - 1):length(row)]
      
      # Remove the items from row that we have used up
      row <- row[-c(1:length(const_cols), (length(row) - 1):length(row))]
      
      # "Points Behind" is populated if row has an additional column
      z[i, "pointsbehind"] <- ifelse(length(row) > 0, row, "")
      
      # Finally, inherit the rank, name, and nationality from the previous row
      z[i, c("rank", "name", "dcountry")] <- z[i - 1,
                                               c("rank", "name", "dcountry")]
      
      # Use the previous dive number to deduce which dive number is next
      z[i, "divenum"] <- z[i - 1, "divenum"] + 1
      
    }
    
  }
  
  return(z)
  
}










# -----------------------------------------------------------------------------
# ------------------------------- ARCHIVED CODE -------------------------------
# -----------------------------------------------------------------------------

# TODO: Functions need to be properly commented with doc strings and in some
# cases cleaned and/or separated into multiple functions

# TODO: Make tabulate_judges robust to multiple pages of judges

# TODO: For now am ignoring alternate judges - when/where do they come up
# in the detailed results page?

# TODO: Manually obtain some of the judges nationalities


# -----------------------------------------------------------------------------
get_page_row_ranges <- function(page_data) {
  
  # **Using fact that heights are always same height**
  # (would want to generalize that ideally)
  if (length(unique(page_data$height)) != 1) {
    return("Error: varying text heights, expected height to be constant.")
  }
  
  min_height <- min(page_data$height)
  
  y_positions <- table(page_data$y)
  
  # Only take into account positions with frequency > 1
  y_positions <- y_positions[which(y_positions > 1)]
  y_positions <- as.numeric(names(y_positions))
  y_positions <- y_positions - min_height/2
  
  # Chunk up the y value ranges
  y_ranges <- seq(min(y_positions), max(y_positions) + min_height, min_height)
  y_ranges <- data.frame(min = y_ranges, max = y_ranges + min_height)
  y_ranges$name <- paste(y_ranges$min, " - ", y_ranges$max, sep = "")
  
  # Assign a y_range to each piece of text
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
# TODO: Make header/footer cut-offs a user input? (Test with Olympics data
# and see how it goes)
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



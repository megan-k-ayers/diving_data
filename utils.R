# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# HELPER FUNCTIONS FOR CONVERTING PDF DIVING DATA TO DATA FRAMES USING PDFTOOLS
#
# TODO: Functions need to all be properly commented with doc strings and in some
# cases cleaned and/or separated into multiple functions
#
# TODO: Make judges functions robust to multiple pages of judges
#
# TODO: Manually obtain some of the judges nationalities
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
#' Using page data, create row ranges for row values that have more than one
#' text piece associated to it in panel of judges sheets
#' @param page_data Data frame from one list item of pdf_data function output
#' @return page_data data frame with y$range attached
#' TODO: Consolidate this with results get_row_ranges version using some if
#' statements on the "type" of page
get_row_ranges_j <- function(page_data) {
  
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
    
    page_data$y_range <- ifelse(page_data$y > y_ranges[i, "min"] &
                                  page_data$y < y_ranges[i, "max"],
                                y_ranges[i, "name"], page_data$y_range)
    
  }
  
  if(sum(page_data$y_range == "") > 0) {
    return("Error: some text values could not be placed in a row range.")
  } 
  
  return(page_data)
  
}


# -----------------------------------------------------------------------------
# Given pdf_data output df item, add row-ranges variable to diving results df
# based on the y positions of the dive number codes (which are in all table
# rows and are reliable)
get_row_ranges_r <- function(page_data) {
  
  # **Using fact that heights are always same height**
  # (would want to generalize that ideally)
  if (length(unique(page_data$height)) != 1) {
    return("Error: varying text heights, expected height to be constant.")
  }
  
  min_height <- min(page_data$height)
  
  
  # Find the Dive No. values - use these to find the row ranges on this page
  # since they are a reliable pattern
  divenos <- page_data[grepl("^[0-9]{3,4}[A-Z]$", page_data$text), ]
  
  # If no dive no. are found, this page is not useful to us (empty page)
  if (nrow(divenos) == 0) {
    return("Page has no relevant row data.")
  }
  
  # Only take into account positions with frequency > 1
  y_positions <- as.numeric(divenos$y)
  y_positions <- y_positions - 2 # add a little extra padding for variation
  # in y values - this will be the min value
  # TODO: Add this tolerance as a user input?
  
  # Create max values
  y_ranges <- data.frame(min = y_positions, max = y_positions + 4)
  y_ranges$name <- paste(y_ranges$min, " - ", y_ranges$max, sep = "")
  
  # Assign a y_range to each piece of text
  page_data$y_range <- ""
  page_data$y <- as.numeric(page_data$y)
  for (i in 1:nrow(y_ranges)){
    
    page_data$y_range <- ifelse(page_data$y > y_ranges[i, 1] &
                                  page_data$y < y_ranges[i, 2],
                        y_ranges[i, 3], page_data$y_range)
    
  }
  
  # Filter out text that didn't fall in a row containing a dive number
  page_data <- page_data[page_data$y_range != "", ]
  
  return(page_data)
  
}


# -----------------------------------------------------------------------------
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
#' @param event Name of the diving event (character)
#' @param round Round of the diving event (character)
#' @return Data frame of diving results - each row is an individual dive within
#' a competition round, within an event, for a given diver.
get_results_df <- function(x, event, round) {
  
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
      # Event and round are constant for z
      z[i, c("event", "round")] <- c(event, round)
      
      # Remove the items from row that we have used up
      row <- row[-c(1:(noc_ind + length(const_cols)))]
      
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
      
      # Event and round are constant for z
      z[i, c("event", "round")] <- c(event, round)
      
      # Remove the items from row that we have used up
      row <- row[-c(1:(dn_ind + length(const_cols) - 1))]
      
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
      
      # Event and round are constant for z
      z[i, c("event", "round")] <- c(event, round)
      
      # Remove the items from row that we have used up
      row <- row[-c(1:length(const_cols))]
      
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
#' Tabulate judge information from given file
#' @param x pdf_data function output (list - assuming of length one since judge
#' pages are always length one for this sample)
#' @return data frame with num = judge number; name = judge name; and round =
#' round of diving judged
get_judges_df <- function(x) {
  
  # Find where the text "Function" is at the top of the page, and find last
  # instance of "Legend" at the bottom of the table
  top <- as.numeric(x[x$text == "Function", "y"])
  bottom <- unlist(x[x$text %in% c("Legend:", "Timekeeping"), "y"])
  bottom <- min(bottom)
  x <- x[x$y > top & x$y < bottom, ]
  
  # Looking for "Panels" so we know which rounds had which judges
  panels <- x[which(x$text == "Panel"), c("height", "y")]
  
  if (nrow(panels) > 0) {
    # Adding a buffer on the y-values in case rows are not exactly aligned 
    panels$min_y <- panels$y - panels$height/2
    panels$max_y <- panels$y + panels$height/2
    panels <- panels[order(panels$y), ]
    
    # Go through chunk of rows defined by each panel
    judges_table <- data.frame()
    for (i in 1:nrow(panels)) {
      
      if (i == nrow(panels)) { # If you are at the last panel, don't worry about
        # bottom limit
        x_panel <- x[which(x$y > as.numeric(panels[i, "min_y"])), ]
      } else {
        x_panel <- x[which(x$y > as.numeric(panels[i, "min_y"]) &
                             x$y < as.numeric(panels[i + 1, "min_y"])), ]
      }
      
      x_panel <- get_row_ranges_j(x_panel)
      # Paste together text if it belongs to the same row
      x_panel <- aggregate(text ~ y_range, data = x_panel, paste,
                           collapse = " ")
      
      # Separate out rounds information for this panel
      rounds <- x_panel[grepl("rounds", x_panel$text), 2]
      rounds <- gsub("[^0-9-]", "", rounds)
      rounds <- as.integer(unlist(strsplit(rounds, "-")))
      
      # The files for which I wrote this sometimes had different judges for odd
      # and even rounds (ie. Panel A: rounds 1-3-5), so here checking to see
      # if we have a range of rounds or a list
      if (length(rounds) == 2 & nrow(panels) < 3) { # we have a range
        rounds <- seq(rounds[1], rounds[2])
      } # otherwise leave it - judges skip rounds
      
      # Now extract judge names/numbers, collect into data frame
      judges <- x_panel[grepl("Judge|Alternate", x_panel$text), "text"]
      judges <- data.frame(num = gsub("[^0-9]", "", judges),
                           name = gsub("[^a-zA-Z ]", "", judges))
      judges$name <- gsub("Judge |Alternate ", "", judges$name)
      
      judges$num = ifelse(judges$num == "", "A", judges$num)
      
      rounds <- data.frame(round = rounds)
      judges <- merge(judges, rounds, by = c()) # repeat judges for each of
      # their rounds
      
      judges_table <- rbind(judges_table, judges)
      
    }
    
    return(judges_table)
    
  } else {
    
    x <- get_row_ranges_j(x)
    
    # Paste text together that has the same y range
    x <- aggregate(text ~ y_range, data = x, paste, collapse = " ")
    
    judges <- x[grepl("Judge|Alternate", x$text), "text"]
    judges <- data.frame(num = gsub("[^0-9]", "", judges),
                         name = gsub("[^a-zA-Z ]", "", judges))
    judges$name <- gsub("Judge |Alternate ", "", judges$name)
    judges$num = ifelse(judges$num == "", "A", judges$num)
    
    rounds <- data.frame(round = 1:6)
    judges <- merge(judges, rounds, by = c())
    
    return(judges)
    
  }
  
}


# -----------------------------------------------------------------------------
# Uses above utils functions to tabulate the detailed results page
tabulate_results <- function(x_full, event, round) {
  
  n_pages <- length(x_full)
  x_table <- data.frame()
  for (i in 1:n_pages){
    x <- x_full[[i]]
  
    # Filter to only useful information in this page
    if (i == n_pages){
      # Top of table starts with "Detailed Results" in English or French
      top <- as.numeric(max(x[grepl("Results|Résultats", x$text), "y"]))
      # Bottom of table ends before "Note:" on last page
      bottom <- as.numeric(x[grepl("Note:", x$text), "y"])
      x <- x[x$y > top & x$y < bottom, ]
    } else {
      # Top of table starts with "Detailed Results"
      top <- as.numeric(max(x[grepl("Results|Résultats", x$text), "y"]))
      # Bottom of table ends before "Official Timekeeping" on last page
      bottom <- as.numeric(x[grepl("Official", x$text), "y"])
      x <- x[x$y > top & x$y < bottom, ]
    }
    
    # Group text into y-ranges (approximately the rows)
    x <- get_row_ranges_r(x)
    
    # If this is an empty page, continue on
    if (is.character(x)) {
      next
    }
    
    # Combine text in the same y_range
    x <- x[order(x$x), ] # Important that data is sorted by x for next steps
    x <- split(x, x$y_range)
    x <- lapply(x, function(a) a$text)
    
    # Clean this into a table using helper function
    x <- get_results_df(x, event, round)
    x_table <- rbind(x_table, x)
    
  }
  
  return(x_table)
  
}


# -----------------------------------------------------------------------------
# Use utils functions above to get compiled information on judges for this round
# of this event
tabulate_judges <- function(x_full, event, round) {
  
  n_pages <- length(x_full)
  judges_all <- data.frame(event = character(),
                           round = character(),
                           divenum = numeric(),
                           J1 = character(),
                           J2 = character(),
                           J3 = character(),
                           J4 = character(),
                           J5 = character(),
                           J6 = character(),
                           J7 = character(),
                           JA = character())
  for (i in 1:n_pages){
    x <- x_full[[i]]
    judges <- get_judges_df(x)
    names(judges) <- c("num", "name", "divenum")
    judges$name <- trimws(judges$name)
    
    # Reshape to fit data frame requirements
    judges <- reshape(judges, idvar = "divenum", timevar= "num",
                      direction = "wide")
    names(judges) <- gsub("name.", "J", names(judges))
    judges$event <- event
    judges$round <- gsub("[^a-zA-Z]", "", unname(round))
    
    if ("JA" %in% names(judges)) {
      judges <- judges[, c("event", "round", "divenum", "J1", "J2", "J3", "J4",
                           "J5", "J6", "J7", "JA")]
    } else {
      judges <- judges[, c("event", "round", "divenum", "J1", "J2", "J3", "J4",
                           "J5", "J6", "J7")]
    }
    
    
    # Add to combined data frame
    judges_all <- rbind(judges_all, judges)
    
    # Filter out Women's final rounds with an extra row (women do 5 dives instead
    # of 6)
    judges_all <- judges_all[!(judges_all$event %in% c("W10mPF", "W3mSB") & 
                                 judges_all$divenum == 6), ]
    
  }
  
  judges_all$round <- round
  judges_all$event <- event
  
  return(judges_all)
  
}





rm(list = ls())

all_csv_files <- list.files("./detailed_results/", recursive = T)
all_csv_files <- paste0("./detailed_results/", 
                        all_csv_files)[-length(all_csv_files)]
f <- all_csv_files[1]

all_data <- data.frame()
for (f in all_csv_files) {
  print(paste("Processing", f))
  raw_data <- read.csv(f, as.is = T)
  
  # Check diving number
  dive_num_table <- table(raw_data$divenum)
  if ((length(dive_num_table) != 5) && (length(dive_num_table) != 6)) {
    print(paste("Contains", length(dive_num_table), "Dives"))
  }
  
  # Aggregate to check for dive number
  raw_data <- as.data.frame(raw_data)
  aggregated_divenum <- aggregate(divecode ~ rank + round + event + name, 
                                  data=raw_data, 
                                  length)
  
  women_wrong_divenum <- which(grepl("women", aggregated_divenum$event) != 
    (aggregated_divenum$divecode == 5))
  men_wrong_divenum <- which(!grepl("women", aggregated_divenum$event) != 
    (aggregated_divenum$divecode == 6))
  
  if (!identical(women_wrong_divenum, integer(0))) {
    print("Wrong Women Dive Number: ")
    print(aggregated_divenum[women_wrong_divenum, ])
  }
  if (!identical(men_wrong_divenum, integer(0))) {
    print("Wrong Men Dive Number: ")
    print(aggregated_divenum[men_wrong_divenum, ])
  }
  
  # Check for column types
  column_types <- sapply(raw_data, class)
  if ((sum(grepl("numeric", column_types)) != 10) && 
      (sum(grepl("numeric", column_types)) != 11)) {
    print("Incorrect Column Types: ")
    print(column_types)
  }
  
  # Check for duplicated
  dup_rows <- duplicated(raw_data)
  if (sum(dup_rows) != 0) {
    print("Duplicated Rows Round: ")
    print(raw_data[dup_rows, ])
  }
  
  # Check for country
  print(table(raw_data$dcountry))
  
  event_title <- gsub("_RESULTS.csv", "", gsub("./detailed_results/", "", f))
  year <- substr(event_title, 1, 4)
  event <- substr(event_title, 6, nchar(event_title))
  raw_data["year"] <- rep(year, nrow(raw_data))
  raw_data["competition"] <- rep(event, nrow(raw_data))
  all_data <- rbind(all_data, raw_data)
}

write.csv(all_data, "./detailed_results/all_results.csv", row.names = F)


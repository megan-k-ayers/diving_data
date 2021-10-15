# diving_data

### processing_pdfs.R file
This function utilizes **utils.R** to pull detailed results and panel of judges data from diving competition results PDF files. 

### utils.R file
This is a collection of helper functions that process diving data from PDF form into R data frames.

### detailed_results folder
This folder contains the scraped detailed results data from each competition (one file per competition).

### judges folder
This folder contains the scraped Panel of Judges information from each competition (one file per competition).

### data folder
This folder contains our raw data (large PDF files of diving competition results).

### QC_judges.R file
This file contains quality check and preprocessing scripts for all of the judges csv files in the judges folder. It outputs all_judges.csv in the judges folder.

### QC_results.R file
This file contains quality check and preprocessing scripts for all of the results csv files in the detailed_results folder. It outputs all_results.csv in the detailed_results folder. 

### merge_result_judge_country.R file
This file contains the script to merge the all_results.csv and all_judges.csv files, and outputs all_combined.csv files with score results in long format. 

### all_combined.csv file
This file contains all of the merged data between judges and diving results.

### preliminary_merge.R file
This file merges judges with divers from only Tokyo Olympic dataset.

### analysis.Rmd
The file contains scripts of analysis. 

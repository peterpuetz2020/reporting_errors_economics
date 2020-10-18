# statistics for flow diagramme (FIGURE 1) that are not calculated by other scripts;
# including the replication based estimations
# function to load all required packages and install them if necessary
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <-
    Required_Packages[!(Required_Packages %in% installed.packages()[, "Package"])]
  
  
  if (length(Remaining_Packages))
  {
    install.packages(Remaining_Packages)
    
  }
  for (package_name in Required_Packages)
  {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)
    
  }
}

# Specify the list of required packages to be installed and load
Required_Packages <-
  c("xtable")

# Call the Function
Install_And_Load(Required_Packages)

# compute how many of the non-verified tests are estimated to be errors
# read in non_ver
non_ver <- read.csv("data/non_verified.csv", header = TRUE, sep = ",")
# match with all data to get understated and overstated
# read in results without estimation
data_results <-
  read.csv("data/data_results_cleaned_complete.csv",
           header = TRUE,
           sep = ",")
data_results$error_id <-
  paste0(
    data_results$first_author,
    sep = "_",
    data_results$article_page,
    sep = "_",
    data_results$table_panel,
    sep = "_",
    data_results$row,
    sep = "_",
    data_results$column
  )
# merge data sets
non_ver$error_id <- gsub("ö", "oe", non_ver$error_id)
merge <-
  non_ver[(non_ver$error_id %in% as.character(data_results$error_id)), ]
merge <-
  data_results[as.character(data_results$error_id) %in% non_ver$error_id , ]

# how many over and understated among the non-verified tests?
ov_und <- c(sum(merge$prob1), sum(merge$prob2))

# multiply by estimated shares
false_det_rates <- read.table("data/shares_exp.txt")
round(ov_und * false_det_rates)

round(apply(ov_und * false_det_rates, 2, sum))

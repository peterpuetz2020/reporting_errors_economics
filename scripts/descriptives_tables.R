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
  c("xtable", "reporttools", "data.table", "tidyverse")

# Call the Function
Install_And_Load(Required_Packages)

# read in excel files with coded significances
data_coded <-
  read.csv(
    "data/only_eye_catchers_coded_complete.csv",
    skip = 1,
    header = TRUE,
    sep = ";",
    na.strings = c("NA", ""),
    colClasses = c(rep('factor', 12), rep('numeric', 6))
  )

# remove last column which includes comments
data_coded <- data_coded[, -ncol(data_coded)]

# read in variables from original file
variables <-
  read.csv(
    "data/only_eye_catchers_explanatory_variables.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    na.strings = c("NA", "")
  )

# merge datasets
data <- merge(data_coded, variables)

# remove redundant datasets
rm(data_coded, variables)

# restrict data to tests which have (coded) eye-catchers
data <- data[!is.na(data$rep_sign), ]

# remove some variables which are not of interest:
data <- select(data,-starts_with("D_"))

# van den Berg (2005) and Brown (2009, first page 197), table 4 also use one-sided tests, recode
data$type_emp[data$first_author == "van den Berg"] <- "one side"
data$type_emp[data$first_author == "Brown" &
                data$year == 2009 &
                data$article_page == 197 & data$table_panel == 4] <- "one side"

# hierarchy: if p value reported, take p-value, set remaining to NA
data$coefficient[!is.na(as.numeric(data$p_value))] <-
  data$standard_deviation[!is.na(as.numeric(data$p_value))] <-
  data$t_stat[!is.na(as.numeric(data$p_value))] <- NA
# if t statistic reported and p-value not, take t statistic, set remaining to NA
data$coefficient[!is.na(as.numeric(data$t_stat))] <-
  data$standard_deviation[!is.na(as.numeric(data$t_stat))] <- NA

# create paper id
data$paper_id <-
  paste0(data$first_author,
         sep = "_",
         data$year,
         sep = "_",
         data$article_page)

### recode table number
# split tables with panel indication such as 1_A
splitlist <- strsplit(as.character(data$table_panel), split = "_")

# extract only first entry which is the number of the table or the place in the appendix,
# we want to drop the indicator for the panel
table_num <- numeric(length(data$table_panel))
for (i in 1:length(data$table_panel))
{
  table_num[i] <- splitlist[[i]][1]
}
table(table_num)

# get maximum table number (numeric!) for each paper
data$table_num <- as.numeric(table_num)

# maximum table number as new variable
setDT(data)[, max_tab_by_article := max(table_num, na.rm = TRUE), by = paper_id]

## for appendix tables, add tables
data$table_num[table_num == "A"] <-
  data$max_tab_by_article[table_num == "A"] + 1
data$table_num[table_num == "B"] <-
  data$max_tab_by_article[table_num == "B"] + 2
data$table_num[table_num == "C"] <-
  data$max_tab_by_article[table_num == "C"] + 3
data$table_num[table_num == "D"] <-
  data$max_tab_by_article[table_num == "D"] + 4

# count number of tables
setDT(data)[, tab_by_article := length(unique(table_panel)), by = paper_id]
data <- as.data.frame(data)

# add created variable to data frame
data$table_panel <- as.numeric(data$table_num)

# remove old variable from dataset
data <- select(data,-c(table_num))

## table stats reported, split by journal
co_se <-
  ((!is.na(data$coefficient) & !is.na(data$standard_deviation)) * 1)
tz <- ((!is.na(data$t_stat)) * 1)
pv <- ((!is.na(data$p_value)) * 1)
stat_reported <-
  rbind(
    table(co_se, data$journal_id)[2, ],
    table(tz, data$journal_id)[2, ],
    table(pv, data$journal_id)[2, ]
  )

# add sums
stat_reported <- cbind(stat_reported, apply(stat_reported, 1, sum))

# add names
colnames(stat_reported) <- c("AER", "JPE", "QJE", "Total")
rownames(stat_reported) <- c(
  "Tests reported with coef. and se",
  "Tests reported with $t/z$-statistic",
  "Tests reported with $p$-value"
)

# create latex output for TABLE 1
stat_reported <- rbind(stat_reported, apply(stat_reported, 2, sum))

latex_output <-
  xtable(
    stat_reported,
    cap = "Distribution of reported statistical values",
    lab = "table:descriptives_general",
    align = c("L{5.5cm}R{1.5cm}R{1.5cm}R{1.5cm}R{1.5cm}")
  )
print.xtable(
  latex_output,
  hline.after = c(-1, 0, 3, 4),
  include.rownames = TRUE,
  table.placement = "t",
  caption.placement = "top",
  sanitize.text.function =   function(str)
    gsub("_", "\\_", str, fixed = TRUE)
)

# number of tests per paper
setDT(data)[, number_of_tests := length(table_panel), by = paper_id]

# group and rename categories of categorical variables
data$field <- recode_factor(data$field,
                            "Macro" = "Macroeconomics",
                            "Micro" = "Microeconomics")
data$negative_result <- recode_factor(
  data$negative_result,
  "Yes" = "Negative results: yes",
  "Nes" = "Negative results: yes",
  "No" = "Negative results: no"
)
data$model <- recode_factor(data$model,
                            "yes" = "With theoretical model",
                            "no" = "Without theoretical model")
data$data_availability <- recode_factor(data$data_availability,
                                        "Yes" = "Data available",
                                        "No" = "Data not available")
data$codes_availability <- recode_factor(data$codes_availability,
                                         "Yes" = "Code available",
                                         "No" = "Code not available")

# year is factor, transform to numeric
data$year <- as.numeric(data$year)

# reduce data set to paper level
data_reduced <- distinct(data, paper_id, .keep_all = TRUE)
data_reduced <- arrange(data_reduced, first_author)

# choose variables which are used
vars_paper_nominal <- c(
  "journal_id",
  "model",
  "codes_availability",
  "data_availability",
  "field",
  "negative_result"
)
vars_paper_cont <-
  c(
    "year",
    "num_authors",
    "editor_d",
    "tenured_0",
    "phd_age",
    "ras",
    "thanks",
    "tab_by_article",
    "number_of_tests"
  )

# choose only these variables in data set
des <- select(data_reduced, vars_paper_nominal)
des_cont <- select(data_reduced, vars_paper_cont)

des_test <- select(data, vars_paper_nominal)
des_cont_test <- select(data, vars_paper_cont)

#############################################################################################################
#############################################################################################################
# Creation of Table
#############################################################################################################
#############################################################################################################
#############################################################################################################

# tables of categorical variables at article level
no_articles <- rbind(
  (des %>% group_by (journal_id) %>% summarise(n = n()))[, 2],
  (des %>% group_by (field) %>% summarise(n = n()))[, 2],
  (des %>% group_by (negative_result) %>% summarise(n = n()))[, 2],
  (des %>% group_by (model) %>% summarise(n = n()))[, 2],
  (des %>% group_by (data_availability) %>% summarise(n = n()))[, 2],
  (des %>% group_by (codes_availability) %>% summarise(n = n()))[, 2]
)

# tables of categorical variables at test level
no_tests <- rbind(
  (des_test %>% group_by (journal_id) %>% summarise(n = n()))[, 2],
  (des_test %>% group_by (field) %>% summarise(n = n()))[, 2],
  (des_test %>% group_by (negative_result) %>% summarise(n = n()))[, 2],
  (des_test %>% group_by (model) %>% summarise(n = n()))[, 2],
  (des_test %>% group_by (data_availability) %>% summarise(n = n()))[, 2],
  (des_test %>% group_by (codes_availability) %>% summarise(n = n()))[, 2]
)

# combine these two data frame with two columns
nicetable <-
  data.frame("Number of articles" = no_articles, "Number of tests" = no_tests)
colnames(nicetable) <- c("Number of articles", "Number of tests")

# reorder colnames
colnames(des)
des <-
  des %>% select(journal_id,
                 field,
                 negative_result,
                 model,
                 data_availability,
                 codes_availability)

des <- as_tibble(des)

# get levels of variables
des <- des %>%
  gather(name, value, factor_key = TRUE) %>%  # reshape datset
  count(name, value) %>% select(-n)

# levels per variable
levels_per_var <- des %>% count(name) %>%
  select(n) %>% pull()

# these are the rownames
rown <- des %>%
  select(value) %>%
  pull()

# combine with table
nicetable <- cbind(rown, nicetable)

# no colnames for variable level
colnames(nicetable)[1] <- ""


##################################### output #######################
# TABLE 1: descriptives divided by journal
print.xtable(
  latex_output,
  hline.after = c(-1, 0, 3, 4),
  include.rownames = TRUE,
  table.placement = "t",
  caption.placement = "top",
  sanitize.text.function =   function(str)
    gsub("_", "\\_", str, fixed = TRUE)
)
# shares - how often are statistics reported
stat_reported[, 4] / (sum(stat_reported[, 4])/2)

# TABLE A3: discrete variables
latex_output_disc_paper <- xtable(as.data.frame(nicetable),
                                  cap = "Distribution of discrete variables over tests and articles", lab =
                                    "table:discrete")

# horizontal lines should be drawn after every variable
hlines <- cumsum(levels_per_var)

# output without NAs
print.xtable(
  latex_output_disc_paper,
  hline.after = c(-1, 0, hlines),
  caption.placement = "top",
  table.placement = "t",
  include.rownames = FALSE
)

## TABLE A4: table for continuous variables
# those varying only on paper level
colnames(des_cont) <-
  c(
    "Year",
    "Number of authors",
    "Share of editors among authors",
    "Share of tenured authors",
    "Authors' average years since PhD",
    "Number of research assistants thanked",
    "Number of individuals thanked",
    "Number of tables",
    "Number of tests"
  )

foo_cont = function() {
  print(
    tableContinuous(
      vars = des_cont,
      stats = c("n", "min", "q1", "mean", "median",
                "q3", "max", "s"),
      col.tit = c(
        "Variable",
        "n",
        "n",
        "Min",
        "Q1",
        "Mean",
        "Median",
        "Q3",
        "Max",
        "SD"
      ),
      longtable = TRUE,
      cap = "Continuous variables on article level",
      lab = "cont_pap",
      font.size = "normalsize",
      caption.placement = "top"
    )
  )
  
  #  print(tableNominal(vars=des_test,longtable=TRUE,cumsum = FALSE,cap = "categorical variables on test level"))
  # print(tableContinuous(vars = des_test_con, stats = c("n", "min", "q1", "mean", "median",
  #                                                      "q3", "max", "s"),col.tit = c("Variable","n","n", "Min", "Q1", "Mean", "Median",
  #                                                                                     "Q3", "Max", "SD"),cap = "Continuous variables on test level", longtable = TRUE,lab="cont_test", font.size = "normalsize"))
}

x <- foo_cont()

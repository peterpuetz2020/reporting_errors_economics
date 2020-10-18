# standard errors, t/z statistics or p-values with the p-values indicated by eye-catchers and table notes
# note that we compare here invervals of possible t-values as indicated by derounded standard error and coefficient or
# t/z statistics with intervals of possible t-values as indicated by eye-catchers. This is equivalent
# to comparing p-value intervals as described in the paper.

# should corrected data set without potentially wrongly detected errors from survey with response...
survey_considered = TRUE

# replications of don't know answers and tests without response considered
repl_considered = TRUE

# final rate: for the non-responses and dunnos without replication, a estimated shares of falsely detected
# understated and overstated significances (from replication_results.R) is assumed, i.e. the rate after considering
# the survey and the replications is corrected a bit downwards (especially for understated errors)
estimated_rate = TRUE

# robustness: zeros removed at the end of statistics
zero_removed = FALSE

# robustness check: trimmed decimals in reported statistics (accounts for potential wrong rounding)
trimmed_decimals = FALSE

# apply robustness checks only for completely revised file
if (zero_removed == TRUE |
    trimmed_decimals == TRUE &
    survey_considered == TRUE &
    repl_considered == TRUE & estimated_rate == TRUE)
  robustness = TRUE else
  robustness = FALSE


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
Required_Packages <- c("xtable", "openxlsx")

# Call the Function
Install_And_Load(Required_Packages)

# placeholder matrices to store results at test and article level
error_matrix_freq <- matrix(0, 2, 4)
error_matrix <- matrix(0, 2, 4)

# read in excel files with coded significance levels
data_coded <-
  read.csv(
    "data/only_eye_catchers_coded_complete.csv",
    skip = 1,
    header = TRUE,
    sep = ";",
    na.strings = c("NA", ""),
    colClasses = c(rep('factor', 12), rep('numeric', 6))
  )

# remove last columns and row full of zeros
data_coded <- data_coded[-nrow(data_coded), -ncol(data_coded)]

# read in variables from original file from Brodeur et al.
variables <-
  read.csv(
    "data/only_eye_catchers_explanatory_variables.csv",
    header = TRUE,
    sep = ";",
    dec = ",",
    na.strings = c("NA", "")
  )

# create unique identifier for first data set
data_coded$stat_id <-
  paste0(
    data_coded$first_author,
    sep = "_",
    data_coded$year,
    sep = "_",
    data_coded$article_page,
    sep = "_",
    data_coded$table_panel,
    sep = "_",
    data_coded$row,
    sep = "_",
    data_coded$column
  )

# is it really unique? yes!
data_coded[duplicated(data_coded$stat_id), ]

# create unique identifier for second data set
variables$stat_id <-
  paste0(
    variables$first_author,
    sep = "_",
    variables$year,
    sep = "_",
    variables$article_page,
    sep = "_",
    variables$table_panel,
    sep = "_",
    variables$row,
    sep = "_",
    variables$column
  )

# is it really unique? yes!
variables[duplicated(variables$stat_id), ]

# merge datasets
data <- merge(data_coded, variables)

# remove identifier
drops <- "stat_id"
data <- data[, !names(data) %in% drops]

# drop initial data sets
rm(data_coded, variables)

# restrict data to tests which have (coded) eye-catchers: Not all of the tests which were
# indicated to have eye-catchers by the respective variable of Brodeur et al. indeed had
# an eye-catcher which was defined by the table notes
data <- data[!is.na(data$rep_sign), ]

# remove some variables which are not of interest:
data <- data[,!grepl(names(data), pattern = "D_")]
data <- data[,!grepl(names(data), pattern = "weight_")]

# some p-values are given as <0.001: as 0.001 is the lowest threshold, we set them to 0 such
# that we have a numeric value to do the calculations with
data$p_value[data$p_value == "<0.001"] <- 0

# van den Berg (2005) and Brown (2009, first page 197), table 4 use one-sided tests, recode
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


# divide dataset to three datasets:
# 1. observations where only coefficient and standard error are reported
# 2. observations where test statistic but no p-value is reported
# 3. observations where p-value is reported
dat_coef_se <-
  data[which(c(
    !is.na(data$coefficient) &
      !is.na(data$standard_deviation) &
      is.na(data$t_stat) & is.na(data$p_value)
  )), ]
dat_zstat <-
  data[which(c(!is.na(data$t_stat) & is.na(data$p_value))), ]
dat_pval <- data[which(c(!is.na(data$p_value))), ]

####################################
# derounding functions
####################################

#------------------------
# derounding of coef/se (calculated t)
#------------------------

# this function determines the number of decimal places for each number
# it is important to use factors for the determination of the decimal length. Otherwise
# the 0s at the end get lost and we are more conservative when derounding than we need to be.
# note htat this function works only for one single number, but not for a vector
decpoint.length <- function(x) {
  xi <- as.character(gsub('\\s*$', '', x))
  pos <- gregexpr('\\.', xi)[[1]][1]
  if (pos < 0) {
    decpoints <-
      0
  } else {
    decpoints <- nchar(substr(xi, pos + 1, nchar(xi)), type = 'chars')
  }
  return(decpoints)
}

# which significance threshold combinations are there:
# if 15% threshold available, also 10% and 5% available and nothing else
table(data$S.15., data$S.10.)
table(data$S.15., data$S.5.)
table(data$S.15., data$S.1.)
table(data$S.15., data$S.0.1.)

# if 0.1% threshold available, always 1% and 5% available, sometimes also 10%
table(data$S.0.1., data$S.1.)
table(data$S.0.1., data$S.5.)
table(data$S.0.1., data$S.10.)

# all combinations exist for 10%, 5% and 1%
table(data$S.10., data$S.5., data$S.1.)
# for understanding: sum(data$S.10.==0 & data$S.5.==1 & data$S.1.==0) and sum(data$S.10.==0 & data$S.5.==0 & data$S.1.==1)


#############################################
# generic function to detect inconsistencies
# can be used if reported derounded statistics
# with minimum and maximum are available on the
# z/t statistic scale. the first commands are
# commented, the commands below work analogously
# Each paper reports significance levels either as 0.15, 0.1, 0.05, 0.01, or 0.001.
# We assume that a reported significance of 0.1 means that the reported statistical value
# is not significant at 0.05 (if there is also this threshold in the table notes).
# This means the authors always report the smallest significance level possible.
#############################################

find_inconsistency <-
  function (z.lrb,
            z.urb,
            type_emp,
            sig.level,
            S15,
            S10,
            S5,
            S1,
            S01) {
    # prob1 indicates an overstated significance level
    prob1 <- 0
    # prob2 indicates an understated significance level
    prob2 <- 0
    # prob3 indicates an inconsistency between the eye-catchers assigned to the reported
    # statistical values and the eye-catchers indicated by the table notes, e.g. two asterisks
    # are attached to a reported coeffient, but the table notes do not define two asterisks;
    # this allows to check for coding errors
    prob3 <- 0
    
    # for two sided tests:
    if (type_emp != "one side") {
      # for non-significant tests
      if (sig.level == 0) {
        # if there is the 15% significance level in the table notes
        if (S15 == 1) {
          # if the lower rounding bound of the derounded z/t-value is larger than the critical
          # value on the 15%-level, then we have an understated significance
          if (z.lrb > cv15) {
            prob2 <- 1
          }
          
        }
        
        # if there is the 10% but not the 15% significance level in the table notes
        if (S15 == 0 & S10 == 1) {
          # if the lower rounding bound of the derounded z/t-value is bigger than the critical
          # value on the 10%-level, then we have an understated significance
          if (z.lrb > cv10) {
            prob2 <- 1
          }
          
        }
        
        # see above
        if (S15 == 0 & S10 == 0 & S5 == 1) {
          if (z.lrb > cv5) {
            prob2 <- 1
          }
          
        }
        
        if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 1) {
          if (z.lrb > cv1) {
            prob2 <- 1
          }
          
        }
        
        if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 0 & S01 == 1) {
          if (z.lrb > cv01) {
            prob2 <- 1
          }
          
        }
        
      }
      
      
      # 15% level of significance indicated by eye-catchers (SPECIAL CASE: if 0.15 is indicated in the table notes, 0.1 is as well, see statistics above)
      if (sig.level == 0.15) {
        # check whether the reported significance level of 0.15 is indicated in table notes
        if (S15 == 0) {
          prob3 <- 1
        }
        
        # if the lower rounding bound of the derounded z/t-value is bigger than the critical
        # value on the 10%-level, then we have an understated significance
        if (z.lrb > cv10) {
          prob2 <- 1
        }
        
        # if the upper rounding bound of the derounded z/t-value is smaller than the critical
        # value on the 15%-level, then we have an overstated significance
        if (z.urb <= cv15) {
          prob1 <- 1
        }
        
      }
      
      # 10% level of significance indicated by eye-catchers, see above for an explanation of the following commands
      if (sig.level == 0.1) {
        if (S10 == 0) {
          prob3 <- 1
        }
        
        if (S5 == 1) {
          if (z.lrb > cv5) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S1 == 1) {
          if (z.lrb > cv1) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S01 == 1) {
          if (z.lrb > cv5) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S1 == 0 & S01 == 0) {
          if (z.urb <= cv10) {
            prob1 <- 1
          }
        }
      }
      
      # 5% level of significance indicated by eye-catchers
      if (sig.level == 0.05) {
        if (S5 == 0) {
          prob3 <- 1
        }
        if (S1 == 1) {
          if (z.lrb > cv1) {
            prob2 <- 1
          }
          
          if (z.urb <= cv5) {
            prob1 <- 1
          }
        }
        
        if (S1 == 0 & S01 == 1) {
          if (z.lrb > cv01) {
            prob2 <- 1
          }
          
          if (z.urb <= cv5) {
            prob1 <- 1
          }
        }
        
        if (S1 == 0 & S01 == 0) {
          if (z.urb <= cv5) {
            prob1 <- 1
          }
        }
      }
      
      
      #1% level of significance indicated by eye-catchers
      
      if (sig.level == 0.01) {
        if (S1 == 0) {
          prob3 <- 1
        }
        if (S01 == 1) {
          if (z.lrb > cv01) {
            prob2 <- 1
          }
          
          if (z.urb <= cv1) {
            prob1 <- 1
          }
        }
        
        if (S01 == 0) {
          if (z.urb <= cv1) {
            prob1 <- 1
          }
        }
      }
      
      # 0.1% level of significance indicated by eye-catchers
      if (sig.level == 0.001) {
        if (S01 == 0) {
          prob3 <- 1
        }
        
        if (z.urb <= cv01) {
          prob1 <- 1
        }
        
      }
      
      # for one sided tests: everything equivalent to two sided tests as described above
      # except for the critical values
    }	else {
      # non-significant tests
      if (sig.level == 0) {
        if (S15 == 1) {
          if (z.lrb > cv15_os) {
            prob2 <- 1
          }
          
        }
        
        if (S15 == 0 & S10 == 1) {
          if (z.lrb > cv10_os) {
            prob2 <- 1
          }
          
        }
        
        if (S15 == 0 & S10 == 0 & S5 == 1) {
          if (z.lrb > cv5_os) {
            prob2 <- 1
          }
          
          if (z.urb > cv5_os) {
            z.urb <- cv5_os
          }
        }
        
        if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 1) {
          if (z.lrb > cv1_os) {
            prob2 <- 1
          }
          
        }
        
        if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 0 & S01 == 1) {
          if (z.lrb > cv01_os) {
            prob2 <- 1
          }
          
        }
        
      }
      
      # 15% level of significance indicated by eye-catchers (SPECIAL CASE: if 0.15 is used then the tables always use 0.1 as well)
      if (sig.level == 0.15) {
        if (S15 == 0) {
          prob3 <- 1
        }
        
        if (z.lrb > cv10_os) {
          prob2 <- 1
        }
        
        if (z.urb <= cv15_os) {
          prob1 <- 1
        }
      }
      
      # 10% level of significance indicated by eye-catchers
      if (sig.level == 0.1) {
        if (S10 == 0) {
          prob3 <- 1
        }
        if (S5 == 1) {
          if (z.lrb > cv5_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10_os) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S1 == 1) {
          if (z.lrb > cv1_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10_os) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S01 == 1) {
          if (z.lrb > cv01_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv10_os) {
            prob1 <- 1
          }
        }
        
        if (S5 == 0 & S1 == 0 & S01 == 0) {
          if (z.urb <= cv10_os) {
            prob1 <- 1
          }
        }
      }
      
      # 5% level of significance indicated by eye-catchers
      if (sig.level == 0.05) {
        if (S5 == 0) {
          prob3 <- 1
        }
        if (S1 == 1) {
          if (z.lrb > cv1_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv5_os) {
            prob1 <- 1
          }
        }
        
        if (S1 == 0 & S01 == 1) {
          if (z.lrb > cv01_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv5_os) {
            prob1 <- 1
          }
        }
        
        if (S1 == 0 & S01 == 0) {
          if (z.urb <= cv5_os) {
            prob1 <- 1
          }
        }
      }
      
      # 1% level of significance indicated by eye-catchers
      if (sig.level == 0.01) {
        if (S1 == 0) {
          prob3 <- 1
        }
        if (S01 == 1) {
          if (z.lrb > cv01_os) {
            prob2 <- 1
          }
          
          if (z.urb <= cv1_os) {
            prob1 <- 1
          }
        }
        
        if (S01 == 0) {
          if (z.urb <= cv1_os) {
            prob1 <- 1
          }
        }
      }
      
      # 0.1% level of significance indicated by eye-catchers
      if (sig.level == 0.001) {
        if (S01 == 0) {
          prob3 <- 1
        }
        
        if (z.urb <= cv01_os) {
          prob1 <- 1
        }
        
      }
      
      
    }
    
    # return type(s) of error if present
    result <- c(prob1, prob2,  prob3)
    return(result)
    
  }

#############################################
# generic function to detect strong reporting
# errors. similar to find_inconsistency above
# but tests are only flagged if the significant
# statement changes, i.e. from significant to
# non-significant or vice versa
#############################################

find_inconsistency_strong <-
  function (z.lrb,
            z.urb,
            type_emp,
            sig.level,
            S15,
            S10,
            S5,
            S1,
            S01) {
    prob1 <- 0
    prob2 <- 0
    prob3 <- 0
    
    # store the lowest significant threshold (i.e. lowest z/t-value) the table notes indicate
    lowest_rep_threshold <- NA
    # if there is an eye-catcher for 15% in the table notes, then the corresponding z/t-value
    # is the lowest threshold reported
    if (S15 == 1)
    {
      # for two-sided tests
      lowest_rep_threshold <- cv15
      # for one-sided tests
      lowest_rep_threshold_os <- cv15_os
    }
    # if there is an eye-catcher for 1% in the table notes, but none for 15%, then the corresponding z/t-value
    # is the lowest threshold reported
    if (S15 == 0 & S10 == 1)
    {
      lowest_rep_threshold <- cv10
      lowest_rep_threshold_os <- cv10_os
    }
    # the rest is equivalent
    if (S15 == 0 & S10 == 0 & S5 == 1)
    {
      lowest_rep_threshold <- cv5
      lowest_rep_threshold_os <- cv5_os
    }
    if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 1)
    {
      lowest_rep_threshold <- cv1
      lowest_rep_threshold_os <- cv1_os
    }
    if (S15 == 0 & S10 == 0 & S5 == 0 & S1 == 0 & S01 == 1)
    {
      lowest_rep_threshold <- cv01
      lowest_rep_threshold_os <- cv01_os
    }
    
    # detect strong reporting errors for two-sided tests
    if (type_emp != "one side") {
      if (sig.level == 0) {
        if (z.lrb > lowest_rep_threshold) {
          prob2 <- 1
        }
        
      }
      
      
      if (sig.level != 0) {
        if (z.urb <= lowest_rep_threshold) {
          prob1 <- 1
        }
        
        if (sig.level == 0.15) {
          if (S15 == 0) {
            prob3 <- 1
          }
        }
        
        
        if (sig.level == 0.1) {
          if (S10 == 0) {
            prob3 <- 1
          }
          
        }
        
        
        if (sig.level == 0.05) {
          if (S5 == 0) {
            prob3 <- 1
          }
          
        }
        
        if (sig.level == 0.01) {
          if (S1 == 0) {
            prob3 <- 1
          }
          
        }
        
        
        if (sig.level == 0.001) {
          if (S01 == 0) {
            prob3 <- 1
          }
          
          
        }
      }
      
    }	else {
      # detect strong reporting errors for one-sided tests
      
      if (sig.level == 0) {
        if (z.lrb > lowest_rep_threshold_os) {
          prob2 <- 1
        }
        
      }
      
      
      if (sig.level != 0) {
        if (z.urb <= lowest_rep_threshold_os) {
          prob1 <- 1
        }
        
        if (sig.level == 0.15) {
          if (S15 == 0) {
            prob3 <- 1
          }
        }
        
        
        if (sig.level == 0.1) {
          if (S10 == 0) {
            prob3 <- 1
          }
          
        }
        
        
        if (sig.level == 0.05) {
          if (S5 == 0) {
            prob3 <- 1
          }
          
        }
        
        if (sig.level == 0.01) {
          if (S1 == 0) {
            prob3 <- 1
          }
          
        }
        
        
        if (sig.level == 0.001) {
          if (S01 == 0) {
            prob3 <- 1
          }
          
          
        }
        
        
        
      }
    }
    
    # return type(s) of error if present
    result <- c(prob1, prob2, prob3)
    return(result)
    
  }




###############################################################################################
# This is the error detection function for tests if only coefficient and standard error are reported
# (and not t/z-value or p-value) aside from the eye-catcher.
# The function needs coeffient ("coefi") and standard error ("se"), both numeric and as factor (ending ".fac"),
# the test type employed (one or two-sided, "test_emp"), the reported significance level given by the eye-catchers
# ("sig.level") and knowledge about which significance thresholds are given in the table notes ("S15",..,"S01")
# The function uses the find_inconsistency function from above to indicate whether the derounding interval
# consistent with the reported statistical values
# does not overlap with the interval corresponding to the eye-catcher and the table notes.
# This first function is well documented and the remaining functions are build analogously and are not documented.
#################################################################################################
rep_error_detection_coef_se <-
  function(coefi,
           coefi.fac,
           se,
           se.fac,
           type_emp,
           sig.level,
           S15,
           S10,
           S5,
           S1,
           S01) {
    # take absolute value of coefficient
    coefi <- abs(coefi)
    
    # prob1: placeholder for overstated significance levels
    # prob2: placeholder for understated significance levels
    # prob3: placeholder for reported significance levels which are defined in table notes (only for cross-checking our own coding)
    prob1 <- rep(0, length(coefi))
    prob2 <- rep(0, length(coefi))
    prob3 <- rep(0, length(coefi))
    
    #rounding bounds
    z.lrb <- NULL
    z.urb <- NULL
    
    # placeholder matrix for derounding bounds
    bounds_coefse <- matrix(0, length(coefi), 2)
    colnames(bounds_coefse) <-
      c("lower_derounding_bound", "upper_derounding_bound")
    
    # placeholder matrix to store results, i.e. (strong) error types
    results <- matrix(0, length(coefi), 3)
    colnames(results) <- c("prob1", "prob2", "prob3")
    results_strong <- matrix(0, length(coefi), 3)
    colnames(results_strong) <- c("prob1", "prob2", "prob3")
    
    # if there is any test with reported coefficient (implies that there is also a standard error,
    # as they are always reported together according to our hierarchy, see above)...
    if (length(coefi) > 0)
      
      # ...loop over all reported coefficients/standard errors
      for (i in 1:length(coefi)) {
        # for robustness check 1 which removes zeros at the end of the reported statistics
        if (zero_removed == TRUE)
        {
          # remove zeros at the end
          # make difference between only one decimal...
          if (substr(coefi.fac[i], nchar(as.character(coefi.fac[i])), nchar(as.character(coefi.fac[i]))) ==
              "0" & decpoint.length(coefi.fac[i]) == 1)
          {
            # new level of factor coefi.fac does not have zero at the end as the numeric version coefi does not have
            levels(coefi.fac) <-
              c(levels(coefi.fac), as.character(coefi[i]))
            coefi.fac[i] <- (coefi[i])
          }
          #  ... and more decimals
          if (substr(coefi.fac[i], nchar(as.character(coefi.fac[i])), nchar(as.character(coefi.fac[i]))) ==
              "0" & decpoint.length(coefi.fac[i]) > 1)
          {
            # just cut the zero
            levels(coefi.fac) <-
              c(levels(coefi.fac), (substr(
                coefi.fac[i], 1, nchar(as.character(coefi.fac[i])) - 1
              )))
            coefi.fac[i] <-
              as.factor(substr(coefi.fac[i], 1, nchar(as.character(coefi.fac[i])) - 1))
          }
          
          # do the same again as there might be another zero at the end (does not make a difference regarding the results)
          if (substr(coefi.fac[i], nchar(as.character(coefi.fac[i])), nchar(as.character(coefi.fac[i]))) ==
              "0" & decpoint.length(coefi.fac[i]) == 1)
          {
            levels(coefi.fac) <- c(levels(coefi.fac), as.character(coefi[i]))
            coefi.fac[i] <- (coefi[i])
          }
          if (substr(coefi.fac[i], nchar(as.character(coefi.fac[i])), nchar(as.character(coefi.fac[i]))) ==
              "0" & decpoint.length(coefi.fac[i]) > 1)
          {
            levels(coefi.fac) <-
              c(levels(coefi.fac), (substr(
                coefi.fac[i], 1, nchar(as.character(coefi.fac[i])) - 1
              )))
            coefi.fac[i] <-
              as.factor(substr(coefi.fac[i], 1, nchar(as.character(coefi.fac[i])) - 1))
          }
          
          # same for standard errors
          if (substr(se.fac[i], nchar(as.character(se.fac[i])), nchar(as.character(se.fac[i]))) ==
              "0" & decpoint.length(se.fac[i]) == 1)
          {
            levels(se.fac) <- c(levels(se.fac), as.character(se[i]))
            se.fac[i] <- (se[i])
          }
          if (substr(se.fac[i], nchar(as.character(se.fac[i])), nchar(as.character(se.fac[i]))) ==
              "0" & decpoint.length(se.fac[i]) > 1)
          {
            levels(se.fac) <-
              c(levels(se.fac), (substr(
                se.fac[i], 1, nchar(as.character(se.fac[i])) - 1
              )))
            se.fac[i] <-
              as.factor(substr(se.fac[i], 1, nchar(as.character(se.fac[i])) - 1))
          }
          
          # do the same again as there might be another zero at the end (does not make a difference regarding the results)
          if (substr(se.fac[i], nchar(as.character(se.fac[i])), nchar(as.character(se.fac[i]))) ==
              "0" & decpoint.length(se.fac[i]) == 1)
          {
            levels(se.fac) <- c(levels(se.fac), as.character(se[i]))
            se.fac[i] <- (se[i])
          }
          if (substr(se.fac[i], nchar(as.character(se.fac[i])), nchar(as.character(se.fac[i]))) ==
              "0" & decpoint.length(se.fac[i]) > 1)
          {
            levels(se.fac) <-
              c(levels(se.fac), (substr(
                se.fac[i], 1, nchar(as.character(se.fac[i])) - 1
              )))
            se.fac[i] <-
              as.factor(substr(se.fac[i], 1, nchar(as.character(se.fac[i])) - 1))
          }
        }
        
        
        # number of decimals of reported coefficient
        coefi.decimal <- decpoint.length(coefi.fac[i])
        
        ## compute min and max of (de-rounded) coefficient
        # for robustness check 2, namely potentially trimmed decimals...
        if (trimmed_decimals == TRUE)
          
          # ...account for potentially wrong rounding / trimmed decimals
          coefi.max <-
          coefi[i] + as.numeric(paste0(0, ".", strrep(c("0"), coefi.decimal), 9999999999)) else
          
          # otherwise account for correct rounding which might have occurred
          coefi.max <-
          coefi[i] + as.numeric(paste0(0, ".", strrep(c("0"), coefi.decimal), 4999999999))
        
        coefi.min <-
          coefi[i] - as.numeric(paste0(0, ".", strrep(c("0"), coefi.decimal), 5))
        
        # number of decimals of reported standard error
        se.decimal <- decpoint.length(se.fac[i])
        
        ## compute min and max of (de-rounded) coefficient
        # for robustness check 2, namely potentially trimmed decimals...
        if (trimmed_decimals == TRUE)
          
          # accounting for potentially wrong rounding / trimmed decimals
          se.max <-
          se[i] + as.numeric(paste0(0, ".", strrep(c("0"), se.decimal), 9999999999)) else
          # otherwise account for correct rounding which might have occurred
          se.max <-
          se[i] + as.numeric(paste0(0, ".", strrep(c("0"), se.decimal), 4999999999))
        
        se.min <-
          se[i] - as.numeric(paste0(0, ".", strrep(c("0"), se.decimal), 5))
        
        # If the absolute value of the coefficient is reported as 0 or 0.0 etc. (-0, -0.0 works equivalently as we take the absolute
        # value, see command 	coefi <- abs(coefi) above), then the minimum coefi.min becomes negative.
        # Therefore, we can assume that for these coefs the minimum is 0.
        if (coefi.min < 0) {
          coefi.min <- 0
        }
        
        # standard error is by definition positive
        if (se.min <= 0) {
          se.min <- 0.00000000000000000000000001
        }
        
        # lower rounding bound
        z.lrb[i] <- coefi.min / se.max
        
        # upper rounding bound
        z.urb[i] <- coefi.max / se.min
        
        #detect (strong) reporting errors, see functions above, store result in matrix
        results[i, ] <-
          find_inconsistency(z.lrb[i],
                             z.urb[i],
                             type_emp[i],
                             sig.level[i],
                             S15[i],
                             S10[i],
                             S5[i],
                             S1[i],
                             S01[i])
        results_strong[i, ] <-
          find_inconsistency_strong(z.lrb[i],
                                    z.urb[i],
                                    type_emp[i],
                                    sig.level[i],
                                    S15[i],
                                    S10[i],
                                    S5[i],
                                    S1[i],
                                    S01[i])
        
        # store rounding bounds
        bounds_coefse[i, ] <- c(z.lrb[i], z.urb[i])
      }
    
    # return data frame with results and rounding bounds
    list(data.frame(results),
         data.frame(results_strong),
         data.frame(bounds_coefse))
    
  }

# this is the error detection function function for tests if t/z-value (but not the p-value) are reported aside the eye-catcher.
# see above for more explanations
rep_error_detection_z_t <-
  function(z,
           z.fac,
           type_emp,
           sig.level,
           S15,
           S10,
           S5,
           S1,
           S01) {
    z <- abs(z)
    prob1 <- rep(0, length(z))
    prob2 <- rep(0, length(z))
    prob3 <- rep(0, length(z))
    z.lrb <- NULL
    z.urb <- NULL
    
    # placeholder matrix to store results
    results <- matrix(0, length(z), 3)
    colnames(results) <- c("prob1", "prob2", "prob3")
    results_strong <- matrix(0, length(z), 3)
    colnames(results_strong) <- c("prob1", "prob2", "prob3")
    
    # placeholder matrix to store lower and upper rounding bound
    bounds_z <- matrix(0, length(z), 2)
    colnames(bounds_z) <-
      c("lower_derounding_bound", "upper_derounding_bound")
    
    if (length(z) > 0)
      for (i in 1:length(z)) {
        # robustness check: remove zeros
        if (zero_removed == TRUE)
        {
          if (substr(z.fac[i], nchar(as.character(z.fac[i])), nchar(as.character(z.fac[i]))) ==
              "0" & decpoint.length(z.fac[i]) == 1)
          {
            levels(z.fac) <- c(levels(z.fac), as.character(z[i]))
            z.fac[i] <- (z[i])
          }
          if (substr(z.fac[i], nchar(as.character(z.fac[i])), nchar(as.character(z.fac[i]))) ==
              "0" & decpoint.length(z.fac[i]) > 1)
          {
            levels(z.fac) <-
              c(levels(z.fac), (substr(
                z.fac[i], 1, nchar(as.character(z.fac[i])) - 1
              )))
            z.fac[i] <-
              as.factor(substr(z.fac[i], 1, nchar(as.character(z.fac[i])) - 1))
          }
          
          if (substr(z.fac[i], nchar(as.character(z.fac[i])), nchar(as.character(z.fac[i]))) ==
              "0" & decpoint.length(z.fac[i]) == 1)
          {
            levels(z.fac) <- c(levels(z.fac), as.character(z[i]))
            z.fac[i] <- (z[i])
          }
          if (substr(z.fac[i], nchar(as.character(z.fac[i])), nchar(as.character(z.fac[i]))) ==
              "0" & decpoint.length(z.fac[i]) > 1)
          {
            levels(z.fac) <-
              c(levels(z.fac), (substr(
                z.fac[i], 1, nchar(as.character(z.fac[i])) - 1
              )))
            z.fac[i] <-
              as.factor(substr(z.fac[i], 1, nchar(as.character(z.fac[i])) - 1))
          }
        }
        
        # number of decimals of reported z/t-value
        z.decimal <- decpoint.length(z.fac[i])
        
        # robustness check trimmed decimals
        if (trimmed_decimals == TRUE)
          
          # account for potentially wrong de-rounding
          z.max <-
          z[i] + as.numeric(paste0(0, ".", strrep(c("0"), z.decimal), 9999999999)) else
          
          # account for potential (correct) de-rounding
          z.max <-
          z[i] + as.numeric(paste0(0, ".", strrep(c("0"), z.decimal), 4999999999))
        
        z.min <-
          z[i] - as.numeric(paste0(0, ".", strrep(c("0"), z.decimal), 5))
        
        # if the absolute value of z/t is reported as zero, the minimum becomes negatives, so we can
        # truncate at zero
        if (z.min < 0) {
          z.min <- 0
        }
        
        # lower rounding bound
        z.lrb[i] <- z.min
        
        # upper rounding bound
        z.urb[i] <- z.max
        
        # detect (strong) reporting errors, see functions above, store result in matrix
        results[i, ] <-
          find_inconsistency(z.lrb[i],
                             z.urb[i],
                             type_emp[i],
                             sig.level[i],
                             S15[i],
                             S10[i],
                             S5[i],
                             S1[i],
                             S01[i])
        results_strong[i, ] <-
          find_inconsistency_strong(z.lrb[i],
                                    z.urb[i],
                                    type_emp[i],
                                    sig.level[i],
                                    S15[i],
                                    S10[i],
                                    S5[i],
                                    S1[i],
                                    S01[i])
        
        # store lower and upper rounding bound in matrix
        bounds_z[i, ] <- c(z.lrb[i], z.urb[i])
        
      }
    
    # return data frame
    list(data.frame(results),
         data.frame(results_strong),
         data.frame(bounds_z))
    
  }

# this is the error detection function function for tests if the p-value is reported aside the eye-catcher.
# the comparison is easier, as the derounded p-value can immediately be compared to the
# the interval corresponding to the eye-catcher and the table notes. That is why this function
# does not use the find_inconsistency function from above but has its own code to detect
# reporting errors which works analogously.
# see above for more explanations.
rep_error_detection_p <-
  function(p, p.fac, sig.level, S15, S10, S5, S1, S01) {
    p <- abs(p)
    prob1 <- rep(0, length(p))
    prob2 <- rep(0, length(p))
    prob3 <- rep(0, length(p))
    p.lb <- rep(0, length(p))
    p.ub <- rep(0, length(p))
    p.min <- rep(0, length(p))
    p.max <- rep(0, length(p))
    
    # placeholder matrix to store lower and upper rounding bound
    bounds_p <- matrix(0, length(p), 2)
    colnames(bounds_p) <-
      c("lower_derounding_bound", "upper_derounding_bound")
    
    if (length(p) > 0)
      for (i in 1:length(p)) {
        # robustness check: zeros removed
        if (zero_removed == TRUE)
        {
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) == 1)
          {
            levels(p.fac) <- c(levels(p.fac), as.character(z[i]))
            p.fac[i] <- (z[i])
          }
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) > 1)
          {
            levels(p.fac) <-
              c(levels(p.fac), (substr(
                p.fac[i], 1, nchar(as.character(p.fac[i])) - 1
              )))
            p.fac[i] <-
              as.factor(substr(p.fac[i], 1, nchar(as.character(p.fac[i])) - 1))
          }
          
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) == 1)
          {
            levels(p.fac) <- c(levels(p.fac), as.character(z[i]))
            p.fac[i] <- (z[i])
          }
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) > 1)
          {
            levels(p.fac) <-
              c(levels(p.fac), (substr(
                p.fac[i], 1, nchar(as.character(p.fac[i])) - 1
              )))
            p.fac[i] <-
              as.factor(substr(p.fac[i], 1, nchar(as.character(p.fac[i])) - 1))
          }
        }
        
        ## de-rounding bounds
        p.decimal <- decpoint.length(p.fac[i])
        
        # robustness check: accounting for potentially wrong rounding / trimming
        if (trimmed_decimals == TRUE)
          p.max[i] <-
          p[i] + as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 9999999999)) else
          
          # accounting for potential (correct) rounding
          p.max[i] <-
          p[i] + as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 4999999999))
        
        p.min[i] <-
          p[i] - as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 5))
        
        # p-value has to be between 0 and 1
        if (p.min[i] <= 0) {
          p.min[i] <- 0.0000000001
        }
        if (p.max[i] >= 1) {
          p.max[i] <- 0.9999999999
        }
        
        # store bounds
        p.ub[i] <- p.max[i]
        p.lb[i] <- p.min[i]
        bounds_p[i, ] <- c(p.lb[i], p.ub[i])
        
        ######## now reporting error detection algorihm for reported p-values, analogous to those described above ######
        # non-significant results as indicated by eye-catchers
        if (sig.level[i] == 0) {
          if (S15[i] == 1) {
            if (p.ub[i] < 0.15) {
              prob2[i] <- 1
            }
            
          }
          
          if (S15[i] == 0 & S10[i] == 1) {
            if (p.ub[i] < 0.1) {
              prob2[i] <- 1
            }
          }
          
          if (S15[i] == 0 & S10[i] == 0 & S5[i] == 1) {
            if (p.ub[i] < 0.05) {
              prob2[i] <- 1
            }
            
          }
          
          if (S15[i] == 0 & S10[i] == 0 & S5[i] == 0 & S1[i] == 1) {
            if (p.ub[i] < 0.01) {
              prob2[i] <- 1
            }
            
          }
          
          if (S15[i] == 0 &
              S10[i] == 0 & S5[i] == 0 & S1[i] == 0 & S01[i] == 1) {
            if (p.ub[i] < 0.001) {
              prob2[i] <- 1
            }
            
          }
          
        }
        
        # 15% level of significance as indicated by eye-catchers (never reported, check via table(S15))
        
        # 10% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.1) {
          if (S10[i] == 0) {
            prob3[i] <- 1
          }
          if (S5[i] == 1) {
            if (p.ub[i] < 0.05) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.1) {
              prob1[i] <- 1
            }
          }
          
          if (S5[i] == 0 & S1[i] == 1) {
            if (p.ub[i] < 0.01) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.1) {
              prob1[i] <- 1
            }
          }
          
          if (S5[i] == 0 & S01[i] == 1) {
            if (p.ub[i] < 0.001) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.1) {
              prob1[i] <- 1
            }
          }
          
          if (S5[i] == 0 & S1[i] == 0 & S01[i] == 0) {
            if (p.lb[i] >= 0.1) {
              prob1[i] <- 1
            }
          }
        }
        
        # 5% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.05) {
          if (S5[i] == 0) {
            prob3[i] <- 1
          }
          if (S1[i] == 1) {
            if (p.ub[i] < 0.01) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.05) {
              prob1[i] <- 1
            }
            
          }
          
          if (S1[i] == 0 & S01[i] == 1) {
            if (p.ub[i] < 0.001) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.05) {
              prob1[i] <- 1
            }
            
          }
          
          if (S1[i] == 0 & S01[i] == 0) {
            if (p.lb[i] >= 0.05) {
              prob1[i] <- 1
            }
            
          }
        }
        
        
        # 1% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.01) {
          if (S1[i] == 0) {
            prob3[i] <- 1
          }
          if (S01[i] == 1) {
            if (p.ub[i] < 0.001) {
              prob2[i] <- 1
            }
            
            if (p.lb[i] >= 0.01) {
              prob1[i] <- 1
            }
            
          }
          
          if (S01[i] == 0) {
            if (p.lb[i] >= 0.01) {
              prob1[i] <- 1
            }
            
            
          }
        }
        
        
        # 0.1% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.001) {
          if (S01[i] == 0) {
            prob3[i] <- 1
          }
          
          if (p.lb[i] >= 0.001) {
            prob1[i] <- 1
          }
          
        }
        
        
      }
    
    list(data.frame(prob1, prob2, prob3), data.frame(bounds_p))
  }


##### same for strong reporting errors #####
rep_error_detection_p_strong <-
  function(p, p.fac, sig.level, S15, S10, S5, S1, S01) {
    p <- abs(p)
    prob1 <- rep(0, length(p))
    prob2 <- rep(0, length(p))
    prob3 <- rep(0, length(p))
    p.lb <- rep(0, length(p))
    p.ub <- rep(0, length(p))
    p.min <- rep(0, length(p))
    p.max <- rep(0, length(p))
    lowest_rep_threshold <- rep(0, length(p))
    if (length(p) > 0)
      for (i in 1:length(p)) {
        # robustness check: zeros removed
        if (zero_removed == TRUE)
        {
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) == 1)
          {
            levels(p.fac) <- c(levels(p.fac), as.character(z[i]))
            p.fac[i] <- (z[i])
          }
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) > 1)
          {
            levels(p.fac) <-
              c(levels(p.fac), (substr(
                p.fac[i], 1, nchar(as.character(p.fac[i])) - 1
              )))
            p.fac[i] <-
              as.factor(substr(p.fac[i], 1, nchar(as.character(p.fac[i])) - 1))
          }
          
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) == 1)
          {
            levels(p.fac) <- c(levels(p.fac), as.character(z[i]))
            p.fac[i] <- (z[i])
          }
          if (substr(p.fac[i], nchar(as.character(p.fac[i])), nchar(as.character(p.fac[i]))) ==
              "0" & decpoint.length(p.fac[i]) > 1)
          {
            levels(p.fac) <-
              c(levels(p.fac), (substr(
                p.fac[i], 1, nchar(as.character(p.fac[i])) - 1
              )))
            p.fac[i] <-
              as.factor(substr(p.fac[i], 1, nchar(as.character(p.fac[i])) - 1))
          }
        }
        
        #Min and max
        p.decimal <- decpoint.length(p.fac[i])
        if (trimmed_decimals == TRUE)
          p.max[i] <-
          p[i] + as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 9999999999)) else
          p.max[i] <-
          p[i] + as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 4999999999))
        
        p.min[i] <-
          p[i] - as.numeric(paste0(0, ".", strrep(c("0"), p.decimal), 5))
        
        if (p.min[i] <= 0) {
          p.min[i] <- 0.0000000001
        }
        if (p.max[i] >= 1) {
          p.max[i] <- 0.9999999999
        }
        
        p.ub[i] <- p.max[i]
        p.lb[i] <- p.min[i]
        
        lowest_rep_threshold[i] <- NA
        if (S15[i] == 1)
        {
          lowest_rep_threshold[i] <- 0.15
        }
        if (S15[i] == 0 & S10[i] == 1)
        {
          lowest_rep_threshold[i] <- 0.1
        }
        if (S15[i] == 0 & S10[i] == 0 & S5[i] == 1)
        {
          lowest_rep_threshold[i] <- 0.05
        }
        if (S15[i] == 0 & S10[i] == 0 & S5[i] == 0 & S1[i] == 1)
        {
          lowest_rep_threshold[i] <- 0.01
        }
        if (S15[i] == 0 &
            S10[i] == 0 & S5[i] == 0 & S1[i] == 0 & S01[i] == 1)
        {
          lowest_rep_threshold[i] <- 0.001
        }
        
        #non-significant as indicated by eye-catchers
        if (sig.level[i] == 0) {
          if (p.ub[i] < lowest_rep_threshold[i]) {
            prob2[i] <- 1
          }
          
        }
        
        # significant as indicated by eye-catchers
        if (sig.level[i] != 0) {
          if (p.lb[i] > lowest_rep_threshold[i]) {
            prob1[i] <- 1
          }
          
        }
        
        ## the rest is just for cross-checking our coding
        
        # 15% not reported
        
        # 10% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.1) {
          if (S10[i] == 0) {
            prob3[i] <- 1
          }
          
        }
        
        # 5% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.05) {
          if (S5[i] == 0) {
            prob3[i] <- 1
          }
        }
        
        
        # 1% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.01) {
          if (S1[i] == 0) {
            prob3[i] <- 1
          }
          
        }
        
        
        # 0.1% level of significance as indicated by eye-catchers
        if (sig.level[i] == 0.001) {
          if (S01[i] == 0) {
            prob3[i] <- 1
          }
          
        }
        
        
      }
    
    data.frame(prob1, prob2, prob3)
    
  }


# critical values from standard normal distribution for two-sided tests
cv15 <- qnorm(0.15 / 2, lower.tail = FALSE)
cv10 <- qnorm(0.1 / 2, lower.tail = FALSE)
cv5 <- qnorm(0.05 / 2, lower.tail = FALSE)
cv1 <- qnorm(0.01 / 2, lower.tail = FALSE)
cv01 <- qnorm(0.001 / 2, lower.tail = FALSE)

# for one-sided tests
cv15_os <- qnorm(0.15, lower.tail = FALSE)
cv10_os <- qnorm(0.1, lower.tail = FALSE)
cv5_os <- qnorm(0.05, lower.tail = FALSE)
cv1_os <- qnorm(0.01, lower.tail = FALSE)
cv01_os <- qnorm(0.001, lower.tail = FALSE)

## function inputs
## specific for statistics reported

# coefficient and standard error reported, store as numeric and factor
coefi <- as.numeric(paste(dat_coef_se$coefficient))
coefi.fac <- dat_coef_se$coefficient
se <- as.numeric(paste(dat_coef_se$standard_deviation))
se.fac <- dat_coef_se$standard_deviation
# type of test
type_emp_coef_se <- dat_coef_se$type_emp

# z/t-value reported
z <- as.numeric(paste(dat_zstat$t_stat))
z.fac <- dat_zstat$t_stat
# type of test
type_emp_zstat <- dat_zstat$type_emp

# p-value reported
p <- as.numeric(paste(dat_pval$p_value))
p.fac <- dat_pval$p_value


########################
# error detection for reported coef/se
########################
## further function inputs
# reported significance level attached to statistic
sig.level <- dat_coef_se$rep_sign / 100
# possible significance levels as indicated by table notes
S15 <- dat_coef_se$S.15.
S10 <- dat_coef_se$S.10.
S5 <- dat_coef_se$S.5.
S1 <- dat_coef_se$S.1.
S01 <- dat_coef_se$S.0.1.

# run function to obtain results
coefse_results <-
  rep_error_detection_coef_se(coefi,
                              coefi.fac,
                              se,
                              se.fac,
                              type_emp_coef_se,
                              sig.level,
                              S15,
                              S10,
                              S5,
                              S1,
                              S01)
# store all errors
A <- coefse_results[[1]]
apply(A, 2, mean)
# store strong errors
A_strong <- coefse_results[[2]]
apply(A_strong, 2, mean)
# store rounding bounds
A_bounds <- coefse_results[[3]]

#-----------------
# error detection for reported z
#-----------------
# further function inputs
sig.level <- dat_zstat$rep_sign / 100
S15 <- dat_zstat$S.15.
S10 <- dat_zstat$S.10.
S5 <- dat_zstat$S.5.
S1 <- dat_zstat$S.1.
S01 <- dat_zstat$S.0.1.

# run function to obtain results
z_results <-
  rep_error_detection_z_t(z, z.fac, type_emp_zstat, sig.level, S15, S10, S5, S1, S01)
# store all errors
B <- z_results[[1]]
apply(B, 2, mean)
# store strong errors
B_strong <- z_results[[2]]
apply(B_strong, 2, mean)
# store rounding bounds
B_bounds <- z_results[[3]]


#------------------
# error detection for reported p:
# no transformation to t statistic,
# direct comparison to significance thresholds
#------------------

# further function inputs
sig.level <- dat_pval$rep_sign / 100
S15 <- dat_pval$S.15.
S10 <- dat_pval$S.10.
S5 <- dat_pval$S.5.
S1 <- dat_pval$S.1.
S01 <- dat_pval$S.0.1.

# run function to obtain results
p_results <-
  rep_error_detection_p(p, p.fac, sig.level, S15, S10, S5, S1, S01)
# store all errors, not rounding bounds
C <- p_results[[1]]
apply(C, 2, mean)
# store rounding bounds
C_bounds <- p_results[[2]]
# store strong errors
p_results_strong <-
  rep_error_detection_p_strong(p, p.fac, sig.level, S15, S10, S5, S1, S01)
C_strong <- p_results_strong
apply(C_strong, 2, mean)

# combine results
results <-
  cbind(
    rbind(A_bounds, B_bounds, C_bounds),
    rbind(A, B, C),
    rbind(A_strong, B_strong, C_strong)
  )
colnames(results)[(ncol(results) - 2):ncol(results)] <-
  c("prob1_strong", "prob2_strong", "prob3_strong")

# for combining the results with the initial dataset, note order
data <- rbind(dat_coef_se, dat_zstat, dat_pval)
data_results <- cbind(data, results)

# create paper id
data_results$paper_id <-
  paste0(
    data_results$first_author,
    sep = "_",
    data_results$year,
    sep = "_",
    data_results$article_page
  )

# make paper id first variable
data_results <-
  cbind(data_results[ncol(data_results)], data_results[1:(ncol(data_results) -
                                                            1)])

# descriptive statistics: how many decimal numbers do the reported statistics have?
# coefficients
table(vapply(coefi.fac, function(x)
  decpoint.length(x), numeric(length(1))))
# some outliers
coefi.fac[which(vapply(coefi.fac, function(x)
  decpoint.length(x), numeric(length(1))) == 15)]
# standard errors
table(vapply(se.fac, function(x)
  decpoint.length(x), numeric(length(1))))
# t/z values
table(vapply(z, function(x)
  decpoint.length(x), numeric(length(1))))
# p values
table(vapply(p, function(x)
  decpoint.length(x), numeric(length(1))))

# table of last digit: are there any numbers under-or overrepresented?
digit_coef <-
  table(sapply(coefi.fac, function(x)
    substring(x, first = nchar(sub(
      '\\.', '', x
    )) + 1)))
digit_se <-
  table(sapply(se.fac, function(x)
    substring(x, first = nchar(sub(
      '\\.', '', x
    )) + 1)))
digit_t <-
  table(sapply(z, function(x)
    substring(x, first = nchar(sub(
      '\\.', '', x
    )) + 1)))
digit_p <-
  table(sapply(p, function(x)
    substring(x, first = nchar(sub(
      '\\.', '', x
    )) + 1)))
digits <- digit_coef + digit_se +
  c(as.numeric(digit_t)[1], 0, as.numeric(digit_t)[-1]) +
  c(as.numeric(digit_p)[1], 0, as.numeric(digit_p)[-1])
round(digits / sum(digits), 3)

# remove objects not needed anymore
rm(
  A,
  B,
  C,
  A_bounds,
  B_bounds,
  C_bounds,
  A_strong,
  B_strong,
  C_strong,
  dat_coef_se,
  dat_zstat,
  dat_pval
)

######### look at top of script: Do want to calculate error rates with or without considering the survey? #####
if (survey_considered == TRUE)
{
  ## label some errors as no errors according to survey: in explorative_stats we identify those detected errors which
  ## are (probably) in fact no errors according to the authors and our manual check.
  
  # load survey answers which are no errors
  ans_no_rep <-
    read.csv("data/answers_no_errors_anonymized.csv", header = TRUE)
  
  # create error id for big data set corresponding to the error id in the survey dataset just loaded
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
  
  # is the error id really unique?
  length(unique(data_results$error_id)) == nrow(data_results)
  
  # remove errors in data_results which are probably no errors according to authors and us
  # one problematic entry: here, we intentionnaly recoded the (wrong) table enumeration
  # for the survey so that the authors find their errors
  ans_no_rep$error_id <- as.character(ans_no_rep$error_id)
  ans_no_rep$error_id[!(ans_no_rep$error_id %in% data_results$error_id)]
  
  # manual check in the data- recode this entry to the initial (wrong) one
  ans_no_rep$error_id[!(ans_no_rep$error_id %in% data_results$error_id)] <-
    "Waldinger_787_9_3_2"
  
  # does it fit?
  sum(data_results$error_id %in% as.character(ans_no_rep$error_id)) ==
    nrow(ans_no_rep)
  
  # remove errors, i.e. recode prob1 and prob2 to zero for respective flagged tests
  data_results[data_results$error_id %in% as.character(ans_no_rep$error_id), (grepl(names(data_results), pattern =
                                                                                      "prob"))] <- 0
  
  # remove objects not needed
  rm(ans_no_rep)
  
  # remove error id variable
  drops <- "error_id"
  data_results <- data_results[, !names(data_results) %in% drops]
  
}

######### look at top of script: Do want to calculate error rates with correcting initially flagged tests as shown by replication? #####
if (repl_considered == TRUE)
{
  # load replication results
  repl <- read.xlsx("data/replications_30_work.xlsx", sheet = 1)
  
  # keep only variables which are important for constructing an error id and the true categorization of inconsistencies
  # as proven by replication
  vars_keep <-
    c(
      "First.author",
      "First.article.page",
      "Table",
      "Row",
      "Column",
      "Reporting.error.numeric"
    )
  repl_red <- (repl[, colnames(repl) %in% vars_keep])
  
  # create error id for in both data sets
  repl_red$error_id <-
    paste0(
      repl_red$First.author,
      sep = "_",
      repl_red$First.article.page,
      sep = "_",
      repl_red$Table,
      sep = "_",
      repl_red$Row,
      sep = "_",
      repl_red$Col
    )
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
  
  # combine data files
  data_results_repl <-
    data_results[(data_results$error_id %in% as.character(repl_red$error_id)), ]
  
  # recode error_id, because only those errors in data_results should be removed which prove
  # to be no errors in the replications
  repl_red$error_id[repl_red$Reporting.error.numeric == 1 |
                      is.na(repl_red$Reporting.error.numeric)] <- "indeed_reporting_error"
  
  # store file before removing errors, will be needed below in the output for calculating cases with non-standard reporting
  data_results_uncorrected <- data_results
  
  # remove errors
  data_results[data_results$error_id %in% as.character(repl_red$error_id), (grepl(names(data_results), pattern =
                                                                                    "prob"))] <- 0
  
  # remove error id
  drops <- "error_id"
  data_results <- data_results[, !names(data_results) %in% drops]
}

######### look at top of script: Do want to calculate error rates by estimating the error rates for the non-verified tests? #####
if (estimated_rate == TRUE)
{
  # read in all cases in which authors did not reply or answered I don't know and which we did not replicate
  # those cases were detected as inconsistencies by our algorithm but we do not know whether they are mistakes
  repl_not_repl <- read.csv("data/non_verified.csv", header = TRUE)
  
  # create error id for main data
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
  
  # store those cases for which we do not know answers
  data_results_unknown <-
    data_results[(data_results$error_id %in% repl_not_repl$error_id), ]
  
  # all errors and strong errors divided in over and understated
  err_rates_unknown <-
    matrix(c(
      mean(data_results_unknown$prob1),
      mean(data_results_unknown$prob2),
      mean(data_results_unknown$prob1_strong),
      mean(data_results_unknown$prob2_strong)
    ), 2, 2, byrow = TRUE)
  rownames(err_rates_unknown) <-
    c("Any reporting errors", "Strong reporting errors")
  colnames(err_rates_unknown) <- c("Overstated", "Understated")
  
  ## remove those cases from whole data set: for the remaining ones we are quite sure whether they are errors or not
  # save original sample size first
  n_sample_complete <- nrow(data_results)
  n_sample_complete_paper <- length(unique(data_results$paper_id))
  
  # now remove cases
  data_results <-
    data_results[!(data_results$error_id %in% repl_not_repl$error_id), ]
  
  # drop error id
  drops <- "error_id"
  data_results <- data_results[, !names(data_results) %in% drops]
  
  # save sample size of sample for which it is quite clear whether there is a reporting error or not
  n_sample_known <- nrow(data_results)
  
  ## do the same on the paper level
  # we want to have the articles for which we do not know the answers for at least some tests AND they should have
  # no understated or overstated error among all other tests, respectively i.e. also among those which have been replicated.
  # for the papers which have already at least one overstated or understated for sure, it does not make a difference
  # what we do with the cases which are not clear
  
  # create variables first which indicate per paper whether they have certain types of errors or not
  help_vars <-
    aggregate(cbind(prob1, prob2, prob1_strong, prob2_strong) ~ paper_id,
              data = data_results,
              sum)
  
  # restrict to those which have no (strong) overstated error yet
  known_no_ov <- help_vars$paper_id[help_vars$prob1 == 0]
  known_no_ov_strong <-
    help_vars$paper_id[help_vars$prob1_strong == 0]
  
  # create variables first which indicate per paper whether they have certain types of flagged tests which were not verified
  help_vars_unknown <-
    aggregate(cbind(prob1, prob2, prob1_strong, prob2_strong) ~ paper_id,
              data = data_results_unknown,
              sum)
  
  # restrict to those which have at least one (strong) overstated error as detected by algorithm
  unknown_at_least_one_ov <-
    help_vars_unknown$paper_id[help_vars_unknown$prob1 > 0]
  unknown_at_least_one_ov_strong <-
    help_vars_unknown$paper_id[help_vars_unknown$prob1_strong > 0]
  
  # create data set on article for which we have no idea whether articles include at least one (strong) overstated reporting error or not
  paper_unknown_ov <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_ov[known_no_ov %in%  unknown_at_least_one_ov], ]
  paper_unknown_ov_strong <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_ov_strong[known_no_ov_strong %in%  unknown_at_least_one_ov_strong], ]
  
  # in contrast, we know for the following articles for sure whether they have at least one (strong) overstated reporting error or not
  paper_known_ov <-
    help_vars[!help_vars$paper_id %in% known_no_ov[known_no_ov %in%  unknown_at_least_one_ov], ]
  paper_known_ov_strong <-
    help_vars[!help_vars$paper_id %in% known_no_ov_strong[known_no_ov_strong %in%  unknown_at_least_one_ov_strong], ]
  
  # store sample sizes
  n_pap_at_least_one_ov_unknown <- nrow(paper_unknown_ov)
  n_pap_at_least_one_ov_known <- nrow(paper_known_ov)
  n_pap_at_least_one_ov_unknown_strong <-
    nrow(paper_unknown_ov_strong)
  n_pap_at_least_one_ov_known_strong <-
    nrow(paper_known_ov_strong)
  
  # store error rates:
  share_at_least_one_ov_known <- mean(paper_known_ov$prob1 > 0)
  share_at_least_one_ov_unknown <- mean(paper_unknown_ov$prob1 > 0)
  share_at_least_one_ov_known_strong <-
    mean(paper_known_ov_strong$prob1_strong > 0)
  share_at_least_one_ov_unknown_strong <-
    mean(paper_unknown_ov_strong$prob1_strong > 0)
  
  ## same for understated
  # create variables first which indicate per paper whether they have certain types of errors or not
  # restrict to those which have no (strong) understated error yet
  known_no_und <- help_vars$paper_id[help_vars$prob2 == 0]
  known_no_und_strong <-
    help_vars$paper_id[help_vars$prob2_strong == 0]
  
  # restrict to those which have at least one (strong) understated error as detected by algorithm, do this for non-verified tests
  unknown_at_least_one_und <-
    help_vars_unknown$paper_id[help_vars_unknown$prob2 > 0]
  unknown_at_least_one_und_strong <-
    help_vars_unknown$paper_id[help_vars_unknown$prob2_strong > 0]
  
  # create data set on article for which we have no idea whether articles include at least one (strong) understated reporting error or not
  paper_unknown_und <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_und[known_no_und %in%  unknown_at_least_one_und], ]
  paper_unknown_und_strong <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_und_strong[known_no_und_strong %in%  unknown_at_least_one_und_strong], ]
  
  # in contrast, we know for the following articles for sure whether they have at least one reporting error or not
  paper_known_und <-
    help_vars[!help_vars$paper_id %in% known_no_und[known_no_und %in%  unknown_at_least_one_und], ]
  paper_known_und_strong <-
    help_vars[!help_vars$paper_id %in% known_no_und_strong[known_no_und_strong %in%  unknown_at_least_one_und_strong], ]
  
  # store sample sizes
  n_pap_at_least_one_und_unknown <- nrow(paper_unknown_und)
  n_pap_at_least_one_und_known <- nrow(paper_known_und)
  n_pap_at_least_one_und_unknown_strong <-
    nrow(paper_unknown_und_strong)
  n_pap_at_least_one_und_known_strong <-
    nrow(paper_known_und_strong)
  
  # store error rates:
  share_at_least_one_und_known <- mean(paper_known_und$prob2 > 0)
  share_at_least_one_und_unknown <- mean(paper_unknown_und$prob2 > 0)
  share_at_least_one_und_known_strong <-
    mean(paper_known_und_strong$prob2_strong > 0)
  share_at_least_one_und_unknown_strong <-
    mean(paper_unknown_und_strong$prob2_strong > 0)
  
  ## for papers with at least one error of any kind (irrespective of type of error)
  # we want to have the articles for which we do not know the answers for at least some tests AND they should have no
  # no error among all other tests, i.e. also among those which have been replicated
  # for the latter which have already at least one error for sure, it does not make a difference
  # what we do with the cases which are not clear
  
  # restrict to those which have no error yet
  known_no_err <-
    help_vars$paper_id[(help_vars$prob1 + help_vars$prob2) == 0]
  known_no_err_strong <-
    help_vars$paper_id[(help_vars$prob1_strong + help_vars$prob2_strong) == 0]
  
  # restrict to those which have at least one error as detected by algorithm
  unknown_at_least_one_err <-
    help_vars_unknown$paper_id[(help_vars_unknown$prob1 + help_vars_unknown$prob2) >
                                 0]
  unknown_at_least_one_err_strong <-
    help_vars_unknown$paper_id[(help_vars_unknown$prob1_strong + help_vars_unknown$prob2_strong) >
                                 0]
  
  # create data set on article article level, take those for which we have no idea whether article includes at least one reporting error or not
  paper_unknown_err <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_err[known_no_err %in%  unknown_at_least_one_err], ]
  paper_unknown_err_strong <-
    help_vars_unknown[help_vars_unknown$paper_id %in% known_no_err_strong[known_no_err_strong %in%  unknown_at_least_one_err_strong], ]
  
  # in contrast, we know for the following articles for sure whether they have at least one reporting error or not
  paper_known_err <-
    help_vars[!help_vars$paper_id %in% known_no_err[known_no_err %in%  unknown_at_least_one_err], ]
  paper_known_err_strong <-
    help_vars[!help_vars$paper_id %in% known_no_err_strong[known_no_err_strong %in%  unknown_at_least_one_err_strong], ]
  
  # store sample sizes
  n_pap_at_least_one_err_unknown <- nrow(paper_unknown_err)
  n_pap_at_least_one_err_known <- nrow(paper_known_err)
  n_pap_at_least_one_err_unknown_strong <-
    nrow(paper_unknown_err_strong)
  n_pap_at_least_one_err_known_strong <-
    nrow(paper_known_err_strong)
  
  # store error rates:
  share_at_least_one_err_known <-
    mean(paper_known_err$prob1 + paper_known_err$prob2 > 0)
  share_at_least_one_err_unknown <-
    mean(paper_unknown_err$prob1 + paper_unknown_err$prob2 > 0)
  share_at_least_one_err_known_strong <-
    mean(paper_known_err_strong$prob1_strong + paper_known_err_strong$prob2_strong >
           0)
  share_at_least_one_err_unknown_strong <-
    mean(
      paper_unknown_err_strong$prob1_strong + paper_unknown_err_strong$prob2_strong >
        0
    )
  
  # read in estimated rates of falsely detected errors
  false_det_rates <- read.table("data/shares_exp.txt")
  false_det_rates_strong <- read.table("data/shares_exp_strong.txt")
  false_det_rates_paper <- read.table("data/shares_exp_paper.txt")
  false_det_rates_paper_strong <-
    read.table("data/shares_exp_paper_strong.txt")
}

data_export <- data_results


# number of mistakes per paper separated by mistake type
mistakes_absolute <-
  aggregate(cbind(prob1, prob2, prob3) ~ paper_id, data = data_results, sum)
mistakes_absolute_strong <-
  aggregate(cbind(prob1_strong, prob2_strong, prob3_strong) ~ paper_id,
            data = data_results,
            sum)

# add sum of mistakes
mistakes_absolute <-
  cbind(mistakes_absolute, apply(mistakes_absolute[, -1], 1, sum))
colnames(mistakes_absolute) <-
  c("Paper_id",
    "overstated",
    "understated",
    "illogical",
    "error_sum")
mistakes_absolute_strong <-
  cbind(mistakes_absolute_strong,
        apply(mistakes_absolute_strong[, -1], 1, sum))
colnames(mistakes_absolute_strong) <-
  c("Paper_id",
    "overstated",
    "understated",
    "illogical",
    "error_sum")

# calculate prevalence of reporting errors at test level
error_matrix_freq[1, ] <-
  apply(mistakes_absolute[, -1], 2, function (x)
    sum(as.numeric(x))) / nrow(data_results)
error_matrix_freq[2, ] <-
  apply(mistakes_absolute_strong[, -1], 2, function (x)
    sum(as.numeric(x))) / nrow(data_results)

# calculate prevalence at paper level: yes or no
mistakes_binary <-
  as.data.frame(cbind(mistakes_absolute[, 1], (mistakes_absolute[, -1] > 0) *
                        1))
mistakes_binary[, -1] <-
  apply(mistakes_binary[, -1], 2, function(x)
    as.numeric(paste(x)))
print(apply(mistakes_binary[, -1], 2, mean))

mistakes_binary_strong <-
  as.data.frame(cbind(mistakes_absolute_strong[, 1], (mistakes_absolute_strong[, -1] >
                                                        0) * 1))
mistakes_binary_strong[, -1] <-
  apply(mistakes_binary_strong[, -1], 2, function(x)
    as.numeric(paste(x)))
print(apply(mistakes_binary_strong[, -1], 2, mean))

# how many articles with at least one mistake
error_matrix[1, ] <- apply(mistakes_binary[, -1], 2, mean)
error_matrix[2, ] <- apply(mistakes_binary_strong[, -1], 2, mean)

# convert to data frame
rownames(error_matrix_freq) <-
  c("Any reporting errors", "Strong reporting errors")
colnames(error_matrix_freq) <-
  c("Overstated", "Understated", "Illogical", "Sum")
error_matrix_freq <- as.data.frame(error_matrix_freq)

# if desired, correct by estimated share of errors in non-verified tests
if (estimated_rate == TRUE) {
  # store correct detection rates (as estimated from replication sample) for reporting errors and strong
  # reporting errors for understated and overstated errors at the test level
  correct_det_rates_testwise <-
    rbind(false_det_rates[, 2], false_det_rates_strong[, 2])
  
  # for this, we have to build a weighted mean of the error rates found in the verified tests and the error rates estimated in the non-verified tests
  error_matrix_freq[1:2, 1:2] <-
    error_matrix_freq[1:2, 1:2] * n_sample_known / n_sample_complete + err_rates_unknown *
    correct_det_rates_testwise * (1 - n_sample_known / n_sample_complete)
  
  # and the sum
  error_matrix_freq$Sum <-
    error_matrix_freq$Overstated + error_matrix_freq$Understated
}

## same at the paper level: compute share of papers containing at least one of the following misreported significances
# convert to data frame
rownames(error_matrix) <-
  c("Any reporting errors", "Strong reporting errors")
colnames(error_matrix) <-
  c("Overstated", "Understated", "illogical", "Any")
error_matrix <- as.data.frame(error_matrix)
# if desired, correct by estimated share of errors in non-verified tests

# if desired, correct by estimated share of papers with at least one error in papers with non-verified tests
if (estimated_rate == TRUE) {
  # overstated: weighted mean of papers with at least one known and with at least one unknown overstated error,
  # the latter is multiplied by the estimated
  # rate of correctly detected papers with at least one overstated error as estimated from replication exercise
  error_matrix[1, 1] <-
    share_at_least_one_ov_known * n_pap_at_least_one_ov_known / n_sample_complete_paper +
    share_at_least_one_ov_unknown * n_pap_at_least_one_ov_unknown / n_sample_complete_paper *
    false_det_rates_paper[1, 2]
  
  # same for understated
  error_matrix[1, 2] <-
    share_at_least_one_und_known * n_pap_at_least_one_und_known / n_sample_complete_paper +
    share_at_least_one_und_unknown * n_pap_at_least_one_und_unknown / n_sample_complete_paper *
    false_det_rates_paper[2, 2]
  
  # same for at least one error of any kind
  error_matrix[1, 4] <-
    share_at_least_one_err_known * n_pap_at_least_one_err_known / n_sample_complete_paper +
    share_at_least_one_err_unknown * n_pap_at_least_one_err_unknown / n_sample_complete_paper *
    false_det_rates_paper[3, 2]
  
  # same for strong errors, first overstated
  error_matrix[2, 1] <-
    share_at_least_one_ov_known_strong * n_pap_at_least_one_ov_known_strong /
    n_sample_complete_paper +
    share_at_least_one_ov_unknown_strong * n_pap_at_least_one_ov_unknown_strong /
    n_sample_complete_paper * false_det_rates_paper_strong[1, 2]
  
  # understated
  error_matrix[2, 2] <-
    share_at_least_one_und_known_strong * n_pap_at_least_one_und_known_strong /
    n_sample_complete_paper +
    share_at_least_one_und_unknown_strong * n_pap_at_least_one_und_unknown_strong /
    n_sample_complete_paper * false_det_rates_paper_strong[2, 2]
  
  # any
  error_matrix[2, 4] <-
    share_at_least_one_err_known_strong * n_pap_at_least_one_err_known_strong /
    n_sample_complete_paper +
    share_at_least_one_err_unknown_strong * n_pap_at_least_one_err_unknown_strong /
    n_sample_complete_paper * false_det_rates_paper_strong[3, 2]
}

## export data sets for all and for strong errors, will be used for regression analyses for survey responses and no-responses
# for all errors
data_export_all <-
  data_export[, -which(grepl(names(data_export), pattern = "_strong"))]

# for strong errors
data_export_strong <- data_export
data_export_strong[, c("prob1", "prob2", "prob3")] <-
  data_export_strong[, c("prob1_strong", "prob2_strong", "prob3_strong")]
data_export_strong <-
  data_export_strong[, -which(grepl(names(data_export_strong), pattern =
                                      "_strong"))]

# all prevalences together
prev_export <- cbind(
  rbind(
    apply(mistakes_binary[, -c(1, 4)], 2, sum),
    error_matrix[1, -3],
    apply(mistakes_binary_strong[, -c(1, 4)], 2, sum),
    error_matrix[2, -3]
  ),
  rbind(
    apply(mistakes_absolute[, -c(1, 4)], 2, sum),
    error_matrix_freq[1, -3],
    apply(mistakes_absolute_strong[, -c(1, 4)], 2, sum),
    error_matrix_freq[2, -3]
  )
)

# if the error is estimated by the replication strategy, compute absolute numbers
if (survey_considered == TRUE &
    repl_considered == TRUE & estimated_rate == TRUE)
{
  prev_export <-
    cbind(
      rbind(
        round(error_matrix[1, -3] * nrow(mistakes_binary), digits = 0),
        error_matrix[1, -3],
        round(error_matrix[2, -3] * nrow(mistakes_binary_strong), digits =
                0),
        error_matrix[2, -3]
      ),
      rbind(
        round(error_matrix_freq[1, -3] * nrow(data), digits = 0),
        error_matrix_freq[1, -3],
        round(error_matrix_freq[2, -3] * nrow(data), digits = 0),
        error_matrix_freq[2, -3]
      )
    )
}


#### now files are exported: this is relevant for creating the data files used for the regressions
if (robustness == FALSE)
{
  if (survey_considered == TRUE)
  {
    if (repl_considered == TRUE)
    {
      if (estimated_rate == FALSE)
      {
        # export data set with all survey and replication corrected cases, will be used for regression analyses
        write.csv(
          data_export_all,
          file = paste("data/data_results_cleaned_complete.csv", sep = ""),
          row.names = FALSE
        )
        write.csv(
          data_export_strong,
          file = paste(
            "data/data_results_sign_change_cleaned_complete.csv",
            sep = ""
          ),
          row.names = FALSE
        )
      }
    } 
  } 
  
} 


########################################### output ###################################################
# how many of the detected overstated inconsistencies for which we do not know whether they are errors
# could be due to execptional reporting style
# read in data without response and with I don't know answers
# only makes sense if replication is considered but data set is not split by verified and non-verified cases
if (repl_considered == TRUE & estimated_rate == FALSE)
{
  # read in data without verification and answer by authors
  data_non_ver <-
    read.csv("data/non_verified_survey_anonymized.csv", header = TRUE)
  
  # create error id for main data frame
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
  
  # create data without verification (from this data we drew the random sample for replication) and all variables of interest
  data_for_repl_sample <-
    data_results[(data_results$error_id %in% as.character(data_non_ver$error_id)), ]
  
  # how often non-standard reporting style ("other", "probit logit") for overstated
  print(table(data_for_repl_sample$type_emp[data_for_repl_sample$prob1 ==
                                              1]))
  
  ## how many of them with successful replication?
  # create error id for replication data set
  repl$error_id <-
    paste0(
      repl$First.author,
      sep = "_",
      repl$First.article.page,
      sep = "_",
      repl$Table,
      sep = "_",
      repl$Row,
      sep = "_",
      repl$Column
    )
  
  # merge data without verification with replicated data
  repl_dat <- merge(data_for_repl_sample, repl, by = "error_id")
  
  # how many of the replicated cases apply probit logit or other models according to brodeur et. al
  print(table(repl_dat$type_emp[repl_dat$prob1 == 1 &
                                  !is.na(repl_dat$Reporting.error.numeric)]))
  
  # how many of those prove to be reporting errors and no reporting errors
  print(table(repl_dat$type_emp[repl_dat$prob1 == 1 &
                                  !is.na(repl_dat$Reporting.error.numeric) &
                                  repl_dat$Reporting.error.numeric == 1]))
  print(table(repl_dat$type_emp[repl_dat$prob1 == 1 &
                                  !is.na(repl_dat$Reporting.error.numeric) &
                                  repl_dat$Reporting.error.numeric == 0]))
}

# how many tests not verified by survey or replication
if (repl_considered == TRUE & estimated_rate == TRUE)
  print(nrow(repl_not_repl))

## distribution of errors over articles
# for all errors
dist <- as.matrix(table(mistakes_absolute$error_sum))
dist <- cbind(as.numeric(rownames(dist)), dist)
dist <- as.data.frame(t(dist))
row.names(dist) <- c("Number of errors", "Frequency")
# dist

# for strong errors
dist_strong <- as.matrix(table(mistakes_absolute_strong$error_sum))
dist_strong <- cbind(as.numeric(rownames(dist_strong)), dist_strong)
dist_strong <- as.data.frame(t(dist_strong))
row.names(dist_strong) <- c("Number of errors", "Frequency")
# dist_strong

# create latex tables A1 and A2, distribution of (strong) errors over articles
if (repl_considered == TRUE & estimated_rate == FALSE)
{
  print(
    xtable(
      dist,
      caption = "Distribution of errors over articles",
      digits = 0,
      label = "tab:dist_err_pap"
    ),
    include.rownames = TRUE,
    include.colnames = FALSE,
    table.placement = "h",
    caption.placement = "top",
    hline.after = c(-1, nrow(dist))
  )
  print(
    xtable(
      dist_strong,
      caption = "Distribution of strong errors over articles",
      digits = 0,
      label = "tab:dist_strong_err_pap"
    ),
    include.rownames = TRUE,
    include.colnames = FALSE,
    table.placement = "h",
    caption.placement = "top",
    hline.after = c(-1, nrow(dist_strong))
  )
}


# how many articles in total (with and without error)
length(unique(data_export$paper_id))

# prevalences as depicted in Table 3 and Table A6 (robustness checks), respectively
# according to choices at the beginning of the script
round(prev_export, digits = 4)

# shares mentioned in section 7 (discussion), note also here
# which choices are made at the beginning of the script
# count number of reported significant and non-significant tests
data$sig <- ifelse(data$rep_sign>0,"sign","not_sign")
table(data$sig)
sign <- table(data$sig)["sign"]
not_sign <- table(data$sig)["not_sign"]

# excess of strong overstated errors among all strong errors
round((prev_export[3,4])/(prev_export[3,5]),4)  

# assumption: all strong errors occur in eye-catchers
# strong overstated / (insignificant results + strong overstated - strong understated)
round(prev_export[3,4]/(not_sign + prev_export[3,4] - prev_export[3,5]),4)
# strong understated / (significant results - strong overstated + strong understated)
round(prev_export[3,5]/(sign - prev_export[3,4] + prev_export[3,5]),4)
# excess
round((prev_export[3,4]/(not_sign + prev_export[3,4] - prev_export[3,5]))/
(prev_export[3,5]/(sign - prev_export[3,4] + prev_export[3,5])),4)



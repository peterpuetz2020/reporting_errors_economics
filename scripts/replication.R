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
Required_Packages <- c("openxlsx", "tidyverse")

# Call the Function
Install_And_Load(Required_Packages)

###### sample selection: we describe how the sample for the replication is drawn #####
all <- read.csv("data/non_verified_survey_anonymized.csv", header = TRUE)
all$paper_id <-
  paste0(all$First.author,
         sep = "_",
         all$Year,
         sep = "_",
         all$First.article.page)

# create error id
all$error_id <-
  paste0(
    all$First.author,
    sep = "_",
    all$First.article.page,
    sep = "_",
    all$Table,
    sep = "_",
    all$Row,
    sep = "_",
    all$Column
  )

### random sampling not reproducible because of delayed answer of one author,
# in principle as follows:
# set seed for sample generation
set.seed(4124)

# draw 30% sample
sam_30 <- all [sample(1:nrow(all), round(nrow(all) * 0.3)), ]

# how many papers
length(unique(sam_30$paper_id))

# take whole papers with these errors
final_sample <- all[all$paper_id %in% unique(sam_30$paper_id), ]


########## after replicating this sample, we analyse the results ##############
# read in coded replication
repl <- read.xlsx("data/replications_30_work.xlsx", sheet = 1)

# create paper id
repl$paper_id <-
  paste0(
    repl$First.author,
    sep = "_",
    repl$Year,
    sep = "_",
    repl$First.article.page
  )

# create error id
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

# remove all cases which we suceeded to replicate from all answers don't know answers and no replies
# therefore, first identify the replicated ones
repl_repl <- repl[!is.na(repl$Reporting.error.numeric), ]

# remove them from all
repl_notrepl <- all[!(all$error_id %in% unique(repl_repl$error_id)), ]

# export file, will be used in error detection algorithm
write.csv(repl_notrepl, "data/non_verified.csv")

# over-and understated
tab_und_ov <-
  table(repl$Direction, repl$Reporting.error.numeric, useNA = "always")
tab_und_ov

# how many papers with replicable inconsistencies
table(repl$paper_id[!is.na(repl$Reporting.error.numeric)], repl$Direction[!is.na(repl$Reporting.error.numeric)])

# percentage of replicated studies: <NA> column indicates share of overstated/understated which we
# were not able to replicate
tab_und_ov / apply(tab_und_ov, 1, sum)
# non-weighted share of overstated and understated significance level which are reporting errors
tab_und_ov_nona <- table(repl$Direction, repl$Reporting.error.numeric)
tab_und_ov_nona / apply(tab_und_ov_nona, 1, sum)

# sort by paper_id
repl <- repl[order(repl$paper_id), ]

# reasons for non-replication on test level:
non_repl <- subset(repl, is.na(Reporting.error.numeric))
reasons_non_repl_test_level_abs <- table(non_repl$Why.not.replicable)
reasons_non_repl_test_level_rel <-
  table(non_repl$Why.not.replicable) / sum(table(non_repl$Why.not.replicable))

# on paper level
reasons_non_repl_paper_level_abs <-
  apply(table(non_repl$paper_id , non_repl$Why.not.replicable), 2, function(x)
    sum(x > 0))
reasons_non_repl_paper_level_rel <-
  apply(table(non_repl$paper_id , non_repl$Why.not.replicable), 2, function(x)
    sum(x > 0)) /
  length(unique(repl$paper_id[is.na(repl$Reporting.error.numeric)]))

# compare: how many paper with non-replicable errors
length(unique(non_repl$paper_id))

# how many papers with replicable results
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)]))

# how many papers had some replicable and some non-replicable tests?
pap_both_repl_and_nonrepl <-
  abs(length(unique(repl$paper_id)) - length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)])) -
        length(unique(non_repl$paper_id)))

## weighted share of wrongly detected understated errors
# first calculate weights according to sampling scheme via weighting function, test and article level
wei <- function(n, k, a)
{
  if (k > 1)
  {
    prod <- numeric(k - 1)
    for (i in 0:(k - 2))
    {
      prod[i + 1] <- (n - a - i) / (n - i)
    }
    out <- a / n + sum(cumprod(prod) * c(a / (n - (1:(
      k - 1
    )))))
  } else
    out <- a / n
  return(out)
}

# population
n <- nrow(all)

# number of random samples from this population
k <- round(nrow(all) / 5)

# size of the group/cluster of observation for which probability is calculated
ss_paper <- as.numeric(table(repl$paper_id))
weights <- numeric(length(ss_paper))
for (a in 1:length(ss_paper))
  weights[a] <- wei(n, k, ss_paper[a])

# weights for all tests in paper
repl$fin_weights <- 1 / rep(weights, as.numeric(table(repl$paper_id)))
repl$fin_weights[is.na(repl$Reporting.error)] <- NA

# weighted mean for share of correctly detected overstated
ov <- repl[repl$Direction == "overstated", ]
wm_ov <-
  weighted.mean(ov$Reporting.error.numeric[!is.na(ov$Reporting.error.numeric)], ov$fin_weights[!is.na(ov$Reporting.error.numeric)])

# same for understated
# weighted mean for share of correctly detected overstated
und <- repl[repl$Direction == "understated", ]
wm_und <-
  weighted.mean(und$Reporting.error.numeric[!is.na(und$Reporting.error.numeric)], und$fin_weights[!is.na(und$Reporting.error.numeric)])

# comparison to simple mean
share_ov_und_falsely_detected <-
  tab_und_ov_nona / apply(tab_und_ov_nona, 1, sum)
share_ov_und_falsely_detected
# absolute numbers
tab_und_ov

# export shares of falsely detected overstated and understated
shares_exp <-
  as.data.frame(matrix(c(1 - wm_ov, wm_ov, 1 - wm_und, wm_und), 2, 2, byrow =
                         TRUE))
colnames(shares_exp) <- c("false", "correct")
rownames(shares_exp) <- c("overstated", "understated")
shares_exp
write.table(shares_exp, "data/shares_exp.txt")

# count how many papers with at least one overstated or understated replicated
pap_repl_one_error <-
  aggregate(repl$Direction[!is.na(repl$Reporting.error.numeric)], by = list(repl$paper_id[!is.na(repl$Reporting.error.numeric)]), table)[, -1]
pap_repl_one_error <-
  map_df(pap_repl_one_error, ~ .x) %>% select(overstated, understated)
pap_repl_one_error[is.na(pap_repl_one_error)] <- 0
pap_repl_one_error$any <- apply(pap_repl_one_error, 1, sum)
apply(pap_repl_one_error, 2, function (x)
  sum(x > 0))
# get weights for paper
pap_wei <-
  aggregate(repl$fin_weights[!is.na(repl$Reporting.error.numeric)], by = list(repl$paper_id[!is.na(repl$Reporting.error.numeric)]), mean)[, -1]

# how many of them are correct:
repl$Direction.correct <- numeric(nrow(repl))
repl$Direction.correct[repl$Direction == "overstated" &
                         repl$Reporting.error.numeric == 1] <- "overstated_correct"
repl$Direction.correct[repl$Direction == "understated" &
                         repl$Reporting.error.numeric == 1] <- "understated_correct"
pap_repl_one_error_correct <-
  aggregate(repl$Direction.correct[!is.na(repl$Reporting.error.numeric)], by =
              list(repl$paper_id[!is.na(repl$Reporting.error.numeric)]), table)[, -1]
pap_repl_one_error_correct <-
  map_df(pap_repl_one_error_correct, ~ .x) %>% select(overstated_correct, understated_correct)
pap_repl_one_error_correct[is.na(pap_repl_one_error_correct)] <- 0
pap_repl_one_error_correct$any <-
  apply(pap_repl_one_error_correct, 1, sum)
apply(pap_repl_one_error_correct, 2, function (x)
  sum(x > 0))

# share of papers with at least one error which are correctly detected, without weighting
shares <-
  apply(pap_repl_one_error_correct, 2, function (x)
    sum(x > 0)) / apply(pap_repl_one_error, 2, function (x)
      sum(x > 0))

# weighted
for (i in 1:ncol(pap_repl_one_error))
{
  # one error type
  type <- pap_repl_one_error[, i]
  
  # index for articles which contain at least one error of this type before replication
  ind <- type > 0
  
  # weighted mean
  shares[i] <-
    weighted.mean((pap_repl_one_error_correct[ind, i] > 0) * 1, pap_wei[ind])
}

# export shares with at least one error which are correctly detected
shares_exp_paper <- as.data.frame(matrix(c(1 - shares, shares), 3, 2))
colnames(shares_exp_paper) <- c("false", "correct")
rownames(shares_exp_paper) <- c("overstated", "understated", "any")
shares_exp_paper
write.table(shares_exp_paper, "data/shares_exp_paper.txt")

# count how many strong understated and strong overstated replicated
# first step: get minimum and maximum p-value consistent with reported statistics
deround_mat <- matrix((unlist(strsplit(
  as.character(
    repl$`p-value.interval.as.implied.by.reported.statistical.values.(this.is.the.p-value.interval.corresponding.to.the.lower.and.upper.derounding.bound.of.the.t-value.and.the.type.of.test.using.the.standard.normal.distribution,.rounded.to.6.decimal.places)`
  ),
  split = "[ < ]"
))),
nrow(repl), 7, byrow = TRUE)
deround_mat <- (deround_mat[, c(1, 7)])
deround_mat <- apply(deround_mat, 2, as.numeric)
# get minimum and maximum p-value as implied by eye-catcher
eye_catch_mat <- matrix((unlist(strsplit(
  as.character(
    repl$`p-value.interval.as.reported.by.means.of.eye-catchers`
  ),
  split = "[ < ]"
))),
nrow(repl), 7, byrow = TRUE)
eye_catch_mat <- (eye_catch_mat[, c(1, 7)])
eye_catch_mat <- apply(eye_catch_mat, 2, as.numeric)

# count under- and overstated which change significance
undov_sign <- matrix(0, nrow(repl), 2)

for (i in 1:nrow(repl))
{
  if (deround_mat[i, 2] < eye_catch_mat[i, 1] & eye_catch_mat[i, 2] == 1)
    undov_sign[i, 1] <- 1
  
  # not really correct to place .1 for the overstated here: it has to be the largest
  # significance threshold indicated by the eye-catchers
  if (eye_catch_mat[i, 2] < deround_mat[i, 1] & deround_mat[i, 1] > .1)
    undov_sign[i, 2] <- 1
}
colnames(undov_sign) <- c("understated", "overstated")

# how many replicated
apply(undov_sign[!is.na(repl$Reporting.error.numeric), ], 2, sum)

# weighted mean for share of correctly detected overstated
repl$ov_strong <- undov_sign[, 2]
table(repl$ov_strong[repl$ov_strong == 1], repl$Reporting.error.numeric[repl$ov_strong ==
                                                                          1])
wm_ov_strong <-
  weighted.mean(repl$Reporting.error.numeric[repl$ov_strong == 1 &
                                               !is.na(repl$Reporting.error.numeric)],
                repl$fin_weights[repl$ov_strong == 1 &
                                   !is.na(repl$Reporting.error.numeric)])

# same for understated
# weighted mean for share of correctly detected overstated
repl$und_strong <- undov_sign[, 1]
table(repl$und_strong[repl$und_strong == 1], repl$Reporting.error.numeric[repl$und_strong ==
                                                                            1])
wm_und_strong <-
  weighted.mean(repl$Reporting.error.numeric[repl$und_strong == 1 &
                                               !is.na(repl$Reporting.error.numeric)],
                repl$fin_weights[repl$und_strong == 1 &
                                   !is.na(repl$Reporting.error.numeric)])

# export shares of falsely detected under and over
shares_exp_strong <-
  as.data.frame(matrix(
    c(1 - wm_ov_strong, wm_ov_strong, 1 - wm_und_strong, wm_und_strong),
    2,
    2,
    byrow = TRUE
  ))
colnames(shares_exp_strong) <- c("false", "correct")
rownames(shares_exp_strong) <- c("overstated", "understated")
shares_exp_strong
write.table(shares_exp_strong, "data/shares_exp_strong.txt")

# strong errors at paper level
# count how many papers with at least one strong understated or overstated replicated
# indicator for a strong error
repl$strong_error <- repl$ov_strong + repl$und_strong
pap_repl_one_error_strong <-
  aggregate(repl$Direction[!is.na(repl$Reporting.error.numeric) &
                             repl$strong_error == 1], by = list(repl$paper_id[!is.na(repl$Reporting.error.numeric) &
                                                                                repl$strong_error == 1]), table)[, -1]
pap_repl_one_error_strong <-
  map_df(pap_repl_one_error_strong, ~ .x) %>%  select(overstated, understated)
pap_repl_one_error_strong[is.na(pap_repl_one_error_strong)] <- 0
pap_repl_one_error_strong$any <-
  apply(pap_repl_one_error_strong, 1, sum)
apply(pap_repl_one_error_strong, 2, function (x)
  sum(x > 0))

# how many of them are correct:
repl$Direction.correct.strong <- numeric(nrow(repl))
repl$Direction.correct.strong[repl$Direction == "overstated" &
                                repl$Reporting.error.numeric == 1 &
                                repl$strong_error == 1] <- "overstated_strong_correct"
repl$Direction.correct.strong[repl$Direction == "understated" &
                                repl$Reporting.error.numeric == 1 &
                                repl$strong_error == 1] <- "understated_strong_correct"
pap_repl_one_error_correct_strong <-
  aggregate(repl$Direction.correct.strong[!is.na(repl$Reporting.error.numeric)  &
                                            repl$strong_error == 1], by = list(repl$paper_id[!is.na(repl$Reporting.error.numeric)  &
                                                                                               repl$strong_error == 1]), table)[, -1]
pap_repl_one_error_correct_strong <-
  map_df(pap_repl_one_error_correct_strong, ~ .x) %>% select(overstated_strong_correct, understated_strong_correct)
pap_repl_one_error_correct_strong[is.na(pap_repl_one_error_correct_strong)] <-
  0
pap_repl_one_error_correct_strong$any <-
  apply(pap_repl_one_error_correct_strong, 1, sum)
apply(pap_repl_one_error_correct_strong, 2, function (x)
  sum(x > 0))

# share of papers with at least one error which are correctly detected, unweighted
shares <-
  apply(pap_repl_one_error_correct_strong, 2, function (x)
    sum(x > 0)) / apply(pap_repl_one_error_strong, 2, function (x)
      sum(x > 0))

# paper weights
pap_wei <-
  aggregate(repl$fin_weights[!is.na(repl$Reporting.error.numeric) &
                               repl$strong_error == 1], by = list(repl$paper_id[!is.na(repl$Reporting.error.numeric) &
                                                                                  repl$strong_error == 1]), mean)[, -1]

#weighted
for (i in 1:ncol(pap_repl_one_error_strong))
{
  # one error type
  type <- pap_repl_one_error_strong[, i]
  
  # index for articles which contain at least one error of this type before replication
  ind <- type > 0
  
  # weighted mean
  shares[i] <-
    weighted.mean((pap_repl_one_error_correct_strong[ind, i] > 0) * 1, pap_wei[ind])
}

# export shares with at least one error which are correctly detected
shares_exp_paper_strong <-
  as.data.frame(matrix(c(1 - shares, shares), 3, 2))
colnames(shares_exp_paper_strong) <- c("false", "correct")
rownames(shares_exp_paper_strong) <-
  c("overstated", "understated", "any")
shares_exp_paper_strong
write.table(shares_exp_paper_strong, "data/shares_exp_paper_strong.txt")

########### output: the numbers used in Section 5 and Figure 2
# number of articles which we tried to replicate
length(unique(repl$paper_id))

# 30% sample size
0.3 * nrow(all)

# tests wie tried to replicate
nrow(repl)

# share from all non-verified tests
nrow(repl) / nrow(all)

# how may tests did we replicate (first two columns, NAs are not replicable tests)
tab_und_ov
sum(tab_und_ov[, -3])

# share we replicated
sum(tab_und_ov[, -3]) / nrow(repl)

# and how many tests didn't we replicate
nrow(repl) - sum(tab_und_ov[, -3])

# share of tests we did not replicate
1 - sum(tab_und_ov[, -3]) / nrow(repl)

# number of papers including at least one non-replicable test
length(unique(repl$paper_id[is.na(repl$Reporting.error.numeric)]))

# share of papers including at least one non-replicable test
length(unique(repl$paper_id[is.na(repl$Reporting.error.numeric)])) / length(unique(repl$paper_id))

# share of articles in which we replicated all tests
1 - length(unique(repl$paper_id[is.na(repl$Reporting.error.numeric)])) /
  length(unique(repl$paper_id))

# in how many articles did we replicate at least one test
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)]))

# in how many articles did we replicate at least one test (share)
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)])) / length(unique(repl$paper_id))

# articles with at least one replicated and non-replicated test
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)])) +
  length(unique(repl$paper_id[is.na(repl$Reporting.error.numeric)])) -
  length(unique(repl$paper_id))

## reasons for non-replication
# test level
reasons_non_repl_test_level_abs
reasons_non_repl_test_level_rel

# data or code on test level
reasons_non_repl_test_level_abs[1] + reasons_non_repl_test_level_abs[2]
reasons_non_repl_test_level_rel[1] + reasons_non_repl_test_level_rel[2]

# article level
reasons_non_repl_paper_level_abs
reasons_non_repl_paper_level_rel

# data or code on paper level
reasons_non_repl_paper_level_abs[1] + reasons_non_repl_paper_level_abs[2]
reasons_non_repl_paper_level_rel[1] + reasons_non_repl_paper_level_rel[2]

# not replicable although data and code available on test level
sum(reasons_non_repl_test_level_abs[3:6])
sum(reasons_non_repl_test_level_rel[3:6])

# not replicable although data and code available on article level
sum(reasons_non_repl_paper_level_abs[3:6])
sum(reasons_non_repl_paper_level_rel[3:6])

# not replicable due to software unavailability on test level
sum(reasons_non_repl_test_level_abs[7])
sum(reasons_non_repl_test_level_rel[7])

# not replicable due to software unavailability on article level
sum(reasons_non_repl_paper_level_abs[7])
sum(reasons_non_repl_paper_level_rel[7])

# in how many articles did we replicate at least one test
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)]))

# share
length(unique(repl$paper_id[!is.na(repl$Reporting.error.numeric)])) / length(unique(repl$paper_id))

# over under
tab_und_ov

#shares of correctly detected inconsistencies
shares_exp

# how many strong errors did we replicate
apply(undov_sign[!is.na(repl$Reporting.error.numeric), ], 2, sum)

#share of all strong errors in our sample including those we did not replicate
sum(apply(undov_sign[!is.na(repl$Reporting.error.numeric), ], 2, sum)) /
  sum(undov_sign)

#shares of correctly detected inconsistencies
shares_exp_strong

# count how many papers with at least one understated or overstated replicated
apply(pap_repl_one_error, 2, function (x)
  sum(x > 0))

# share of papers with at least one error which are correctly detected
shares_exp_paper

# count how many papers with at least one strong understated or overstated replicated
apply(pap_repl_one_error_strong, 2, function (x)
  sum(x > 0))

# share of papers with at least one strong error which are correctly detected
shares_exp_paper_strong

# how many of the replicated tests are indeed errors and no errors (absolute number, not weighted)
sum(repl_repl$Reporting.error.numeric)
table(repl_repl$Direction, repl_repl$Reporting.error.numeric)

# how many tests not verified
nrow(all) - sum(table(repl_repl$Direction, repl_repl$Reporting.error.numeric))
# not in replication sample
nrow(all) - nrow(repl)
#not replicable
nrow(repl) - sum(table(repl_repl$Direction, repl_repl$Reporting.error.numeric))

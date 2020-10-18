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
# specify the list of required packages to be installed and loaded
Required_Packages <- c("xtable")

# call the Function
Install_And_Load(Required_Packages)

# read survey answers
ans <- read.csv("data/answers_anonymized.csv", header = TRUE)

## remove special case due to misalignment (because there was an erratum and misalignment should have been
## recognized ): we cannot really analyse the paper here. below we will assign
## "no reporting error" to the flagged tests
# save cases with misalignment
misal <-
  ans[grepl(ans$our_comment,
            pattern = "misalignment"), ]
# save cases without misalignment
ans <-
  ans[!grepl(ans$our_comment,
             pattern = "misalignment"), ]

## first check prevalences of understated and overstated significances for the tests for which the authors answered
# get minimum and maximum p-value consistent with reported statistics
deround_mat <- matrix((unlist(strsplit(
  as.character(
    ans$p.value.interval.as.implied.by.reported.statistical.values...this.is.the.p.value.interval.corresponding.to.the.lower.and.upper.derounding.bound.of.the.t.value.and.the.type.of.test..using.the.standard.normal.distribution.and.rounded.to.6.decimal.places.
  ),
  split = "[ < ]"
))),
nrow(ans), 7, byrow = TRUE)
deround_mat <- (deround_mat[, c(1, 7)])
deround_mat <- apply(deround_mat, 2, as.numeric)
# get minimum and maximum p-value as implied by eye-catcher
eye_catch_mat <- matrix((unlist(strsplit(
  as.character(ans$p.value.interval.as.reported.by.means.of.eye.catchers),
  split = "[ < ]"
))),
nrow(ans), 7, byrow = TRUE)
eye_catch_mat <- (eye_catch_mat[, c(1, 7)])
eye_catch_mat <- apply(eye_catch_mat, 2, as.numeric)

## count under- and overstated
undov <- matrix(0, nrow(ans), 2)
for (i in 1:nrow(ans))
{
  # if maximum p-value consistent with reported statistics is smaller than lower bound of p-value
  # interval as indicated by eye-catchers, we have an understated significance
  if (deround_mat[i, 2] < eye_catch_mat[i, 1])
    undov[i, 1] <- 1
  # accordingly... overstated
  if (eye_catch_mat[i, 2] < deround_mat[i, 1])
    undov[i, 2] <- 1
}
colnames(undov) <- c("understated", "overstated")
apply(undov, 2, sum)
# store understated and overstated significance level of flagged test before considering survey answers
undov_initial <- apply(undov, 2, sum)
share_undov_before <- apply(undov, 2, sum) / nrow(ans)

# only look at "there is no reporting errors" answers
ans_no_rep_error <- ans[!is.na(ans$There.is.no.reporting.error), ]

# save table with our comments as data frame
no_rep_err <- as.data.frame(table(ans_no_rep_error$our_comment))

## also add new column which includes that authors stated that low df were the reason
no_rep_err$low_df_stated <- rep(0, nrow(no_rep_err))
ind <- grepl("author: low df", no_rep_err$Var1, fixed = TRUE)
no_rep_err$low_df_stated[ind] <- 1


# percentage of coding errors conducted by us
no_rep_err$coding <- rep(0, nrow(no_rep_err))
ind <- grepl("coding", no_rep_err$Var1, ignore.case = TRUE)
no_rep_err$coding[ind] <- 1
our_errors <- sum(no_rep_err$Freq[ind])
our_errors

# percentage of "there is no reporting error" which are for sure reporting errors
no_rep_err$real_error <- rep(0, nrow(no_rep_err))
ind <-
  grepl("Implausible answer", no_rep_err$Var1, ignore.case = TRUE)
no_rep_err$real_error[ind] <- 1
wrong_answers <- sum(no_rep_err$Freq[ind])
wrong_answers

# percentage of "there is no reporting error" which are due to reporting of non-standard reporting style (usually marginal effects)
no_rep_err$marginal <- rep(0, nrow(no_rep_err))
ind <- grepl("marginal", no_rep_err$Var1, ignore.case = TRUE)
no_rep_err$marginal[ind] <- 1
marginal <- sum(no_rep_err$Freq[ind])
marginal

## percentage of "there is no reporting error" which possibly emerge from low df (including responses in which authors did not claim this)
no_rep_err$df <- rep(0, nrow(no_rep_err))
ind <-
  grepl("low df", no_rep_err$Var1, ignore.case = TRUE) 
no_rep_err$df[ind] <- 1
low_df_possible <- sum(no_rep_err$Freq[ind])
low_df_possible

# share of errors in for which authors did not mention df as a problem but which is still possible
low_df_possible_not_stated <-
  sum(no_rep_err$Freq[ind & no_rep_err$low_df_stated == 0])
low_df_possible_not_stated

# share of erros for which authors stated low df
low_df_stated <-
  sum(no_rep_err$Freq[ind & no_rep_err$low_df_stated == 1])
low_df_stated

# out of those, this is not possible for a couple of errors because authors stated low df
# but errors are overstated. exclude those
low_df_correctly_stated <-
  sum(no_rep_err$Freq[ind &
                        no_rep_err$low_df_stated == 1 & no_rep_err$real_error < 1])

# summary of all causes of alleged wrongly flagged tests
summary_no_rep_before <-
  data.frame(matrix(
    c(
      our_errors,
      marginal,
      low_df_correctly_stated,
      low_df_possible_not_stated,
      wrong_answers
    ),
    1,
    5
  ))
colnames(summary_no_rep_before) <-
  c("coder's fault",
    "marg. effects",
    "low df",
    "low df possible",
    "wrong answer")
rownames(summary_no_rep_before) <- ""
summary_no_rep_before

# save sample size of tests which were claimed to be no errors
n_norep_before <- nrow(ans_no_rep_error)

# without real errors (=wrong answers)
summary_no_rep_before[-length(summary_no_rep_before)] / (1 - wrong_answers)

# only keep real errors (wrong answers) out of those for which the authors claimed that these were no errors:
ind <-
  grepl("Implausible", ans_no_rep_error$our_comment, ignore.case = TRUE)
ans_no_rep <- ans_no_rep_error[ind, ]

# treat only them in original data set
ans_real_errors <-
  rbind(ans[is.na(ans$There.is.no.reporting.error), ], ans_no_rep)

# where is the error?
due_to <-
  data.frame(cbind(
    table(ans$Error.in.coefficient, useNA = "always"),
    table(ans$Error.in.standard.error, useNA = "always"),
    table(ans$Error.in.t.value...z.value, useNA = "always"),
    table(ans$Error.in.p.value, useNA = "always"),
    table(ans$Error.in.eye.catcher, useNA = "always"),
    table(ans$There.is.no.reporting.error, useNA = "always"),
    table(ans$I.don.t.know, useNA = "always")
  ))
colnames(due_to) <-
  c("coef.",
    "se.",
    "z-value",
    "p-value",
    "eye-catcher",
    "no error",
    "don't know")
due_to <- due_to[1, ]
rownames(due_to) <- ""

# maximum values equal 0, recode
due_to[due_to == nrow(ans)] <- 0
due_to

## for the reasons of the error, we separate between those answers which admitted an error and those who said there is no error
ans_error <-
  ans_real_errors[is.na(ans_real_errors$There.is.no.reporting.error), ]
why <-
  data.frame(cbind(
    table(
      ans_error$Error.occurred.while.transferring.results.from.statistical.software.to.Word..Latex.etc.,
      useNA = "always"
    ),
    table(
      ans_error$Error.occurred.while.updating.tables.during.the.research...review.process,
      useNA = "always"
    ),
    table(
      ans_error$Error.occured.in.typesetting.by.the.publisher.and.remained.undetected.in.proofreading,
      useNA = "always"
    ),
    table(ans_error$I.don.t.know.1, useNA = "always"),
    table(ans_error$Other.reason)
  ))
colnames(why) <-
  c("transfer", "updating", "typesetting", "don't know", "other")
why <- why[1, ]
rownames(why) <- ""
why
# maximum values equal 0, recode
why[why == nrow(ans_error)] <- 0
why

# to the regression analyses, we have to hand over those tests which are no errors:
ans_no_rep <- ans_no_rep_error[!ind, ]

# export cases in which authors answered I don't know to both questions
ind <- ((ans$I.don.t.know == 1) & ans$I.don.t.know.1 == 1)
ind[is.na(ind)] <- FALSE
ans_dunno <- ans[ind, ]
# write.csv(ans_dunno,"answers_dunno_anonymized.csv",row.names = FALSE)

# number of potentially wrongly detected errors (including don't know)
wrongly_detected_errors <- nrow(ans_no_rep)

# add special case misalignment (because there was erratum for an obvious misalignment).
# assign to the "no errors" category
misal$our_comment <- rep(NA, nrow(misal))
ans_no_rep <- rbind(ans_no_rep, misal)

# export the files without erroneosly flagged tests
# write.csv(ans_no_rep,"answers_no_errors_anonymized.csv",row.names = FALSE)

##########################################################
### frequency of under and overstated error after correction ###

# add extra column to ans_no_rep
ans_no_rep$delete <- rep(1, nrow(ans_no_rep))
# merge data sets
final <- merge(ans, ans_no_rep, "error_id", all = TRUE)
final <- final[!duplicated(final), ]
# delete entries which are no errors
final$delete[is.na(final$delete)] <- 0
final <- final[final$delete != 1, ]

## check prevalences of understated and overstated significances for the tests for which the authors answered
## first step: get minimum and maximum p-value consistent with reported statistics
deround_mat <- matrix((unlist(strsplit(
  as.character(
    final$p.value.interval.as.implied.by.reported.statistical.values...this.is.the.p.value.interval.corresponding.to.the.lower.and.upper.derounding.bound.of.the.t.value.and.the.type.of.test..using.the.standard.normal.distribution.and.rounded.to.6.decimal.places..x
  ),
  split = "[ < ]"
))),
nrow(final), 7, byrow = TRUE)
deround_mat <- (deround_mat[, c(1, 7)])
deround_mat <- apply(deround_mat, 2, as.numeric)
# get minimum and maximum p-value as implied by eye-catcher
eye_catch_mat <- matrix((unlist(strsplit(
  as.character(
    final$p.value.interval.as.reported.by.means.of.eye.catchers.x
  ),
  split = "[ < ]"
))),
nrow(final), 7, byrow = TRUE)
eye_catch_mat <- (eye_catch_mat[, c(1, 7)])
eye_catch_mat <- apply(eye_catch_mat, 2, as.numeric)

# count under- and overstated after correction
undov <- matrix(0, nrow(final), 2)
for (i in 1:nrow(final))
{
  if (deround_mat[i, 2] < eye_catch_mat[i, 1])
    undov[i, 1] <- 1
  if (eye_catch_mat[i, 2] < deround_mat[i, 1])
    undov[i, 2] <- 1
}
colnames(undov) <- c("understated", "overstated")

# count under- and overstated which change significance
undov_sign <- matrix(0, nrow(final), 2)
for (i in 1:nrow(final))
{
  if (deround_mat[i, 2] < eye_catch_mat[i, 1] & eye_catch_mat[i, 2] == 1)
    undov_sign[i, 1] <- 1
  # not really correct to place .1 for the overstated here: it has to be the largest
  # significance threshold indicated by the eye-catchers
  if (eye_catch_mat[i, 2] < deround_mat[i, 1] & deround_mat[i, 1] > .1)
    undov_sign[i, 2] <- 1
}
colnames(undov_sign) <- c("understated", "overstated")
apply(undov_sign, 2, sum)
share_undov_sign <- apply(undov_sign, 2, sum) / nrow(final)

## prevalence of reporting errors only regarding the papers for which we detected errors AND
## received answers (excluding wrongly detected errors)
# create paper id to merge with whole dataset
final$paper_id <- final$paper_id.x

# read in whole data set
reg_data <-
  read.csv("data/data_results_cleaned_complete_anonymized.csv", header = TRUE)

# merge with whole data set
resdata <-
  (reg_data[reg_data$paper_id %in% unique(final$paper_id), ])

## now look at wrongly detected errors: how many of wrongly detected errors which were due to non-standard reporting style
## are  coded by brodeur et al as non-standard reporting style (i.e. other/logit/probit)
# read whole data set
reg_data <- read.csv("data/data_results_anonymized.csv", header = TRUE)

# are the cases/observations really unique?
length(unique(reg_data$error_id)) == nrow(reg_data)

# merge with information on p-values
M <- merge(reg_data, ans_no_rep, "error_id")

# get sample size of sample with all papers for which at least one error was detected (also wrongly included)
n_answered <-
  length(as.character(reg_data$paper_id)[(as.character(reg_data$paper_id) %in% ans$paper_id)])

# check for prevalence of reporting errors in data set with answers to reporting errors (including wrongly
# detected errors)
rep_errors <- c(apply(undov, 2, sum), sum(apply(undov, 2, sum)))

# prevalence before removing wrongly detected errors
rep_errors_before <- c(undov_initial, sum(undov_initial))

# this is the dataset with only understated errors which are probably no errors
undnoerror <- M[M$prob2 == 1, ]

# this is the dataset with only overstated errors which are probably no errors
ovnoerror <- M[M$prob1 == 1, ]

# use corrected dataset with only significance changes
data_results <-
  read.csv("data/data_results_sign_change_cleaned_complete_anonymized.csv",
           header = TRUE)

# is it really unique?
length(unique(data_results$error_id)) == nrow(data_results)

# does it fit?
sum(data_results$error_id %in% as.character(final$error_id)) == nrow(final)

# reduce data set to the papers for which we received answers
resdata <-
  (data_results[data_results$paper_id %in% unique(final$paper_id), ])

# merge with information on p-values
final$paper_id <- final$paper_id.x
final <- final[, !names(final) %in% "paper_id"]
M <- merge(resdata, final, "error_id", all.x = TRUE)

## compare with uncorrected dataset (wrongly detected errors included)
## share of overstated which changed significance level
data_results <-
  read.csv("data/data_results_sign_change_anonymized.csv", header = TRUE)

# is it really unique? yes!
length(unique(data_results$error_id)) == nrow(data_results)
data_results[duplicated(data_results$error_id), ]

resdata_all <-
  (data_results[data_results$paper_id %in% unique(ans$paper_id), ])

# merge with information on p-values
M <- merge(resdata_all, ans, "error_id", all.x = TRUE)

# store overstated and understated before correction
strong_rep_errors_before <-
  as.data.frame(t(c(
    sum(resdata_all$prob1 == 1), sum(resdata_all$prob2 == 1)
  )))
colnames(strong_rep_errors_before) <- c("overstated", "understated")


############### output for paper
# response rate of authors requires first name of authors which are not given here, can ony found out via
# unique email adresses, confidential information
89/163

# response rate: articles (actually + 1 which is removed because of erratum)
# absolute
length(unique(ans$paper_id)) + 1
# relative
(length(unique(ans$paper_id)) + 1)/(length(unique(reg_data$paper_id[reg_data$prob1!=0 | reg_data$prob2!=0])))

# response rate: tests (actually + 6 which are removed because of erratum)
# absolute
nrow(ans) + 6
# relative
(nrow(ans) + 6)/sum(reg_data$prob1+reg_data$prob2)

# response rate of authors requires first name of authors which are not given here

# where is the reporting error?
colnames(due_to) <-
  c(
    "Coefficient",
    "Stand. error",
    "Test statistic",
    "$p$-value",
    "Eye-catcher",
    "There is no error",
    "I don't know"
  )
# combine absolute with relative frequencies
due_to <-
  rbind(due_to, paste("(", sprintf("%.1f", due_to / sum(due_to) * 100), "\\%)", sep =
                        ""))
# print the resulting TABLE 4
print(
  xtable(due_to, digits = 3, align = rep("c", 8)),
  floating = FALSE,
  include.rownames = FALSE,
  sanitize.text.function =   function(str)
    gsub("_", "\\_", str, fixed = TRUE)
)

# why is there apparently no reporting error (according to authors)?
colnames(summary_no_rep_before) <-
  c("Coder's fault",
    "Non-standard reporting",
    "Low df",
    "Low df possible",
    "Wrong answer")
# combine absolute with relative frequencies
summary_no_rep_before <-
  rbind(summary_no_rep_before, paste(
    "(",
    sprintf(
      "%.1f",
      summary_no_rep_before / sum(summary_no_rep_before) * 100
    ),
    "\\%)",
    sep = ""
  ))
# print the resulting TABLE 6
print(
  xtable(
    summary_no_rep_before,
    align = rep("c", 6),
    digits = 3,
    caption = paste(
      'Why is there no reporting error? ($n=',
      n_norep_before,
      ')$',
      "Notes: 'Coder's fault' refers to a error in the original coding or by us. 'Exceptional reporting' means that the reporting style
      deviates from the common one used for OLS regressions and thus
      leads to a detected inconsistency which is no reporting error,
      though. 'Low df' stands for low degres of freedom which cause a falsely diagnosed since our algorithm to
      detect reporting errors relies on critical values of the standard normal distribution. 'Low df possible' means that the authors did not give
      a reason why there is no reporting error, but we found that
      low degrees of freedom are a likely reason that there is indeed no reporting error. 'Wrong answer' indicates that
      the reason of the author why there should not be a reporting error is implausible.",
      sep = ""
    ),
    label = "table:why_no_rep"
  ),
  floating = FALSE,
  include.rownames = FALSE,
  sanitize.text.function =   function(str)
    gsub("_", "\\_", str, fixed = TRUE)
  )

# why is there a reporting error?
colnames(why) <-
  c("Transfer",
    "Updating",
    "Typesetting",
    "I don't know",
    "Other reason")
# combine absolute with relative frequencies
why <-
  rbind(why, paste("(", sprintf("%.1f", why / sum(why) * 100), "\\%)", sep =
                     ""))
# print the resulting TABLE 5
print(
  xtable(
    why,
    digits = 3,
    align = rep("c", 6),
    caption = paste(
      'Why is there a reporting error? ($n=',
      nrow(ans_error),
      ')$',
      "\\ Notes:
      'Transfer' refers to the incorrect transfer of results from statistical software to word processing software such as Word or Latex.
      'Updating' indicates that an error occurred while updating tables during the research / review process.
      'Typesetting' means that an error occured in typesetting by the publisher and remained undetected in proofreading. ",
      sep = ""
    ),
    label = "tab:why"
  ),
  floating = FALSE,
  include.rownames = FALSE,
  sanitize.text.function =   function(str)
    gsub("_", "\\_", str, fixed = TRUE)
  )

# probably wrongly detected errors (including cross-check of authors' answers who stated "no reporting error")
wrongly_detected_errors

# wrongly detected errors due to exceptional reporting style
sum(grepl("marginal", ans_no_rep$our_comment))
sum(grepl("marginal", ans_no_rep$our_comment)) / wrongly_detected_errors

# distribution over aticles
table(ans$paper_id[grepl("marginal", ans$our_comment)])

# wrongly detected errors due to low df
sum(grepl("low df", ans_no_rep$our_comment))
sum(grepl("low df", ans_no_rep$our_comment)) / wrongly_detected_errors

# how many incorrectly detected errors
(rev(rep_errors_before) - rev(rep_errors))

# share from all initially flagged errors
all <- read.csv("data/data_results_anonymized.csv", header = TRUE)
tests_flagged <-
  c(sum(all$prob1 + all$prob2), sum(all$prob1), sum(all$prob2))

## how many correctly detected errors: note that all of those which are not incorrectly detected are
## correctly detected errors: there are many without verification by the author
# remove wrongly detected reporting errors and dunnos from initial file
ans_correct <- ans[!ans$error_id %in% ans_no_rep$error_id, ]

# remove those with answers i don't know to both question (as we do not know whether these are reporting errors)
ans_correct <-
  ans_correct[!ans_correct$error_id %in% ans_dunno$error_id, ]

# merge resulting data set with whole data set from error_detection_algorithm.R to get overstated and understated
ans_correct_full <- all[all$error_id %in% ans_correct$error_id, ]

# count overstated and understated and all
ov_und_sum_correct <-
  c(
    sum(ans_correct_full$prob1 + ans_correct_full$prob2),
    sum(ans_correct_full$prob1),
    sum(ans_correct_full$prob2)
  )

# initially flagged errors (any, overstated, understated)
tests_flagged

# share of correctly detected errors
ov_und_sum_correct / tests_flagged

# falsely detected errors
(rev(rep_errors_before) - rev(rep_errors))

# share of falsely detected errors
(rev(rep_errors_before) - rev(rep_errors)) / tests_flagged

# share of unverified tests
(tests_flagged - (rev(rep_errors_before) - rev(rep_errors)) - ov_und_sum_correct) /
  tests_flagged

# distribution of falsely flagged overstated inconsistencies
# NAs (misalignment) not included in difference above
sum(is.na(ovnoerror$our_comment))
sum(grepl("coding", ovnoerror$our_comment, ignore.case = TRUE))
sum(grepl("marginal", ovnoerror$our_comment, ignore.case = TRUE))

# distribution of falsely flagged understated inconsistencies
# NAs (misalignment) not included in difference above
sum(is.na(undnoerror$our_comment))
sum(grepl("coding", undnoerror$our_comment, ignore.case = TRUE))
sum(grepl("marginal", undnoerror$our_comment, ignore.case = TRUE))
sum(grepl("low df", undnoerror$our_comment, ignore.case = TRUE))

# for significance changes
strong_rep_errors_before
strong_rep_errors_before / sum(strong_rep_errors_before)

# after correction
rev(apply(undov_sign, 2, sum))
rev(apply(undov_sign, 2, sum) / sum(apply(undov_sign, 2, sum)))

# difference
strong_rep_errors_before - rev(apply(undov_sign, 2, sum))

# how many errors due to exceptional reporting style are indicated by a
# special model (logit, probit, other) by the respective variable from Brodeur, see second row
table(grepl("marginal", M$our_comment, ignore.case = TRUE),
      M$type_emp)

# how many cases with manual verification (=all survey responses except "I don't know answers")
n_verified <- nrow(ans) + nrow(misal) - nrow(ans_dunno)
n_verified 

# how many of them incorrectly/correctly detected errors
ov_und_sum_correct / n_verified
1 - ov_und_sum_correct / n_verified

# how many tests remain for replication
all <- read.csv("data/non_verified_survey_anonymized.csv", header = TRUE)
n_non_verified <- nrow(all)
n_non_verified

# share dunno
nrow(ans_dunno) / n_non_verified

# share no response
1 - nrow(ans_dunno) / n_non_verified

# verified tests
n_verified / (n_verified + n_non_verified)

# flagged tests which remain unverified
n_non_verified

# TABLE 4
due_to

# TABLE 5
why

# TABLE 6
summary_no_rep_before

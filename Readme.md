Please cite as: 
Pütz. P and Bruns, S.B.: The (Non-)Significance of Reporting Errors in Economics: Evidence from Three Top Journals. Journal of Economic Surveys (2020).

## About this documentation
This documentation gives an overview of the files included in this repository and provides details on how to replicate the results, tables and figures. The analyses were conducted using R 4.0.1 (64 bit, Windows). 
If you use RStudio, open the `R` project 
`reporting_errors_economics.Rproj` first, then all R scripts loaded into this project should run as they are. If you do not use RStudio, you have to set your working directory at the beginning of each R script (`setwd(…)`) to the directory where the folders “data” and “scripts” are located.

### R scripts

The R scripts generate the results presented in the paper, the respective results can be found at the very end of the scripts. The Two figures in the paper are not created by R, but the results referred to in these figures are included in the R scripts. There are seven R scripts in the folder “scripts”:  

* `descriptives_tables.R`: Reproduces Tables 1, A3 and A4 in the paper.
* `error_detection_algorithm.R`: Flags tests and calculates the prevalence of reporting errors in Tables 3 and A6. Also generates the distribution of (strong) reporting errors per article in Tables A1 and A2 and the different shares of strong reporting errors with overstated and understated significance levels as discussed in Section 7 (the Discussion section). At the beginning of the file, it can be chosen which table (Table 3 or A6) and column of the respective table should be generated. If none of the options is selected (`survey_considered = FALSE`, `repl_considered = FALSE`, `estimated_rate = FALSE`, `zero_removed = FALSE`, `trimmed_decimals = FALSE`), the first column of Table 3 in the paper is generated. If only the survey results are considered (`survey_considered = TRUE`, `repl_considered = FALSE`, `estimated_rate = FALSE`, `zero_removed = FALSE`, `trimmed_decimals = FALSE`), column 2 of Table 3 is reproduced. If the replications are additionally considered to correct the results and to estimate the share of correctly flagged tests among the non-verified tests (`survey_considered = TRUE`, `repl_considered = TRUE`, `estimated_rate = TRUE`, `zero_removed = FALSE`, `trimmed_decimals = FALSE`), column 3 of Table 3 is generated. The two results columns in Table A6 are reproduced by considering dropped zeros at the end of the reported statistics (`survey_considered = TRUE`, `repl_considered = TRUE`, `estimated_rate = TRUE`, `zero_removed = TRUE`, `trimmed_decimals=FALSE`) and considering potentially trimmed decimals (`survey_considered = TRUE`, `repl_considered = TRUE`, `estimated_rate = TRUE`, `zero_removed = FALSE`, `trimmed_decimals = TRUE`), respectively. If the survey and the replications are considered to correct the results but the share of correctly flagged tests among the non-verified tests is not considered (`survey_considered = TRUE`, `repl_considered = TRUE`, `estimated_rate = FALSE`, `zero_removed = FALSE`, `trimmed_decimals = FALSE`), Tables A1 and A2 are generated.
The different shares of reporting errors in Section 7 are also computed according to the choices made at the beginning of the script.
* `flow_diagramme_statistics.R`: Reproduces the numbers shown in the lower part of Figure 1 (“Replication based Estimations”), all other results shown in Figure 1 are provided by the other tables, i.e. by the other R scripts.
* `regressions.R`: Reproduces Table 7. To obtain the first and third column, set `data_and_code = FALSE` at the beginning of the script, to obtain the second and fourth column, set `data_and_code = TRUE` at the beginning of the script. 
* `regressions_propensity_answer.R`: Reproduces Table A5. To obtain the first column, set `null_model = TRUE` and `data_and_code = FALSE` at the beginning of the script and vice versa to obtain the third column. To obtain the second column, set `null_model = FALSE` and `data_and_code = FALSE`.
* `replication.R`: Reproduces the statistics referred to in the replication section (Section 5) and Figure 2, exports the shares of correctly flagged tests `shares_exp[…].txt` which are used in `error_detection_algorithm.R`.
* `survey_results.R`: Reproduces Tables 4-6 and all other statistics referred to in the survey section.


### Data files
There are several data files in the folder “data” needed for the R scripts to work:  

* `answers_anonymized.csv`: Contains the anonymized survey responses. 
* `answers_no_errors_anonymized.csv`: This is an anonymized dataset containing those flagged tests which are probably no reporting errors according to the authors’ response and our cross-check.
* `data_results[…]_anonymized.csv`: These are anonymized files needed for generating the results based on the survey responses via survey_results.R.
* `data_results_cleaned_complete.csv` and `data_results_sign_change_cleaned_complete.csv`: Datasets used for the regression analyses; these datasets include the corrections of the initially flagged tests according to the survey responses and the replication exercise.
* `non_verified.csv`: These are the flagged tests which were neither verified by the authors nor by replication.
* `non_verified_survey_anonymized.csv`: This is an anonymized dataset containing those flagged tests which were not verified by the authors, i.e. no answer or “I don’t know” to both survey questions, the anonymization ensures that it is not clear who answered “I don’t know” .
* `only_eye_catchers_coded_complete.csv`: Contains all hypothesis tests under consideration in the article, the reported statistics, significance levels as indicated by eye-catchers and the information on which the eye-catchers are defined in the respective table notes.
* `only_eye_catchers_explanatory_variables.csv`: Contains a rich set of variables referring to the hypothesis tests.
* `replications_30_work.xlsx`: Contains the replication results.
* `shares_exp[…].txt`: Contains the estimated shares of correctly flagged unverified tests as generated by replication.R, these files are needed to generate column 3 in Table 3 and Table A6.

### Further files
The online appendix, the email sent to the authors whose articles contain at least one flagged test, the survey attached to this mail and the replication results can be found in the folder “supplements”.


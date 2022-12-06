```
> # load cchs data into R --------------------------------------------------------
> 
> index_df <- read.csv("index_df.csv", header = TRUE)
> 
> # load packages ----------------------------------------------------------------
> 
> if (!require(pacman)) {
+   install.packages("pacman", repos = "http://cran.rstudio.com/")
+   library(pacman)
+ }
Loading required package: pacman
> 
> p_load(
+   plyr,
+   tidyverse,
+   gtsummary,
+   crosstable,
+   rstatix,
+   janitor,
+   scales,
+   flextable,
+   skimr,
+   ggplot2,
+   sandwich,
+   msm,
+   xml2,
+   rvest,
+   tibble,
+   styler,
+   lintr
+ )
> 
> # check for missing data--------------------------------------------------------
> which(is.na(index_df))
integer(0)
> 
> # follow exclusion criteria in paper (n = 16706) -------------------------------
> index_df <- subset(index_df, (DHHGAGE != "12 TO 14 YEARS" # include age 20-69
+ & DHHGAGE != "15 TO 17 YEARS" &
+   DHHGAGE != "18 TO 19 YEARS" &
+   DHHGAGE != "80 YEARS OR MORE" &
+   DHHGAGE != "70 TO 74 YEARS" &
+   DHHGAGE != "75 TO 79 YEARS" &
+   GEOGPRV == "ONTARIO" # include Ontario
+ & DHH_SEX == "FEMALE" # include female sex
+ & PAP_022 != "REFUSAL" # include only yes/no response to pap test in last 3 years
+ & PAP_022 != "DON'T KNOW" &
+   PAP_022 != "NOT STATED"))
> 
> # check to see if sample size of index_df is same as index study ---------------
> 
> if (nrow(index_df) == 16706) {
+   print("Sample size is the same as the index study.")
+ } else {
+   print("Sample size is not the same as the index study.")
+ }
[1] "Sample size is the same as the index study."
> 
> 
> # create smaller dataframe------------------------------------------------------
> index_df_2 <- index_df[c(
+   "SDCGRES", "DHHGAGE", "INCGHH",
+   "EDUDR04", "DHHGMS", "SDCGLHM",
+   "SDCGLNG", "PAP_022", "WTS_M"
+ )]
> 
> # check smaller dataframe ------------------------------------------------------
> summary(index_df_2)
   SDCGRES            DHHGAGE             INCGHH            EDUDR04             DHHGMS         
 Length:16706       Length:16706       Length:16706       Length:16706       Length:16706      
 Class :character   Class :character   Class :character   Class :character   Class :character  
 Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                               
                                                                                               
                                                                                               
   SDCGLHM            SDCGLNG            PAP_022              WTS_M        
 Length:16706       Length:16706       Length:16706       Min.   :  10.88  
 Class :character   Class :character   Class :character   1st Qu.:  83.41  
 Mode  :character   Mode  :character   Mode  :character   Median : 143.91  
                                                          Mean   : 252.44  
                                                          3rd Qu.: 283.24  
                                                          Max.   :8639.38  
> 
> # convert all variables in smaller dataframe to factor--------------------------
> 
> index_df_2 <- index_df_2 %>%
+   mutate(
+     SDCGRES = fct_relevel(SDCGRES),
+     DHHGAGE = fct_relevel(DHHGAGE),
+     INCGHH = fct_relevel(INCGHH),
+     EDUDR04 = fct_relevel(EDUDR04),
+     DHHGMS = fct_relevel(DHHGMS),
+     SDCGLHM = fct_relevel(SDCGLHM),
+     SDCGLNG = fct_relevel(SDCGLNG),
+     PAP_022 = fct_relevel(PAP_022)
+   )
> 
> summary(index_df_2)
             SDCGRES                DHHGAGE                 INCGHH                 EDUDR04     
 0 TO 9 YEARS    :  675   55 TO 59 YEARS:2088   $20,000-$39,999:2690   < THAN SECONDARY: 1900  
 10 OR MORE YEARS: 2745   60 TO 64 YEARS:1931   $40,000-$59,999:2724   NOT STATED      :  422  
 NOT APPLICABLE  :12835   50 TO 54 YEARS:1907   $60,000-$79,999:2354   OTHER POST-SEC. : 1081  
 NOT STATED      :  451   35 TO 39 YEARS:1762   $80,000 OR MORE:5032   POST-SEC. GRAD. :10349  
                          40 TO 44 YEARS:1745   NO OR <$20,000 :1568   SECONDARY GRAD. : 2954  
                          30 TO 34 YEARS:1609   NOT STATED     :2338                           
                          (Other)       :5664                                                  
              DHHGMS                 SDCGLHM                  SDCGLNG     
 COMMON-LAW      :1281   ENG. W-W/O OTHER:14406   ENG. W-W/O OTHER:13405  
 MARRIED         :9085   ENG./FRE. W-W/O :  122   ENG./FRE. W-W/O : 2698  
 NOT STATED      :  35   FRE. W-W/O OTHER:  516   FRE. W-W/O OTHER:   46  
 SINGLE/NEVER MAR:3166   NOT ENGL. / FRE.: 1300   NOT ENGL. / FRE.:  168  
 WIDOW/SEP/DIV   :3139   NOT STATED      :  362   NOT STATED      :  389  
                                                                          
                                                                          
             PAP_022         WTS_M        
 < 6 MONTHS      :3486   Min.   :  10.88  
 1 TO < 3 YEARS  :4262   1st Qu.:  83.41  
 3 TO < 5 YEARS  :1027   Median : 143.91  
 5 OR MORE YEARS :2203   Mean   : 252.44  
 6 MTHS TO < 1 YR:4681   3rd Qu.: 283.24  
 NOT APPLICABLE  :1047   Max.   :8639.38  
                                          
> 
> # relabel and relevel immigrant status variable---------------------------------
> 
> index_df_2$sdcgres <- factor(index_df_2$SDCGRES,
+   levels = c(
+     "NOT APPLICABLE",
+     "0 TO 9 YEARS",
+     "10 OR MORE YEARS",
+     "NOT STATED"
+   )
+ )
> 
> levels(index_df_2$sdcgres) <- c(
+   "Non-immigrant",
+   "Recent immigrant",
+   "Non-recent immigrant",
+   "Unknown"
+ )
> 
> 
> 
> # relevel age-------------------------------------------------------------------
> index_df_2$dhhgage <- factor(index_df_2$DHHGAGE,
+   levels = c(
+     "20 TO 24 YEARS",
+     "25 TO 29 YEARS",
+     "30 TO 34 YEARS",
+     "35 TO 39 YEARS",
+     "40 TO 44 YEARS",
+     "45 TO 49 YEARS",
+     "50 TO 54 YEARS",
+     "55 TO 59 YEARS",
+     "60 TO 64 YEARS",
+     "65 TO 69 YEARS"
+   )
+ )
> 
> # create new age column---------------------------------------------------------
> index_df_2$age <- index_df_2$DHHGAGE
> 
> # recode new age column---------------------------------------------------------
> levels(index_df_2$age) <- c(
+   "20-34",
+   "20-34",
+   "20-34",
+   "35-54",
+   "35-54",
+   "35-54",
+   "35-54",
+   "55-69",
+   "55-69",
+   "55-69"
+ )
> 
> # relevel income----------------------------------------------------------------
> index_df_2$income <- factor(index_df_2$INCGHH,
+   levels = c(
+     "NO OR <$20,000",
+     "$20,000-$39,999",
+     "$40,000-$59,999",
+     "$60,000-$79,999",
+     "$80,000 OR MORE",
+     "NOT STATED"
+   )
+ )
> 
> # relabel income----------------------------------------------------------------
> levels(index_df_2$income) <- c(
+   "Less than $20 000",
+   "$20,000-$39,999",
+   "$40,000-$59,999",
+   "$60,000-$79,999",
+   "$80,000 or more",
+   "Unknown"
+ )
> 
> # relevel education-------------------------------------------------------------
> index_df_2$education <- factor(index_df_2$EDUDR04,
+   levels = c(
+     "< THAN SECONDARY",
+     "SECONDARY GRAD.",
+     "OTHER POST-SEC.",
+     "POST-SEC. GRAD.",
+     "NOT STATED"
+   )
+ )
> 
> # recode new education column---------------------------------------------------
> levels(index_df_2$education) <- c(
+   "Secondary graduation or less",
+   "Secondary graduation or less",
+   "Any post-secondary education",
+   "Any post-secondary education",
+   "Unknown"
+ )
> 
> # relevel marital status--------------------------------------------------------
> index_df_2$ms <- factor(index_df_2$DHHGMS,
+   levels = c(
+     "MARRIED",
+     "COMMON-LAW",
+     "WIDOW/SEP/DIV",
+     "SINGLE/NEVER MAR",
+     "NOT STATED"
+   )
+ )
> 
> # recode new marital status column-----------------------------------------------
> levels(index_df_2$ms) <- c(
+   "Married/common-law",
+   "Married/common-law",
+   "Widowed/separated/divorced",
+   "Single/never married",
+   "Unknown"
+ )
> 
> 
> # create new language spoken at home column-------------------------------------
> index_df_2$language_home <- index_df_2$SDCGLHM
> 
> # relevel new column languages spoken at home-----------------------------------
> levels(index_df_2$language_home) <- c(
+   "English", # English
+   "English", # English and French
+   "Other", # French
+   "English", # Other
+   "Unknown"
+ ) # Unknown
> # Note:
> # I think this was misdone in the original data set
> # because the only way to get 516 as the total sample as the other category in table 1,
> # is if they used French (with or without other) on it's own as the 'other' category.
> # However, It should have been the French and Other category grouped into other?
> # correct in 'extension 1'
> 
> 
> # create new language converse column-------------------------------------------
> index_df_2$language_converse <- index_df_2$SDCGLNG
> 
> # relevel new column language converse------------------------------------------
> levels(index_df_2$language_converse) <- c(
+   "English", # English
+   "English", # English and French
+   "Other", # French
+   "English", # Other
+   "Unknown"
+ ) # Unknown
> 
> # I think this was misdone in the original data set,
> # because the only way to get 46 as the other category,
> # is if they used French (with or without other) on it's own as the 'other' category
> # However, It should have been the French and Other category grouped into other?
> # correct in 'extension 1'
> 
> # create new column for pap test outcome----------------------------------------
> index_df_2$pap_outcome <- index_df_2$PAP_022
> 
> # relevel new column pap outcome------------------------------------------------
> levels(index_df_2$pap_outcome) <- c(
+   "Yes",
+   "Yes",
+   "No",
+   "No",
+   "Yes",
+   "Not applicable"
+ )
> 
> # create descriptive statistic table--------------------------------------------
> 
> ## create first two rows--------------------------------------------------------
> skim(index_df_2)
── Data Summary ────────────────────────
                           Values    
Name                       index_df_2
Number of rows             16706     
Number of columns          18        
_______________________              
Column type frequency:               
  factor                   17        
  numeric                  1         
________________________             
Group variables            None      

── Variable type: factor ────────────────────────────────────────────────────────────────────────
   skim_variable     n_missing complete_rate ordered n_unique
 1 SDCGRES                   0             1 FALSE          4
 2 DHHGAGE                   0             1 FALSE         10
 3 INCGHH                    0             1 FALSE          6
 4 EDUDR04                   0             1 FALSE          5
 5 DHHGMS                    0             1 FALSE          5
 6 SDCGLHM                   0             1 FALSE          5
 7 SDCGLNG                   0             1 FALSE          5
 8 PAP_022                   0             1 FALSE          6
 9 sdcgres                   0             1 FALSE          4
10 dhhgage                   0             1 FALSE         10
11 age                       0             1 FALSE          3
12 income                    0             1 FALSE          6
13 education                 0             1 FALSE          3
14 ms                        0             1 FALSE          4
15 language_home             0             1 FALSE          3
16 language_converse         0             1 FALSE          3
17 pap_outcome               0             1 FALSE          3
   top_counts                                 
 1 NOT: 12835, 10 : 2745, 0 T: 675, NOT: 451  
 2 55 : 2088, 60 : 1931, 50 : 1907, 35 : 1762 
 3 $80: 5032, $40: 2724, $20: 2690, $60: 2354 
 4 POS: 10349, SEC: 2954, < T: 1900, OTH: 1081
 5 MAR: 9085, SIN: 3166, WID: 3139, COM: 1281 
 6 ENG: 14406, NOT: 1300, FRE: 516, NOT: 362  
 7 ENG: 13405, ENG: 2698, NOT: 389, NOT: 168  
 8 6 M: 4681, 1 T: 4262, < 6: 3486, 5 O: 2203 
 9 Non: 12835, Non: 2745, Rec: 675, Unk: 451  
10 55 : 2088, 60 : 1931, 50 : 1907, 35 : 1762 
11 35-: 6984, 55-: 5590, 20-: 4132            
12 $80: 5032, $40: 2724, $20: 2690, $60: 2354 
13 Any: 11430, Sec: 4854, Unk: 422            
14 Mar: 10366, Sin: 3166, Wid: 3139, Unk: 35  
15 Eng: 15828, Oth: 516, Unk: 362             
16 Eng: 16271, Unk: 389, Oth: 46              
17 Yes: 12429, No: 3230, Not: 1047            

── Variable type: numeric ───────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate mean   sd   p0  p25  p50  p75  p100 hist 
1 WTS_M                 0             1 252. 343. 10.9 83.4 144. 283. 8639. ▇▁▁▁▁
> 
> table_one <- index_df_2 %>%
+   select(
+     sdcgres, age, INCGHH, education, ms, language_home, language_converse,
+     pap_outcome
+   ) %>%
+   tbl_summary(
+     by = pap_outcome,
+     percent = "row",
+     statistic = list(all_categorical() ~ "{n}"),
+     label = list(
+       sdcgres ~ "Immigrant status",
+       age ~ "Age (years)",
+       INCGHH ~ "Income",
+       education ~ "Highest level of education",
+       ms ~ "Marital status",
+       language_home ~ "Language spoken most often at home",
+       language_converse ~ "Language respondent could converse in"
+     )
+   ) %>%
+   add_overall()
> 
> ## need to do calculations on the table so convert to dataframe-----------------
> 
> table_one_df <- as_tibble(table_one)
> table_one_df <- rename(table_one_df, characteristic = `**Characteristic**`)
> table_one_df <- rename(table_one_df, total_sample = `**Overall**, N = 16,706`)
> table_one_df <- rename(table_one_df, pap_yes = `**Yes**, N = 12,429`)
> table_one_df <- rename(table_one_df, pap_no = `**No**, N = 3,230`)
> table_one_df <- rename(table_one_df, pap_na = `**Not applicable**, N = 1,047`)
> 
> table_one_df <- data.frame(table_one_df)
> 
> ## remove commas in table one dataframe-----------------------------------------
> 
> table_one_df$total_sample <- gsub(",", "", table_one_df$total_sample)
> table_one_df$pap_yes <- gsub(",", "", table_one_df$pap_yes)
> table_one_df$pap_no <- gsub(",", "", table_one_df$pap_no)
> table_one_df$pap_na <- gsub(",", "", table_one_df$pap_na)
> 
> ## convert table one columns to numeric-----------------------------------------
> table_one_df$total_sample <- as.numeric(table_one_df$total_sample)
> table_one_df$pap_yes <- as.numeric(table_one_df$pap_yes)
> table_one_df$pap_no <- as.numeric(table_one_df$pap_no)
> table_one_df$pap_na <- as.numeric(table_one_df$pap_na)
> 
> ## check columns of table one---------------------------------------------------
> summary(table_one_df)
 characteristic      total_sample        pap_yes            pap_no            pap_na     
 Length:33          Min.   :   35.0   Min.   :   26.0   Min.   :   5.00   Min.   :  2.0  
 Class :character   1st Qu.:  555.8   1st Qu.:  385.5   1st Qu.:  98.75   1st Qu.: 67.5  
 Mode  :character   Median : 2734.5   Median : 1989.0   Median : 505.50   Median :202.0  
                    Mean   : 4497.8   Mean   : 3346.3   Mean   : 869.62   Mean   :281.9  
                    3rd Qu.: 5450.5   3rd Qu.: 4009.0   3rd Qu.:1248.25   3rd Qu.:390.2  
                    Max.   :16271.0   Max.   :12144.0   Max.   :3132.00   Max.   :995.0  
                    NA's   :7         NA's   :7         NA's   :7         NA's   :7      
> 
> ## create new column for the unweighted prevalence of pap_yes ------------------
> table_one_df$pap_prevalence <- table_one_df$pap_yes / table_one_df$total_sample
> 
> ## create empty column for weighted_pap_prevalence------------------------------
> table_one_df["weighted_pap_prevalence"] <- NA
> 
> ## rename weights & account for weight in immigrant classification--------------
> index_df_2$wts_m <- as.numeric(index_df_2$WTS_M)
> 
> ### non-immigrant weighted------------------------------------------------------
> w_non_im_y <- sum(index_df_2[which(index_df_2$sdcgres == "Non-immigrant" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_non_im_n <- sum(index_df_2[which(index_df_2$sdcgres == "Non-immigrant" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_non_im_na <- sum(index_df_2[which(index_df_2$sdcgres == "Non-immigrant" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### non-recent immigrant weighted-----------------------------------------------
> w_non_rec_im_y <- sum(index_df_2[which(index_df_2$sdcgres == "Non-recent immigrant" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_non_rec_im_n <- sum(index_df_2[which(index_df_2$sdcgres == "Non-recent immigrant" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_non_rec_im_na <- sum(index_df_2[which(index_df_2$sdcgres == "Non-recent immigrant" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### recent immigrant weighted---------------------------------------------------
> w_rec_im_y <- sum(index_df_2[which(index_df_2$sdcgres == "Recent immigrant" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_rec_im_n <- sum(index_df_2[which(index_df_2$sdcgres == "Recent immigrant" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_rec_im_na <- sum(index_df_2[which(index_df_2$sdcgres == "Recent immigrant" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### immigrant unknown weighted--------------------------------------------------
> w_unknown_im_y <- sum(index_df_2[which(index_df_2$sdcgres == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_unknown_im_n <- sum(index_df_2[which(index_df_2$sdcgres == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_unknown_im_na <- sum(index_df_2[which(index_df_2$sdcgres == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for immigration in weighted_pap_prevalence column in table_one_df
> table_one_df[5, "weighted_pap_prevalence"] <- (w_unknown_im_y / (w_unknown_im_n + w_unknown_im_y + w_unknown_im_na))
> table_one_df[2, "weighted_pap_prevalence"] <- (w_non_im_y / (w_non_im_n + w_non_im_y + w_non_im_na))
> table_one_df[4, "weighted_pap_prevalence"] <- (w_non_rec_im_y / (w_non_rec_im_n + w_non_rec_im_y + w_non_rec_im_na))
> table_one_df[3, "weighted_pap_prevalence"] <- (w_rec_im_y / (w_rec_im_n + w_rec_im_y + w_rec_im_na))
> 
> ## account for weight in age classification-------------------------------------
> 
> ### age 20-34 weighted----------------------------------------------------------
> w_age_2034_y <- sum(index_df_2[which(index_df_2$age == "20-34" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_age_2034_n <- sum(index_df_2[which(index_df_2$age == "20-34" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_age_2034_na <- sum(index_df_2[which(index_df_2$age == "20-34" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### age 35-54 weighted----------------------------------------------------------
> w_age_3554_y <- sum(index_df_2[which(index_df_2$age == "35-54" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_age_3554_n <- sum(index_df_2[which(index_df_2$age == "35-54" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_age_3554_na <- sum(index_df_2[which(index_df_2$age == "35-54" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### age 55-69 weighted----------------------------------------------------------
> w_age_5569_y <- sum(index_df_2[which(index_df_2$age == "55-69" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_age_5569_n <- sum(index_df_2[which(index_df_2$age == "55-69" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_age_5569_na <- sum(index_df_2[which(index_df_2$age == "55-69" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for age in weighted_pap_prevalence column in table_one_df-----
> table_one_df[7, "weighted_pap_prevalence"] <- (w_age_2034_y / (w_age_2034_n + w_age_2034_y + w_age_2034_na))
> table_one_df[8, "weighted_pap_prevalence"] <- (w_age_3554_y / (w_age_3554_n + w_age_3554_y + w_age_3554_na))
> table_one_df[9, "weighted_pap_prevalence"] <- (w_age_5569_y / (w_age_5569_n + w_age_5569_y + w_age_5569_na))
> 
> ## account for weight in income classification----------------------------------
> 
> ### income < 20 000-------------------------------------------------------------
> w_income_less_20000_y <- sum(index_df_2[which(index_df_2$income == "Less than $20 000" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_less_20000_n <- sum(index_df_2[which(index_df_2$income == "Less than $20 000" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_less_20000_na <- sum(index_df_2[which(index_df_2$income == "Less than $20 000" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### income $20,000-$39,999------------------------------------------------------
> w_income_2039_y <- sum(index_df_2[which(index_df_2$income == "$20,000-$39,999" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_2039_n <- sum(index_df_2[which(index_df_2$income == "$20,000-$39,999" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_2039_na <- sum(index_df_2[which(index_df_2$income == "$20,000-$39,999" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### income $40,000-$59,999------------------------------------------------------
> w_income_4059_y <- sum(index_df_2[which(index_df_2$income == "$40,000-$59,999" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_4059_n <- sum(index_df_2[which(index_df_2$income == "$40,000-$59,999" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_4059_na <- sum(index_df_2[which(index_df_2$income == "$40,000-$59,999" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### income $60,000-$79,999------------------------------------------------------
> w_income_6079_y <- sum(index_df_2[which(index_df_2$income == "$60,000-$79,999" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_6079_n <- sum(index_df_2[which(index_df_2$income == "$60,000-$79,999" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_6079_na <- sum(index_df_2[which(index_df_2$income == "$60,000-$79,999" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### income $80,000 or more------------------------------------------------------
> w_income_80_more_y <- sum(index_df_2[which(index_df_2$income == "$80,000 or more" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_80_more_n <- sum(index_df_2[which(index_df_2$income == "$80,000 or more" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_80_more_na <- sum(index_df_2[which(index_df_2$income == "$80,000 or more" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### income unknown--------------------------------------------------------------
> w_income_unknown_y <- sum(index_df_2[which(index_df_2$income == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_income_unknown_n <- sum(index_df_2[which(index_df_2$income == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_income_unknown_na <- sum(index_df_2[which(index_df_2$income == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for age in weighted_pap_prevalence column in table_one_df---
> table_one_df[11, "weighted_pap_prevalence"] <- (w_income_less_20000_y / (w_income_less_20000_n + w_income_less_20000_y + w_income_less_20000_na))
> table_one_df[12, "weighted_pap_prevalence"] <- (w_income_2039_y / (w_income_2039_n + w_income_2039_y + w_income_2039_na))
> table_one_df[13, "weighted_pap_prevalence"] <- (w_income_4059_y / (w_income_4059_n + w_income_4059_y + w_income_4059_na))
> table_one_df[14, "weighted_pap_prevalence"] <- (w_income_6079_y / (w_income_6079_n + w_income_6079_y + w_income_6079_na))
> table_one_df[15, "weighted_pap_prevalence"] <- (w_income_80_more_y / (w_income_80_more_n + w_income_80_more_y + w_income_80_more_na))
> table_one_df[16, "weighted_pap_prevalence"] <- (w_income_unknown_y / (w_income_unknown_n + w_income_unknown_y + w_income_unknown_na))
> 
> ## account for weight in highest level of education ----------------------------
> 
> ### secondary or less-----------------------------------------------------------
> w_edu_sec_less_y <- sum(index_df_2[which(index_df_2$education == "Secondary graduation or less" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_edu_sec_less_n <- sum(index_df_2[which(index_df_2$education == "Secondary graduation or less" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_edu_sec_less_na <- sum(index_df_2[which(index_df_2$education == "Secondary graduation or less" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### any post secondary----------------------------------------------------------
> w_edu_post_sec_y <- sum(index_df_2[which(index_df_2$education == "Any post-secondary education" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_edu_post_sec_n <- sum(index_df_2[which(index_df_2$education == "Any post-secondary education" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_edu_post_sec_na <- sum(index_df_2[which(index_df_2$education == "Any post-secondary education" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### unknown---------------------------------------------------------------------
> w_edu_unknown_y <- sum(index_df_2[which(index_df_2$education == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_edu_unknown_n <- sum(index_df_2[which(index_df_2$education == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_edu_unknown_na <- sum(index_df_2[which(index_df_2$education == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for education in weighted_pap_prevalence column in table_one_df
> table_one_df[18, "weighted_pap_prevalence"] <- (w_edu_sec_less_y / (w_edu_sec_less_n + w_edu_sec_less_y + w_edu_sec_less_na))
> table_one_df[19, "weighted_pap_prevalence"] <- (w_edu_post_sec_y / (w_edu_post_sec_n + w_edu_post_sec_y + w_edu_post_sec_na))
> table_one_df[20, "weighted_pap_prevalence"] <- (w_edu_unknown_y / (w_edu_unknown_n + w_edu_unknown_y + w_edu_unknown_na))
> 
> ## account for weight in marital status ----------------------------------------
> 
> ### married/common-law----------------------------------------------------------
> w_ms_partner_y <- sum(index_df_2[which(index_df_2$ms == "Married/common-law" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_ms_partner_n <- sum(index_df_2[which(index_df_2$ms == "Married/common-law" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_ms_partner_na <- sum(index_df_2[which(index_df_2$ms == "Married/common-law" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### widowed/separated/divorced--------------------------------------------------
> w_ms_wsd_y <- sum(index_df_2[which(index_df_2$ms == "Widowed/separated/divorced" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_ms_wsd_n <- sum(index_df_2[which(index_df_2$ms == "Widowed/separated/divorced" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_ms_wsd_na <- sum(index_df_2[which(index_df_2$ms == "Widowed/separated/divorced" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### single/never married--------------------------------------------------------
> w_ms_sm_y <- sum(index_df_2[which(index_df_2$ms == "Single/never married" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_ms_sm_n <- sum(index_df_2[which(index_df_2$ms == "Single/never married" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_ms_sm_na <- sum(index_df_2[which(index_df_2$ms == "Single/never married" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### unknown---------------------------------------------------------------------
> w_ms_unknown_y <- sum(index_df_2[which(index_df_2$ms == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_ms_unknown_n <- sum(index_df_2[which(index_df_2$ms == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_ms_unknown_na <- sum(index_df_2[which(index_df_2$ms == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for education in weighted_pap_prevalence column in table_one_df
> table_one_df[22, "weighted_pap_prevalence"] <- (w_ms_partner_y / (w_ms_partner_n + w_ms_partner_y + w_ms_partner_na))
> table_one_df[23, "weighted_pap_prevalence"] <- (w_ms_wsd_y / (w_ms_wsd_n + w_ms_wsd_y + w_ms_wsd_na))
> table_one_df[24, "weighted_pap_prevalence"] <- (w_ms_sm_y / (w_ms_sm_n + w_ms_sm_y + w_ms_sm_na))
> table_one_df[25, "weighted_pap_prevalence"] <- (w_ms_unknown_y / (w_ms_unknown_n + w_ms_unknown_y + w_ms_unknown_na))
> 
> ## account for weight in Language spoken most often at home --------------------
> 
> #### english--------------------------------------------------------------------
> w_lang_home_english_y <- sum(index_df_2[which(index_df_2$language_home == "English" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_home_english_n <- sum(index_df_2[which(index_df_2$language_home == "English" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_home_english_na <- sum(index_df_2[which(index_df_2$language_home == "English" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### other-----------------------------------------------------------------------
> w_lang_home_other_y <- sum(index_df_2[which(index_df_2$language_home == "Other" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_home_other_n <- sum(index_df_2[which(index_df_2$language_home == "Other" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_home_other_na <- sum(index_df_2[which(index_df_2$language_home == "Other" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### unknown---------------------------------------------------------------------
> w_lang_home_unknown_y <- sum(index_df_2[which(index_df_2$language_home == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_home_unknown_n <- sum(index_df_2[which(index_df_2$language_home == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_home_unknown_na <- sum(index_df_2[which(index_df_2$language_home == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for language in weighted_pap_prevalence column in table_one_df
> table_one_df[27, "weighted_pap_prevalence"] <- (w_lang_home_english_y / (w_lang_home_english_n + w_lang_home_english_y + w_lang_home_english_na))
> table_one_df[28, "weighted_pap_prevalence"] <- (w_lang_home_other_y / (w_lang_home_other_n + w_lang_home_other_y + w_lang_home_other_na))
> table_one_df[29, "weighted_pap_prevalence"] <- (w_lang_home_unknown_y / (w_lang_home_unknown_n + w_lang_home_unknown_y + w_lang_home_unknown_na))
> 
> ## account for weight in Language spoken most often at home --------------------
> 
> ### english---------------------------------------------------------------------
> w_lang_con_english_y <- sum(index_df_2[which(index_df_2$language_converse == "English" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_con_english_n <- sum(index_df_2[which(index_df_2$language_converse == "English" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_con_english_na <- sum(index_df_2[which(index_df_2$language_converse == "English" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### other-----------------------------------------------------------------------
> w_lang_con_other_y <- sum(index_df_2[which(index_df_2$language_converse == "Other" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_con_other_n <- sum(index_df_2[which(index_df_2$language_converse == "Other" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_con_other_na <- sum(index_df_2[which(index_df_2$language_converse == "Other" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ### unknown---------------------------------------------------------------------
> w_lang_con_unknown_y <- sum(index_df_2[which(index_df_2$language_converse == "Unknown" & index_df_2$pap_outcome == "Yes"), "wts_m"])
> w_lang_con_unknown_n <- sum(index_df_2[which(index_df_2$language_converse == "Unknown" & index_df_2$pap_outcome == "No"), "wts_m"])
> w_lang_con_unknown_na <- sum(index_df_2[which(index_df_2$language_converse == "Unknown" & index_df_2$pap_outcome == "Not applicable"), "wts_m"])
> 
> ## fill in the rows for language in weighted_pap_prevalence column in table_one_df
> table_one_df[31, "weighted_pap_prevalence"] <- (w_lang_con_english_y / (w_lang_con_english_n + w_lang_con_english_y + w_lang_con_english_na))
> table_one_df[32, "weighted_pap_prevalence"] <- (w_lang_con_other_y / (w_lang_con_other_n + w_lang_con_other_y + w_lang_con_other_na))
> table_one_df[33, "weighted_pap_prevalence"] <- (w_lang_con_unknown_y / (w_lang_con_unknown_n + w_lang_con_unknown_y + w_lang_con_unknown_na))
> 
> ## final table one--------------------------------------------------------------
> 
> table_one_df_final <- table_one_df[, c("characteristic", "total_sample", "pap_yes", "weighted_pap_prevalence")]
> 
> table_one_df_final$weighted_pap_prevalence <- round(table_one_df_final$weighted_pap_prevalence*100, 1)
> 
> colnames(table_one_df_final) <- c("Characteristic", "Total sample", "Number that had a pap test within the last 3 years", "Percentage that sample had a pap test within the last
+ 3 years")
> 
> # descriptive statistics--------------------------------------------------------
> 
> ### poisson distribution of outcome variable------------------------------------
> 
> index_df_2$pap_outcome2 <- index_df_2$pap_outcome
> 
> levels(index_df_2$pap_outcome2) <- c(
+   "1",
+   "0",
+   "0"
+ )
> 
> index_df_2$pap_outcome2 <- as.integer(as.character(index_df_2$pap_outcome2))
> 
> hist(index_df_2$pap_outcome2)
> 
> #### poisson regression video 1:  https://www.youtube.com/watch?v=S7MkI6M4suc---
> #### poisson regression video 2:https://www.youtube.com/watch?v=0XfXHYDYoBA-----
> 
> # robust poisson regression using conventional logistic regression--------------
> 
> model_1 <- glm(formula = pap_outcome2 ~ sdcgres + age + income + education + ms, family = poisson(link = "log"), data = index_df_2, weights = wts_m)
> 
> summary(model_1)

Call:
glm(formula = pap_outcome2 ~ sdcgres + age + income + education + 
    ms, family = poisson(link = "log"), data = index_df_2, weights = wts_m)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-88.571   -6.003    2.116    3.835   46.947  

Coefficients:
                                       Estimate Std. Error  z value Pr(>|z|)    
(Intercept)                           -0.258870   0.002819  -91.830  < 2e-16 ***
sdcgresRecent immigrant               -0.260940   0.002189 -119.219  < 2e-16 ***
sdcgresNon-recent immigrant           -0.020305   0.001368  -14.845  < 2e-16 ***
sdcgresUnknown                        -0.078898   0.006777  -11.642  < 2e-16 ***
age35-54                              -0.050594   0.001419  -35.649  < 2e-16 ***
age55-69                              -0.240262   0.001781 -134.938  < 2e-16 ***
income$20,000-$39,999                  0.024034   0.002773    8.667  < 2e-16 ***
income$40,000-$59,999                  0.056268   0.002730   20.608  < 2e-16 ***
income$60,000-$79,999                  0.106816   0.002752   38.813  < 2e-16 ***
income$80,000 or more                  0.097321   0.002578   37.744  < 2e-16 ***
incomeUnknown                          0.018968   0.002868    6.614 3.75e-11 ***
educationAny post-secondary education  0.098393   0.001342   73.298  < 2e-16 ***
educationUnknown                       0.053037   0.007130    7.438 1.02e-13 ***
msWidowed/separated/divorced          -0.047148   0.001800  -26.190  < 2e-16 ***
msSingle/never married                -0.166439   0.001637 -101.699  < 2e-16 ***
msUnknown                             -0.042785   0.015542   -2.753  0.00591 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1717887  on 16705  degrees of freedom
Residual deviance: 1655285  on 16690  degrees of freedom
AIC: 8122185

Number of Fisher Scoring iterations: 5

> 
> ## check prevalence ratios
> exp(coef(model_1))
                          (Intercept)               sdcgresRecent immigrant 
                            0.7719231                             0.7703275 
          sdcgresNon-recent immigrant                        sdcgresUnknown 
                            0.9799002                             0.9241341 
                             age35-54                              age55-69 
                            0.9506645                             0.7864221 
                income$20,000-$39,999                 income$40,000-$59,999 
                            1.0243249                             1.0578816 
                income$60,000-$79,999                 income$80,000 or more 
                            1.1127292                             1.1022141 
                        incomeUnknown educationAny post-secondary education 
                            1.0191492                             1.1033960 
                     educationUnknown          msWidowed/separated/divorced 
                            1.0544687                             0.9539460 
               msSingle/never married                             msUnknown 
                            0.8466744                             0.9581173 
> 
> ## confidence intervals for PR--------------------------------------------------
> 
> ### used code from: from https://stats.oarc.ucla.edu/r/dae/poisson-regression/
> 
> cov.model_1 <- vcovHC(model_1, type = "HC0")
> 
> std.err <- sqrt(diag(cov.model_1))
> r.est <- cbind(
+   Estimate = coef(model_1), "Robust SE" = std.err,
+   "Pr(>|z|)" = 2 * pnorm(abs(coef(model_1) / std.err), lower.tail = FALSE),
+   LL = coef(model_1) - 1.96 * std.err,
+   UL = coef(model_1) + 1.96 * std.err
+ )
> 
> exp(r.est)
                                       Estimate Robust SE Pr(>|z|)        LL        UL
(Intercept)                           0.7719231  1.034717 1.000000 0.7219770 0.8253245
sdcgresRecent immigrant               0.7703275  1.044616 1.000000 0.7071638 0.8391330
sdcgresNon-recent immigrant           0.9799002  1.019239 1.331965 0.9439753 1.0171924
sdcgresUnknown                        0.9241341  1.076203 1.326671 0.8002438 1.0672044
age35-54                              0.9506645  1.016249 1.001697 0.9211011 0.9811767
age55-69                              0.7864221  1.021639 1.000000 0.7541060 0.8201230
income$20,000-$39,999                 1.0243249  1.036636 1.655595 0.9545752 1.0991711
income$40,000-$59,999                 1.0578816  1.035130 1.108678 0.9886598 1.1319500
income$60,000-$79,999                 1.1127292  1.034879 1.001838 1.0404132 1.1900717
income$80,000 or more                 1.1022141  1.033255 1.002935 1.0337587 1.1752025
incomeUnknown                         1.0191492  1.041633 1.900121 0.9408419 1.1039741
educationAny post-secondary education 1.1033960  1.018567 1.000000 1.0643186 1.1439082
educationUnknown                      1.0544687  1.083902 1.665870 0.9004376 1.2348487
msWidowed/separated/divorced          0.9539460  1.024576 1.053528 0.9096140 1.0004386
msSingle/never married                0.8466744  1.021618 1.000000 0.8119164 0.8829204
msUnknown                             0.9581173  1.193502 2.245393 0.6773997 1.3551657
> 
> s <- deltamethod(
+   list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)),
+   coef(model_1), cov.model_1
+ )
> 
> ### exponentiate old estimates dropping the p values----------------------------
> 
> rexp.est <- exp(r.est[, -3])
> 
> ### replace standard errors (ER) with estimates for exponentiated------------=--
> 
> rexp.est[, "Robust SE"] <- s
> 
> rexp.est
                                       Estimate  Robust SE        LL        UL
(Intercept)                           0.7719231 0.02634450 0.7219770 0.8253245
sdcgresRecent immigrant               0.7703275 0.03362457 0.7071638 0.8391330
sdcgresNon-recent immigrant           0.9799002 0.01867350 0.9439753 1.0171924
sdcgresUnknown                        0.9241341 0.06786760 0.8002438 1.0672044
age35-54                              0.9506645 0.02634450 0.9211011 0.9811767
age55-69                              0.7864221 0.03362457 0.7541060 0.8201230
income$20,000-$39,999                 1.0243249 0.01867350 0.9545752 1.0991711
income$40,000-$59,999                 1.0578816 0.06786760 0.9886598 1.1319500
income$60,000-$79,999                 1.1127292 0.02634450 1.0404132 1.1900717
income$80,000 or more                 1.1022141 0.03362457 1.0337587 1.1752025
incomeUnknown                         1.0191492 0.01867350 0.9408419 1.1039741
educationAny post-secondary education 1.1033960 0.06786760 1.0643186 1.1439082
educationUnknown                      1.0544687 0.02634450 0.9004376 1.2348487
msWidowed/separated/divorced          0.9539460 0.03362457 0.9096140 1.0004386
msSingle/never married                0.8466744 0.01867350 0.8119164 0.8829204
msUnknown                             0.9581173 0.06786760 0.6773997 1.3551657
> 
> # create and format table two---------------------------------------------------
> 
> table_two <- data.frame(rexp.est)
> 
> table_two_df <- table_two
> 
> ## add column for characteristic on the left side of the table------------------
> 
> table_two_df <- add_column(table_two_df, characteristic = "", .before = "Estimate")
> 
> ## add in title rows to the table two dataframe---------------------------------
> 
> table_two_df <- rbind(
+   table_two_df[1, ],
+   c("Immigrant status (reference: non-immigrants)", "", "", "", ""),
+   table_two_df[2:4, ],
+   c("Age (reference: age 20–34)", "", "", "", ""),
+   table_two_df[5:6, ],
+   c("Income (reference: >$20,000)", "", "", "", ""),
+   table_two_df[7:11, ],
+   c("Level of education (reference: secondary graduation or less)", "", "", "", ""),
+   table_two_df[12:13, ],
+   c("Marital status (reference: married/common-law)", "", "", "", ""),
+   table_two_df[14:16, ]
+ )
> 
> ## add in intercept label-------------------------------------------------------
> 
> table_two_df[1, "characteristic"] <- "Intercept"
> 
> ## add in immigrant status labels-----------------------------------------------
> 
> table_two_df[c(3, 4, 5), "characteristic"] <- c(
+   "Recent immigrants (<10 years in Canada)",
+   "Non-recent immigrants (>=10 years in Canada)",
+   "Unknown"
+ )
> 
> ## add in age labels------------------------------------------------------------
> table_two_df[c(7, 8), "characteristic"] <- c(
+   "Age 35–54",
+   "Age 55–69"
+ )
> 
> ## add in income labels---------------------------------------------------------
> table_two_df[c(10, 11, 12, 13, 14), "characteristic"] <- c(
+   "$20,000–$39,999",
+   "$40,000–$59,999",
+   "$60,000–$79,999",
+   "$80,000 or more",
+   "Unknown"
+ )
> 
> ## add in education labels------------------------------------------------------
> 
> table_two_df[c(16, 17), "characteristic"] <- c(
+   "Any post-secondary education",
+   "Unknown"
+ )
> 
> 
> ## add in marital status labels-------------------------------------------------
> 
> table_two_df[c(19, 20, 21), "characteristic"] <- c(
+   "Widowed/separated/divorced",
+   "Single/never married",
+   "Unknown"
+ )
> 
> ## remove row names and replace with numbers -----------------------------------
> 
> rownames(table_two_df) <- 1:21
> 
> ## add column names names and replace with numbers -----------------------------
> 
> colnames(table_two_df) <- c("Characteristic", "Prevalence Ratio", "Robust Standard Error", "Lower 95% CI", "Upper 95% CI")
> 
> ## change variables to numeric--------------------------------------------------
> 
> table_two_df$`Prevalence Ratio` <- round(as.numeric(table_two_df$`Prevalence Ratio`), 2)
> table_two_df$`Lower 95% CI` <- round(as.numeric(table_two_df$`Lower 95% CI`), 2)
> table_two_df$`Upper 95% CI` <- round(as.numeric(table_two_df$`Upper 95% CI`), 2)
> 
> ## create final version of table two--------------------------------------------
> 
> table_two_df_final <- table_two_df[, c("Characteristic", "Prevalence Ratio", "Lower 95% CI", "Upper 95% CI")]
> 
> ## add p value to final table two-----------------------------------------------
> 
> table_two_df$`Robust Standard Error` <- as.numeric(table_two_df$`Robust Standard Error`)
> 
> table_two_df$`P Value` <- exp(-0.717 * abs((log(table_two_df$`Prevalence Ratio`) / table_two_df$`Robust Standard Error`))
+   - 0.416 * ((log(table_two_df$`Prevalence Ratio`) / table_two_df$`Robust Standard Error`))^2)
> 
> table_two_df$`P Value` <- round(as.numeric(table_two_df$`P Value`), 10)
> table_two_df$`P Value 2` <- NA
> table_two_df$`P Value 2`[round(table_two_df$`P Value`, 2) <= 0.05] <- "a"
> table_two_df_final$`P Value` <- table_two_df$`P Value 2` 

```

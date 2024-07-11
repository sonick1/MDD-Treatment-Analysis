01_MDD_Analysis
================
Sonick Suri
2024-07-08

- [1. Introduction](#1-introduction)
- [2. Data Exploration](#2-data-exploration)
  - [2.1 Import Necessary Libraries](#21-import-necessary-libraries)
  - [2.2 Load dataset](#22-load-dataset)
  - [2.3 Explore dataset](#23-explore-dataset)
    - [2.3.1 Check Structure of
      datasets](#231-check-structure-of-datasets)
    - [2.3.2 Check Missing values in all
      variables](#232-check-missing-values-in-all-variables)
    - [2.3.3 Check Univariate distributions of all
      variables](#233-check-univariate-distributions-of-all-variables)
    - [2.3.4 Conclusion from data
      exploration](#234-conclusion-from-data-exploration)
- [3 Data Preprocessing](#3-data-preprocessing)
  - [3.1 Renaming Columns](#31-renaming-columns)
  - [3.2 Data formats](#32-data-formats)
  - [3.3 Join the files to create an Analytical
    dataset](#33-join-the-files-to-create-an-analytical-dataset)
  - [3.4 Catgories Standardization](#34-catgories-standardization)
  - [3.5 Feature Engineering](#35-feature-engineering)
- [4. Cohort Creation](#4-cohort-creation)
  - [4.1 Cohort Steps](#41-cohort-steps)
  - [4.1 Cohort Demographics and Treatment
    Statistics](#41-cohort-demographics-and-treatment-statistics)
  - [4.2 Treatment Combinations at time of First
    Admission](#42-treatment-combinations-at-time-of-first-admission)
- [5. Final Analysis](#5-final-analysis)
  - [5.1 Hypothesis testing](#51-hypothesis-testing)
  - [5.2 Logistic Model](#52-logistic-model)
  - [5.3 Conclusion](#53-conclusion)

## 1. Introduction

This notebook contains entire Analysis to generate insights using data
from routine clinical practice to better understand the treatment of
major depressive disorder (MDD). MDD is typically treated with
antidepressants and psychological therapy.

This notebooks Contains following items

- Exploration of the data

- Data Processing

- Cohort Creation

- Data Analysis

## 2. Data Exploration

### 2.1 Import Necessary Libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(lubridate)
library(rtables)
```

    ## Warning: package 'rtables' was built under R version 4.3.3

    ## Loading required package: formatters

    ## Warning: package 'formatters' was built under R version 4.3.3

    ## 
    ## Attaching package: 'formatters'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     %||%
    ## 
    ## Loading required package: magrittr
    ## 
    ## Attaching package: 'magrittr'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## 
    ## Attaching package: 'rtables'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     str

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(broom)
```

### 2.2 Load dataset

``` r
bill_amount <- read.csv("bill_amount.csv")
bill_id <- read.csv("bill_id.csv")
clinical_data <- read.csv("clinical_data.csv")
demographics <- read.csv("demographics.csv")
```

### 2.3 Explore dataset

``` r
View(bill_amount)
View(bill_id)
View(clinical_data)
View(demographics)
```

#### 2.3.1 Check Structure of datasets

Check structure of dataset to get idea of variables in each file. Also
check if there are any formatting changes we will need to make in
future.

``` r
datasets <- list(bill_amount,bill_id,clinical_data,demographics)
data_str <- lapply(datasets, str)
```

    ## 'data.frame':    13600 obs. of  2 variables:
    ##  $ bill_id: num  4.03e+07 2.66e+09 1.15e+09 3.82e+09 9.83e+09 ...
    ##  $ amount : num  1553 1032 6470 756 897 ...
    ## 'data.frame':    13600 obs. of  3 variables:
    ##  $ bill_id          : num  7.97e+09 6.18e+09 7.51e+09 3.76e+09 7.65e+09 ...
    ##  $ patient_id       : chr  "1d21f2be18683991eb93d182d6b2d220" "62bdca0b95d97e99e1c712048fb9fd09" "1d21f2be18683991eb93d182d6b2d220" "62bdca0b95d97e99e1c712048fb9fd09" ...
    ##  $ date_of_admission: chr  "2011-01-01" "2011-01-01" "2011-01-01" "2011-01-01" ...
    ## 'data.frame':    3400 obs. of  26 variables:
    ##  $ id                  : chr  "1d21f2be18683991eb93d182d6b2d220" "62bdca0b95d97e99e1c712048fb9fd09" "c85cf97bc6307ded0dd4fef8bad2fa09" "e0397dd72caf4552c5babebd3d61736c" ...
    ##  $ date_of_admission   : chr  "1/1/11" "1/1/11" "2/1/11" "2/1/11" ...
    ##  $ date_of_discharge   : chr  "11/1/11" "11/1/11" "13/1/11" "14/1/11" ...
    ##  $ medical_history_dia : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ medical_history_sud : int  1 0 0 1 0 NA 0 0 0 0 ...
    ##  $ medical_history_hbp : chr  "0" "0" "0" "No" ...
    ##  $ medical_history_ren : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ medical_history_tum : int  0 0 0 0 0 NA 0 0 0 0 ...
    ##  $ medical_history_anx : int  0 0 0 1 1 0 0 0 0 0 ...
    ##  $ medical_history_mood: int  0 0 0 1 1 0 1 1 0 1 ...
    ##  $ trt_anx             : int  1 0 0 1 0 0 0 0 0 0 ...
    ##  $ trt_con             : int  0 1 1 0 0 1 1 0 1 1 ...
    ##  $ trt_adt             : int  1 1 1 1 0 1 1 1 1 1 ...
    ##  $ trt_ssr             : int  0 1 1 0 0 1 1 0 1 0 ...
    ##  $ trt_the             : int  0 1 1 0 1 1 1 1 1 1 ...
    ##  $ trt_oth             : int  1 0 1 1 0 1 0 1 0 1 ...
    ##  $ symptom_1           : int  0 0 1 1 0 1 0 1 1 1 ...
    ##  $ symptom_2           : int  0 0 1 1 1 0 1 1 1 1 ...
    ##  $ symptom_3           : int  0 1 1 1 0 1 0 0 1 0 ...
    ##  $ symptom_4           : int  1 1 1 1 1 1 1 0 0 1 ...
    ##  $ symptom_5           : int  1 1 0 1 0 1 0 0 0 0 ...
    ##  $ weight              : num  71.3 78.4 72 64.4 55.6 78.8 81.8 73.5 98.4 92.8 ...
    ##  $ height              : int  161 160 151 152 160 169 164 173 166 176 ...
    ##  $ cgis_adm            : int  2 6 5 3 3 2 6 6 4 4 ...
    ##  $ cgis_dis            : int  2 5 5 5 4 4 4 5 4 3 ...
    ##  $ gaf_lv              : int  2 2 4 5 5 2 5 7 4 8 ...
    ## 'data.frame':    3000 obs. of  5 variables:
    ##  $ patient_id     : chr  "fa2d818b2261e44e30628ad1ac9cc72c" "5b6477c5de78d0b138e3b0c18e21d0ae" "320aa16c61937447fd6631bf635e7fde" "c7f3881684045e6c49020481020fae36" ...
    ##  $ gender         : chr  "Female" "f" "Male" "Male" ...
    ##  $ race           : chr  "Indian" "Chinese" "Chinese" "Malay" ...
    ##  $ resident_status: chr  "Singaporean" "Singapore citizen" "Singapore citizen" "Singapore citizen" ...
    ##  $ date_of_birth  : chr  "1971-05-14" "1976-02-18" "1982-07-03" "1947-06-15" ...

``` r
print(data_str)
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL

#### 2.3.2 Check Missing values in all variables

``` r
# Create function to check missing value % in each column
missing_percentage <- function (data){
  round(colSums(is.na(data)) / nrow(data) * 100,2)
}

lapply(datasets, missing_percentage)
```

    ## [[1]]
    ## bill_id  amount 
    ##       0       0 
    ## 
    ## [[2]]
    ##           bill_id        patient_id date_of_admission 
    ##                 0                 0                 0 
    ## 
    ## [[3]]
    ##                   id    date_of_admission    date_of_discharge 
    ##                 0.00                 0.00                 0.00 
    ##  medical_history_dia  medical_history_sud  medical_history_hbp 
    ##                 0.00                 6.85                 0.00 
    ##  medical_history_ren  medical_history_tum  medical_history_anx 
    ##                 0.00                 8.94                 0.00 
    ## medical_history_mood              trt_anx              trt_con 
    ##                 0.00                 0.00                 0.00 
    ##              trt_adt              trt_ssr              trt_the 
    ##                 0.00                 0.00                 0.00 
    ##              trt_oth            symptom_1            symptom_2 
    ##                 0.00                 0.00                 0.00 
    ##            symptom_3            symptom_4            symptom_5 
    ##                 0.00                 0.00                 0.00 
    ##               weight               height             cgis_adm 
    ##                 0.00                 0.00                 0.00 
    ##             cgis_dis               gaf_lv 
    ##                 0.00                 0.00 
    ## 
    ## [[4]]
    ##      patient_id          gender            race resident_status   date_of_birth 
    ##               0               0               0               0               0

#### 2.3.3 Check Univariate distributions of all variables

This is to ensure all variables have been correctly populated and check
if there are any outliers/cleaning required

Explore Histogram of numerical variables, including amount, Weight, and
height

``` r
amount <- bill_amount$amount
weight <- clinical_data$weight
height <- clinical_data$height

# Create a function to plot histogram
plot_histogram <- function(var,var_name){
  min_val <- min(var, na.rm = TRUE)
  max_val <- max(var, na.rm = TRUE)
  title <- paste("Histogram of", var_name, "(Min:", round(min_val, 2), ", Max:", round(max_val, 2), ")")
  
  ggplot() +
  aes(x = var) +
  geom_histogram(boundary = min(var),colour = "black", fill = "lightblue") + 
  labs(x = var_name, title = title)
}

# Create a list of all vectors and plot histogram

numerical_vec_list <- list(amount = amount, weight = weight, height = height)
plots <- mapply(plot_histogram, numerical_vec_list, names(numerical_vec_list), SIMPLIFY = FALSE)

# Display the plots
for (plot in plots) {
  print(plot)
}
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

Explore distribution of few categorical variables (All 1,0 flag
variables we will plot as bar and treat as discrete)

**Clinical Data**

``` r
# Select all variables from clinical_data except few which we have analyzed or do not need to clean
cols_to_exclude <- c("id","date_of_admission","date_of_discharge","weight","height")
bar_cols_clinical <- setdiff(names(clinical_data),cols_to_exclude)
bar_cols_demographics <- c("gender","race","resident_status")

clinical_data[,bar_cols_clinical] <- lapply(clinical_data[bar_cols_clinical], as.factor)
demographics[,bar_cols_demographics] <- lapply(demographics[bar_cols_demographics], as.factor)

plot_list <- list()
plot_bar <- function (var,data){
for (col in var){
  p <- ggplot(data,aes_string(x = col)) + 
    geom_bar(fill = "lightblue", color = "black") + 
    geom_text(stat = "count", aes(label = after_stat(count)),
              position = position_stack(vjust = 0.5)) + 
                labs(title = paste(col),x = "",y = "") + theme_light() 
  plot_list[[col]] <- p
}

for (i in seq(1, length(plot_list),by = 2)) {
  start_index <- i
  end_index <- i + 1
  subset_plots <- plot_list[start_index:end_index]
  grid.arrange(grobs = subset_plots,ncol = 2)
}
}
plot_bar(bar_cols_clinical,clinical_data)
```

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->

**Demographic File**

``` r
plot_bar(bar_cols_demographics,demographics)
```

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

#### 2.3.4 Conclusion from data exploration

Based on initial data exploration, it seems that we need to do few
processing steps before we merge different files to create one final
Analytical data set.

Changes:

- medical_history_hbp: convert yes and no to 1 and 0 respectively

- admission_date and discharge_date in Clinical_data: convert into
  appropriate date format to join with billing data  

- gender: in demographics needs to be cleaned by assigning m as Male,
  and f as Female

- race:Chinese,chinese and India, Indian to be combined separately

- resident_stats: Singaporean and Singapore citizen to be combined
  together

- There are few missing data points in columns medical_history_sud ,
  medical_history_tum (we might need to keep in mind while conducting
  future analysis)

## 3 Data Preprocessing

Here, we will clean the data set which can later be used to final joined
data set. We will also Explore all the variables in this section.

### 3.1 Renaming Columns

To help us keep similar convention across files, we will rename the
columns

``` r
clinical_data <- clinical_data %>% rename_at('id', ~'patient_id')
```

### 3.2 Data formats

Convert dates from clinical and bill tables to date format

``` r
clinical_data$date_of_admission <- as.Date(clinical_data$date_of_admission, format = "%d/%m/%y")
clinical_data$date_of_discharge <- as.Date(clinical_data$date_of_discharge, format = "%d/%m/%y")
bill_id$date_of_admission <- as.Date(bill_id$date_of_admission, format = "%Y-%m-%d")
demographics$date_of_birth <- as.Date(demographics$date_of_birth, format = "%Y-%m-%d")
```

``` r
# Lets also check duration of data
min(clinical_data$date_of_admission)
```

    ## [1] "2011-01-01"

``` r
max(clinical_data$date_of_admission)
```

    ## [1] "2015-12-28"

``` r
min(bill_id$date_of_admission)
```

    ## [1] "2011-01-01"

``` r
max(bill_id$date_of_admission)
```

    ## [1] "2015-12-28"

``` r
nv <- c("cgis_adm","cgis_dis","gaf_lv")
clinical_data[, nv] <- lapply(clinical_data[, nv], function(x) as.numeric(as.character(x)))
```

### 3.3 Join the files to create an Analytical dataset

``` r
df1 <- left_join(clinical_data,demographics,by = "patient_id")
print(nrow(df1))
```

    ## [1] 3400

``` r
df2 <- left_join(bill_id,bill_amount,by = "bill_id")
print(nrow(df2))
```

    ## [1] 13600

Since currently, on same admission, patients could have multiple bills,
we will sum the amount for each patient on their date of admission.

``` r
df3 <- df2 %>% group_by(patient_id,date_of_admission) %>% summarize(total_amount = sum(amount),.groups = 'drop')
```

``` r
dataset <- left_join(df1,df3,by = c("patient_id","date_of_admission"))
print(nrow(dataset))
```

    ## [1] 3400

Number of rows are 3400, which is same as Clinical_date, hence our data
is joined correctly. Now we will clean rest of the variables

### 3.4 Catgories Standardization

``` r
table(dataset$medical_history_hbp)
```

    ## 
    ##    0    1   No  Yes 
    ## 2176  348  761  115

``` r
dataset <- dataset %>%
  mutate(medical_history_hbp = recode(medical_history_hbp, "No" = "0", "Yes" = "1"))

table(dataset$medical_history_hbp)
```

    ## 
    ##    0    1 
    ## 2937  463

``` r
table(dataset$gender)
```

    ## 
    ##      f Female      m   Male 
    ##    116   1586    202   1496

``` r
dataset <- dataset %>%
  mutate(gender = recode(gender, "m" = "Male", "f" = "Female"))

table(dataset$gender)
```

    ## 
    ## Female   Male 
    ##   1702   1698

``` r
table(dataset$race)
```

    ## 
    ## chinese Chinese   India  Indian   Malay  Others 
    ##     356    1810     114     230     707     183

``` r
dataset <- dataset %>%
  mutate(race = recode(race, "chinese" = "Chinese", "India" = "Indian"))

table(dataset$race)
```

    ## 
    ## Chinese  Indian   Malay  Others 
    ##    2166     344     707     183

``` r
table(dataset$resident_status)
```

    ## 
    ##         Foreigner                PR Singapore citizen       Singaporean 
    ##               161               515               696              2028

``` r
dataset <- dataset %>%
  mutate(resident_status = recode(resident_status, "Singaporean" = "Singapore citizen"))

table(dataset$resident_status)
```

    ## 
    ##         Foreigner                PR Singapore citizen 
    ##               161               515              2724

### 3.5 Feature Engineering

Here we will create additional features:

- Age at time of admission (Year of admission - Year of Birth)

- Length of Stay in inpatient setting

- Any Symptom of MDD

- Any Treatments

- Number of treatments taken

- First Admission Date

``` r
dataset <- dataset %>%
  mutate(age = year(ymd(date_of_admission)) - year(ymd(date_of_birth)))
plot_histogram(dataset$age, "Age")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
dataset <- dataset %>%
  mutate(los = as.integer(difftime(date_of_discharge, date_of_admission, units = "days")))

plot_histogram(dataset$los, "Length of Stay")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
dataset <- dataset %>%
  rowwise() %>%
  mutate(any_symptom = if_else(any(c_across(starts_with("symptom_")) == 1), 1, 0)) %>%
  ungroup()

table(dataset$any_symptom)
```

    ## 
    ##    0    1 
    ##   25 3375

``` r
dataset <- dataset %>%
  rowwise() %>%
  mutate(any_treatment = if_else(any(c_across(starts_with("trt_")) == 1), 1, 0)) %>%
  ungroup()

table(dataset$any_treatment)
```

    ## 
    ##    0    1 
    ##    2 3398

``` r
table(dataset$trt_adt)
```

    ## 
    ##    0    1 
    ##  609 2791

``` r
dataset <- dataset %>%
 mutate(across(starts_with("trt_"), ~ as.numeric(as.character(.)))) %>%
  mutate(number_of_treatments = rowSums(select(., starts_with("trt_")))) %>% 
  mutate(across(starts_with("trt_"), ~ as.factor(.)))
```

``` r
dataset$number_ge_2 <- ifelse(dataset$number_of_treatments >= 2,1,0)
dataset$number_ge_2 <- as.factor(dataset$number_ge_2)
```

``` r
dataset <- dataset %>%  group_by(patient_id) %>%
  mutate(min_date_of_admission = min(date_of_admission)) %>% ungroup()

dataset %>% filter(min_date_of_admission == date_of_admission) %>% nrow()
```

    ## [1] 3000

``` r
length(unique(dataset$patient_id))
```

    ## [1] 3000

Since this matched number of patients, the flag is created correctly

## 4. Cohort Creation

We will first get dataset at patient level by looking only at First
Admission

``` r
patient_level_data <- dataset %>% filter(min_date_of_admission == date_of_admission) 
patient_level_data$First_admission_flag <- "Total Patients in Baseline"
patient_level_data$any_treatment <- as.factor(patient_level_data$any_treatment)

nrow(patient_level_data)
```

    ## [1] 3000

### 4.1 Cohort Steps

Identify Patients with

- To inlcude have adult patients (Age \>= 18)

- To exclude patients without treatment (At least 1 Therapy)

- To get moderately ill unresolved MDD patients (cgis at admission \>=
  4)

``` r
nrow(patient_level_data)
```

    ## [1] 3000

``` r
# Age >= 18

patient_level_data <- patient_level_data %>% filter(age >= 18)
nrow(patient_level_data)
```

    ## [1] 3000

``` r
# Any treatmnet taken
patient_level_data <- patient_level_data %>% filter(any_treatment == 1)
nrow(patient_level_data)
```

    ## [1] 2998

``` r
# Unresolved MDD
patient_level_data <- patient_level_data %>% filter(cgis_adm >=4)
nrow(patient_level_data)
```

    ## [1] 2240

### 4.1 Cohort Demographics and Treatment Statistics

Create table using rtables function

Write a function to create clean tables in R

``` r
create_tables <- function(char_vars,num_vars){
lyt <- basic_table(show_colcounts = TRUE) %>%
   split_cols_by("First_admission_flag") %>%
   summarize_row_groups() %>%
   analyze(char_vars,afun = counts_wpcts) %>% 
             analyze(num_vars, function(x, ...) {
    if (is.numeric(x)) {
      in_rows(
        "Mean (sd)" = c(mean(x), sd(x)),
        "Median" = median(x),
        "Min - Max" = range(x),
        .formats = c("xx.xx (xx.xx)", "xx.xx", "xx.xx - xx.xx")
      )
    } else if (is.factor(x) || is.character(x)) {
      in_rows(.list = list_wrap_x(table)(x))
    } else {
      stop("type not supported")
    }
  })
build_table(lyt, patient_level_data)
}
```

Understand demographics

``` r
create_tables(c("gender","race", "resident_status"),c("age","los","total_amount"))
```

    ##                         Total Patients in Baseline
    ##                                  (N=2240)         
    ## ——————————————————————————————————————————————————
    ##                               2240 (100.0%)       
    ##   gender                                          
    ##     Female                     1115 (49.8%)       
    ##     Male                       1125 (50.2%)       
    ##   race                                            
    ##     Chinese                    1413 (63.1%)       
    ##     Indian                     232 (10.4%)        
    ##     Malay                      473 (21.1%)        
    ##     Others                      122 (5.4%)        
    ##   resident_status                                 
    ##     Foreigner                   110 (4.9%)        
    ##     PR                         352 (15.7%)        
    ##     Singapore citizen          1778 (79.4%)       
    ##   age                                             
    ##     Mean (sd)                 52.14 (14.66)       
    ##     Median                        50.00           
    ##     Min - Max                 22.00 - 85.00       
    ##   los                                             
    ##     Mean (sd)                  11.06 (2.86)       
    ##     Median                        11.00           
    ##     Min - Max                  2.00 - 20.00       
    ##   total_amount                                    
    ##     Mean (sd)              21804.99 (10103.39)    
    ##     Median                       20151.95         
    ##     Min - Max               4027.73 - 98120.87

Understand Clinical Characteristics

``` r
medical_history_columns <- grep("medical_history", names(dataset), value = TRUE)
symptom_columns <- grep("symptom_", names(dataset), value = TRUE)

create_tables(c(medical_history_columns,symptom_columns,"number_ge_2"),
              c("cgis_adm","cgis_dis","gaf_lv","number_of_treatments"))
```

    ##                          Total Patients in Baseline
    ##                                   (N=2240)         
    ## ———————————————————————————————————————————————————
    ##                                2240 (100.0%)       
    ##   medical_history_dia                              
    ##     0                           1886 (84.2%)       
    ##     1                           354 (15.8%)        
    ##   medical_history_sud                              
    ##     0                           1455 (65.0%)       
    ##     1                           632 (28.2%)        
    ##   medical_history_hbp                              
    ##     0                           1943 (86.7%)       
    ##     1                           297 (13.3%)        
    ##   medical_history_ren                              
    ##     0                           2128 (95.0%)       
    ##     1                            112 (5.0%)        
    ##   medical_history_tum                              
    ##     0                           1915 (85.5%)       
    ##     1                            133 (5.9%)        
    ##   medical_history_anx                              
    ##     0                           1670 (74.6%)       
    ##     1                           570 (25.4%)        
    ##   medical_history_mood                             
    ##     0                           1655 (73.9%)       
    ##     1                           585 (26.1%)        
    ##   symptom_1                                        
    ##     0                           869 (38.8%)        
    ##     1                           1371 (61.2%)       
    ##   symptom_2                                        
    ##     0                           754 (33.7%)        
    ##     1                           1486 (66.3%)       
    ##   symptom_3                                        
    ##     0                           1009 (45.0%)       
    ##     1                           1231 (55.0%)       
    ##   symptom_4                                        
    ##     0                           618 (27.6%)        
    ##     1                           1622 (72.4%)       
    ##   symptom_5                                        
    ##     0                           1051 (46.9%)       
    ##     1                           1189 (53.1%)       
    ##   number_ge_2                                      
    ##     0                            32 (1.4%)         
    ##     1                           2208 (98.6%)       
    ##   cgis_adm                                         
    ##     Mean (sd)                   4.86 (0.89)        
    ##     Median                          5.00           
    ##     Min - Max                   4.00 - 7.00        
    ##   cgis_dis                                         
    ##     Mean (sd)                   3.96 (1.02)        
    ##     Median                          4.00           
    ##     Min - Max                   1.00 - 6.00        
    ##   gaf_lv                                           
    ##     Mean (sd)                   5.78 (1.33)        
    ##     Median                          6.00           
    ##     Min - Max                   1.00 - 8.00        
    ##   number_of_treatments                             
    ##     Mean (sd)                   4.02 (1.10)        
    ##     Median                          4.00           
    ##     Min - Max                   1.00 - 6.00

### 4.2 Treatment Combinations at time of First Admission

``` r
trt_col <- grep("trt_", names(patient_level_data), value = TRUE)
for (col in trt_col){
  print(patient_level_data %>% filter(!!sym(col) == 1) %>% nrow())
}
```

    ## [1] 1145
    ## [1] 1325
    ## [1] 1865
    ## [1] 1160
    ## [1] 1853
    ## [1] 1647

``` r
patient_level_data1 <- patient_level_data[,trt_col]

patient_level_data1 <- apply(patient_level_data1, 2, as.character)
patient_level_data1 <- apply(patient_level_data1, 2, as.numeric)

trt_df <- patient_level_data %>% group_by(trt_anx,trt_con,trt_adt,trt_ssr,trt_the,trt_oth)
```

``` r
cross_counts <- crossprod(as.matrix(patient_level_data1))
cross_counts
```

    ##         trt_anx trt_con trt_adt trt_ssr trt_the trt_oth
    ## trt_anx    1145     688     956     599     935     829
    ## trt_con     688    1325    1102     685    1088     965
    ## trt_adt     956    1102    1865     957    1556    1378
    ## trt_ssr     599     685     957    1160     973     846
    ## trt_the     935    1088    1556     973    1853    1361
    ## trt_oth     829     965    1378     846    1361    1647

``` r
diag(cross_counts) <- 0
# Convert cross_counts to a data frame
cross_counts_df <- as.data.frame(cross_counts, stringsAsFactors = FALSE)
cross_counts_df$row <- rownames(cross_counts_df)
cross_counts_df <- cross_counts_df %>%
  gather(col, count, -row) %>%
  mutate(col = factor(col, levels = colnames(cross_counts)))

# Plot heatmap with counts inside (smaller text size)
ggplot(cross_counts_df, aes(x = col, y = row, fill = count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), size = 4, color = "black") +  # Adjust size here
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "Treatment", y = "Treatment", title = "Counts Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

## 5. Final Analysis

Objective:

- Association between Symptoms and treatments

### 5.1 Hypothesis testing

Create a data by removing rows that have NA value

``` r
patient_level_data_NA_rm <- patient_level_data %>%
  filter(!is.na(medical_history_sud) & !is.na(medical_history_tum))
nrow(patient_level_data_NA_rm)
```

    ## [1] 1915

``` r
patient_level_data2 <- patient_level_data_NA_rm[, c(trt_col, symptom_columns)]

patient_level_data2 <- apply(patient_level_data2, 2, function(x) as.numeric(as.character(x)))

patient_level_data2 <- as.data.frame(patient_level_data2)

trt_symp <- patient_level_data2 %>% 
  group_by(across(all_of(c(trt_col, symptom_columns))))

cross_counts <- crossprod(as.matrix(patient_level_data2))

rows_to_remove <- trt_col
columns_to_remove <- symptom_columns

filtered_cross_counts <- cross_counts[!rownames(cross_counts) %in% rows_to_remove, !colnames(cross_counts) %in% columns_to_remove]
filtered_cross_counts
```

    ##           trt_anx trt_con trt_adt trt_ssr trt_the trt_oth
    ## symptom_1     606     694     969     595     965     874
    ## symptom_2     649     762    1072     662    1062     949
    ## symptom_3     534     637     889     563     876     775
    ## symptom_4     714     801    1148     730    1160    1018
    ## symptom_5     520     602     845     512     824     756

Perform Chi-Square test to check association

``` r
chisq.test(as.matrix(filtered_cross_counts))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  as.matrix(filtered_cross_counts)
    ## X-squared = 2.7786, df = 20, p-value = 1

The p value is \>= 0.05 hence it seems there is no significant
association between treatment and symptoms.

### 5.2 Logistic Model

To check association between treatment and medical history by adjusting
for factors like demographics and cgis_score, we will see if we get any
effect.

``` r
# Convert Cgis to factor (As we want to treat it as categorical in model)
patient_level_data_NA_rm$cgis_adm <- as.factor(patient_level_data_NA_rm$cgis_adm)
```

``` r
# Fit logistic regression model
logit_model_anx <- glm(trt_anx ~  cgis_adm + gender + race + resident_status + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_anx <- tidy(logit_model_anx)

# Print adjusted odds ratios and associated statistics
print(tidy_results_anx)
```

    ## # A tibble: 18 × 5
    ##    term                             estimate std.error statistic p.value
    ##    <chr>                               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                      -0.133     0.291     -0.456   0.648 
    ##  2 cgis_adm5                         0.0807    0.108      0.746   0.456 
    ##  3 cgis_adm6                         0.268     0.123      2.18    0.0289
    ##  4 cgis_adm7                        -0.165     0.239     -0.689   0.491 
    ##  5 genderMale                       -0.0610    0.0924    -0.660   0.509 
    ##  6 raceIndian                       -0.0366    0.155     -0.237   0.813 
    ##  7 raceMalay                        -0.135     0.116     -1.16    0.245 
    ##  8 raceOthers                        0.0274    0.210      0.131   0.896 
    ##  9 resident_statusPR                 0.203     0.240      0.845   0.398 
    ## 10 resident_statusSingapore citizen  0.144     0.217      0.664   0.507 
    ## 11 age                              -0.00178   0.00315   -0.566   0.572 
    ## 12 medical_history_dia1              0.160     0.125      1.28    0.201 
    ## 13 medical_history_sud1              0.180     0.100      1.79    0.0729
    ## 14 medical_history_hbp1              0.164     0.135      1.21    0.227 
    ## 15 medical_history_ren1             -0.337     0.208     -1.63    0.104 
    ## 16 medical_history_tum1             -0.157     0.186     -0.843   0.399 
    ## 17 medical_history_anx1              0.104     0.106      0.982   0.326 
    ## 18 medical_history_mood1             0.00537   0.105      0.0512  0.959

``` r
# Fit logistic regression model
logit_model_con <- glm(trt_con ~  cgis_adm + gender + race + resident_status + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_con <- tidy(logit_model_con)

# Print adjusted odds ratios and associated statistics
print(tidy_results_con)
```

    ## # A tibble: 18 × 5
    ##    term                               estimate std.error statistic p.value
    ##    <chr>                                 <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                       0.000656    0.293     0.00223  0.998 
    ##  2 cgis_adm5                        -0.0669      0.110    -0.611    0.541 
    ##  3 cgis_adm6                        -0.00776     0.124    -0.0625   0.950 
    ##  4 cgis_adm7                         0.274       0.250     1.10     0.273 
    ##  5 genderMale                       -0.0364      0.0937   -0.388    0.698 
    ##  6 raceIndian                        0.212       0.159     1.33     0.183 
    ##  7 raceMalay                         0.0167      0.117     0.142    0.887 
    ##  8 raceOthers                        0.283       0.218     1.30     0.193 
    ##  9 resident_statusPR                 0.249       0.240     1.04     0.300 
    ## 10 resident_statusSingapore citizen  0.316       0.217     1.45     0.146 
    ## 11 age                               0.0000311   0.00320   0.00972  0.992 
    ## 12 medical_history_dia1             -0.0912      0.126    -0.722    0.471 
    ## 13 medical_history_sud1              0.0849      0.102     0.834    0.404 
    ## 14 medical_history_hbp1              0.105       0.138     0.761    0.447 
    ## 15 medical_history_ren1             -0.0330      0.208    -0.159    0.874 
    ## 16 medical_history_tum1             -0.265       0.186    -1.42     0.155 
    ## 17 medical_history_anx1              0.254       0.109     2.33     0.0200
    ## 18 medical_history_mood1            -0.0888      0.106    -0.837    0.402

``` r
# Get the OR and confidence intervals
coef_exp <- exp(coef(logit_model_con))
confint_exp <- exp(confint(logit_model_con))
```

    ## Waiting for profiling to be done...

``` r
# Create the dataframe
df_con <- data.frame(
  term = names(coef_exp),
  OR = coef_exp,
  CI_lower = confint_exp[, 1],
  CI_upper = confint_exp[, 2]
)

# Print the dataframe
print(df_con)
```

    ##                                                              term        OR
    ## (Intercept)                                           (Intercept) 1.0006559
    ## cgis_adm5                                               cgis_adm5 0.9352818
    ## cgis_adm6                                               cgis_adm6 0.9922730
    ## cgis_adm7                                               cgis_adm7 1.3145584
    ## genderMale                                             genderMale 0.9642760
    ## raceIndian                                             raceIndian 1.2366651
    ## raceMalay                                               raceMalay 1.0168240
    ## raceOthers                                             raceOthers 1.3270924
    ## resident_statusPR                               resident_statusPR 1.2827752
    ## resident_statusSingapore citizen resident_statusSingapore citizen 1.3713604
    ## age                                                           age 1.0000311
    ## medical_history_dia1                         medical_history_dia1 0.9128282
    ## medical_history_sud1                         medical_history_sud1 1.0885928
    ## medical_history_hbp1                         medical_history_hbp1 1.1108364
    ## medical_history_ren1                         medical_history_ren1 0.9675149
    ## medical_history_tum1                         medical_history_tum1 0.7672628
    ## medical_history_anx1                         medical_history_anx1 1.2892398
    ## medical_history_mood1                       medical_history_mood1 0.9150420
    ##                                   CI_lower CI_upper
    ## (Intercept)                      0.5626915 1.780175
    ## cgis_adm5                        0.7546070 1.159540
    ## cgis_adm6                        0.7783351 1.266496
    ## cgis_adm7                        0.8126771 2.170177
    ## genderMale                       0.8023639 1.158746
    ## raceIndian                       0.9070432 1.696399
    ## raceMalay                        0.8081873 1.281122
    ## raceOthers                       0.8714005 2.050361
    ## resident_statusPR                0.7999046 2.056459
    ## resident_statusSingapore citizen 0.8939266 2.101375
    ## age                              0.9937845 1.006326
    ## medical_history_dia1             0.7130313 1.170679
    ## medical_history_sud1             0.8921798 1.329630
    ## medical_history_hbp1             0.8486196 1.459609
    ## medical_history_ren1             0.6448429 1.462468
    ## medical_history_tum1             0.5328827 1.106902
    ## medical_history_anx1             1.0418468 1.598773
    ## medical_history_mood1            0.7436265 1.126949

``` r
# Create the ggplot
ggplot(df_con, aes(x = term, y = OR)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  scale_y_log10() +  # Log-transform the y-axis for better visualization
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       x = "Variables",
       y = "Odds Ratio (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
# Fit logistic regression model
logit_model_adt <- glm(trt_adt ~  cgis_adm + gender + race + resident_status + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_adt <- tidy(logit_model_adt)

# Print adjusted odds ratios and associated statistics
print(tidy_results_adt)
```

    ## # A tibble: 18 × 5
    ##    term                             estimate std.error statistic      p.value
    ##    <chr>                               <dbl>     <dbl>     <dbl>        <dbl>
    ##  1 (Intercept)                       2.34      0.419      5.59   0.0000000223
    ##  2 cgis_adm5                         0.100     0.144      0.693  0.488       
    ##  3 cgis_adm6                         0.273     0.169      1.61   0.107       
    ##  4 cgis_adm7                         0.655     0.386      1.70   0.0899      
    ##  5 genderMale                        0.0911    0.125      0.728  0.466       
    ##  6 raceIndian                        0.00857   0.215      0.0398 0.968       
    ##  7 raceMalay                        -0.0867    0.157     -0.553  0.580       
    ##  8 raceOthers                       -0.509     0.250     -2.04   0.0417      
    ##  9 resident_statusPR                -0.684     0.343     -1.99   0.0461      
    ## 10 resident_statusSingapore citizen -0.298     0.321     -0.927  0.354       
    ## 11 age                              -0.00796   0.00425   -1.87   0.0611      
    ## 12 medical_history_dia1              0.00765   0.168      0.0454 0.964       
    ## 13 medical_history_sud1              0.0188    0.136      0.139  0.890       
    ## 14 medical_history_hbp1              0.262     0.196      1.34   0.181       
    ## 15 medical_history_ren1              0.515     0.329      1.57   0.117       
    ## 16 medical_history_tum1             -0.325     0.233     -1.40   0.162       
    ## 17 medical_history_anx1             -0.182     0.140     -1.29   0.196       
    ## 18 medical_history_mood1            -0.145     0.139     -1.04   0.298

``` r
# Fit logistic regression model
logit_model_ssr <- glm(trt_ssr ~  cgis_adm + gender + race + resident_status  + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_ssr <- tidy(logit_model_ssr)

# Print adjusted odds ratios and associated statistics
print(tidy_results_ssr)
```

    ## # A tibble: 18 × 5
    ##    term                             estimate std.error statistic p.value
    ##    <chr>                               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                       0.300     0.292      1.03    0.305 
    ##  2 cgis_adm5                         0.0480    0.108      0.444   0.657 
    ##  3 cgis_adm6                         0.0827    0.122      0.676   0.499 
    ##  4 cgis_adm7                         0.215     0.240      0.898   0.369 
    ##  5 genderMale                       -0.153     0.0924    -1.66    0.0971
    ##  6 raceIndian                        0.00157   0.155      0.0102  0.992 
    ##  7 raceMalay                         0.185     0.117      1.59    0.112 
    ##  8 raceOthers                       -0.162     0.210     -0.773   0.439 
    ##  9 resident_statusPR                -0.183     0.241     -0.761   0.447 
    ## 10 resident_statusSingapore citizen -0.186     0.218     -0.854   0.393 
    ## 11 age                              -0.00334   0.00315   -1.06    0.289 
    ## 12 medical_history_dia1              0.0818    0.125      0.653   0.514 
    ## 13 medical_history_sud1              0.109     0.100      1.09    0.277 
    ## 14 medical_history_hbp1              0.0704    0.135      0.519   0.604 
    ## 15 medical_history_ren1             -0.321     0.207     -1.55    0.121 
    ## 16 medical_history_tum1              0.134     0.187      0.719   0.472 
    ## 17 medical_history_anx1              0.135     0.107      1.27    0.204 
    ## 18 medical_history_mood1             0.173     0.105      1.65    0.0991

``` r
# Fit logistic regression model
logit_model_the <- glm(trt_the ~  cgis_adm + gender + race + resident_status  + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_the <- tidy(logit_model_the)

# Print adjusted odds ratios and associated statistics
print(tidy_results_the)
```

    ## # A tibble: 18 × 5
    ##    term                             estimate std.error statistic  p.value
    ##    <chr>                               <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                       1.45      0.390      3.72   0.000197
    ##  2 cgis_adm5                        -0.0811    0.145     -0.561  0.575   
    ##  3 cgis_adm6                        -0.132     0.161     -0.822  0.411   
    ##  4 cgis_adm7                        -0.126     0.311     -0.404  0.686   
    ##  5 genderMale                       -0.166     0.123     -1.35   0.177   
    ##  6 raceIndian                       -0.210     0.196     -1.07   0.283   
    ##  7 raceMalay                         0.0693    0.157      0.441  0.659   
    ##  8 raceOthers                       -0.0104    0.278     -0.0373 0.970   
    ##  9 resident_statusPR                -0.0679    0.322     -0.211  0.833   
    ## 10 resident_statusSingapore citizen -0.0334    0.293     -0.114  0.909   
    ## 11 age                               0.00708   0.00423    1.67   0.0943  
    ## 12 medical_history_dia1             -0.00518   0.167     -0.0310 0.975   
    ## 13 medical_history_sud1             -0.181     0.131     -1.39   0.166   
    ## 14 medical_history_hbp1             -0.0448    0.178     -0.251  0.802   
    ## 15 medical_history_ren1              0.363     0.306      1.19   0.235   
    ## 16 medical_history_tum1             -0.0592    0.245     -0.242  0.809   
    ## 17 medical_history_anx1              0.0560    0.142      0.394  0.693   
    ## 18 medical_history_mood1             0.0160    0.140      0.114  0.909

``` r
# Fit logistic regression model
logit_model_oth <- glm(trt_oth ~  cgis_adm + gender + race + resident_status  + age + medical_history_dia + medical_history_sud + medical_history_hbp + medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood, data = patient_level_data_NA_rm, family = binomial)

# Tidy up the model results using broom package
tidy_results_oth <- tidy(logit_model_oth)

# Print adjusted odds ratios and associated statistics
print(tidy_results_oth)
```

    ## # A tibble: 18 × 5
    ##    term                             estimate std.error statistic p.value
    ##    <chr>                               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                       1.02      0.342      2.97   0.00295
    ##  2 cgis_adm5                         0.0166    0.122      0.136  0.892  
    ##  3 cgis_adm6                         0.164     0.142      1.15   0.249  
    ##  4 cgis_adm7                         0.212     0.281      0.753  0.451  
    ##  5 genderMale                        0.0771    0.105      0.733  0.464  
    ##  6 raceIndian                        0.0142    0.176      0.0805 0.936  
    ##  7 raceMalay                         0.0112    0.132      0.0848 0.932  
    ##  8 raceOthers                        0.289     0.255      1.13   0.258  
    ##  9 resident_statusPR                -0.229     0.286     -0.803  0.422  
    ## 10 resident_statusSingapore citizen -0.202     0.261     -0.773  0.440  
    ## 11 age                               0.00326   0.00360    0.906  0.365  
    ## 12 medical_history_dia1             -0.278     0.137     -2.03   0.0422 
    ## 13 medical_history_sud1              0.0877    0.115      0.763  0.446  
    ## 14 medical_history_hbp1             -0.155     0.150     -1.03   0.302  
    ## 15 medical_history_ren1              0.267     0.248      1.07   0.283  
    ## 16 medical_history_tum1              0.0268    0.213      0.126  0.900  
    ## 17 medical_history_anx1             -0.0263    0.121     -0.218  0.827  
    ## 18 medical_history_mood1            -0.0193    0.119     -0.162  0.871

``` r
# Get the OR and confidence intervals
coef_exp <- exp(coef(logit_model_oth))
confint_exp <- exp(confint(logit_model_oth))
```

    ## Waiting for profiling to be done...

``` r
# Create the dataframe
df_oth <- data.frame(
  term = names(coef_exp),
  OR = coef_exp,
  CI_lower = confint_exp[, 1],
  CI_upper = confint_exp[, 2]
)

# Print the dataframe
print(df_oth)
```

    ##                                                              term        OR
    ## (Intercept)                                           (Intercept) 2.7613268
    ## cgis_adm5                                               cgis_adm5 1.0167772
    ## cgis_adm6                                               cgis_adm6 1.1778344
    ## cgis_adm7                                               cgis_adm7 1.2358973
    ## genderMale                                             genderMale 1.0801750
    ## raceIndian                                             raceIndian 1.0142933
    ## raceMalay                                               raceMalay 1.0112430
    ## raceOthers                                             raceOthers 1.3347994
    ## resident_statusPR                               resident_statusPR 0.7949947
    ## resident_statusSingapore citizen resident_statusSingapore citizen 0.8170681
    ## age                                                           age 1.0032692
    ## medical_history_dia1                         medical_history_dia1 0.7571519
    ## medical_history_sud1                         medical_history_sud1 1.0916394
    ## medical_history_hbp1                         medical_history_hbp1 0.8563073
    ## medical_history_ren1                         medical_history_ren1 1.3054494
    ## medical_history_tum1                         medical_history_tum1 1.0271373
    ## medical_history_anx1                         medical_history_anx1 0.9740208
    ## medical_history_mood1                       medical_history_mood1 0.9808772
    ##                                   CI_lower  CI_upper
    ## (Intercept)                      1.4305709 5.4773826
    ## cgis_adm5                        0.8010366 1.2927599
    ## cgis_adm6                        0.8938525 1.5604856
    ## cgis_adm7                        0.7265607 2.2006186
    ## genderMale                       0.8788370 1.3278628
    ## raceIndian                       0.7222675 1.4438867
    ## raceMalay                        0.7828317 1.3132223
    ## raceOthers                       0.8243484 2.2529258
    ## resident_statusPR                0.4452745 1.3713871
    ## resident_statusSingapore citizen 0.4777173 1.3382463
    ## age                              0.9962299 1.0104012
    ## medical_history_dia1             0.5804184 0.9933678
    ## medical_history_sud1             0.8727381 1.3700881
    ## medical_history_hbp1             0.6404351 1.1546491
    ## medical_history_ren1             0.8163367 2.1723435
    ## medical_history_tum1             0.6838045 1.5809336
    ## medical_history_anx1             0.7704438 1.2365171
    ## medical_history_mood1            0.7781356 1.2413525

``` r
# Create the ggplot
ggplot(df_oth, aes(x = term, y = OR)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  scale_y_log10() +  # Log-transform the y-axis for better visualization
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       x = "Variables",
       y = "Odds Ratio (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![](02_MDD_Analysis_Final_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

### 5.3 Conclusion

- We observe weak relationship between treatments and medical history

- For medical history of anxiety, the odds of prescribing
  anticonvulsants increases by factor of 1.28 which was statistically
  significant

# package libraries 
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)
library(ggthemes)

# source script to query 
source(file = "scripts/01-query.R")

# tidy and transform
fnct_tidy_var_names <- function(x){
  
  y <- x %>%
    mutate(
      sex = if_else(
        condition = str_detect(
          string = variable,
          pattern = "S0101_C03_"
        ),
        true = "Male",
        false = "Female"
      )
    ) %>%
    mutate(
      variable_code = case_when(
        str_detect(
          string = variable,
          pattern = "_002"
        ) ~ "_002",
        str_detect(
          string = variable,
          pattern = "_003"
        )  ~ "_003",
        str_detect(
          string = variable,
          pattern = "_004"
        )  ~ "_004",
        str_detect(
          string = variable,
          pattern = "_005"
        )  ~ "_005",
        str_detect(
          string = variable,
          pattern = "_006"
        )  ~ "_006",
        str_detect(
          string = variable,
          pattern = "_007"
        )  ~ "_007",
        str_detect(
          string = variable,
          pattern = "_008"
        )  ~ "_008",
        str_detect(
          string = variable,
          pattern = "_009"
        )  ~ "_009",
        str_detect(
          string = variable,
          pattern = "_010"
        )  ~ "_010",
        str_detect(
          string = variable,
          pattern = "_011"
        )  ~ "_011",
        str_detect(
          string = variable,
          pattern = "_012"
        )  ~ "_012",
        str_detect(
          string = variable,
          pattern = "_013"
        )  ~ "_013",
        str_detect(
          string = variable,
          pattern = "_014"
        )  ~ "_014",
        str_detect(
          string = variable,
          pattern = "_015"
        )  ~ "_015",
        str_detect(
          string = variable,
          pattern = "_016"
        )  ~ "_016",
        str_detect(
          string = variable,
          pattern = "_017"
        )  ~ "_017",
        str_detect(
          string = variable,
          pattern = "_018"
        )  ~ "_018",
        str_detect(
          string = variable,
          pattern = "_019"
        )  ~ "_019"
      ) 
    )
  
  age_sex_codes <- tribble(
    ~"label", ~"code",
    "0-4 years", "_002",
    "5-9 years", "_003",
    "10-14 years", "_004",
    "15-19 years", "_005",
    "20-24 years", "_006",
    "25-29 years", "_007",
    "30-34 years", "_008",
    "35-39 years", "_009",
    "40-44 years", "_010",
    "45-49 years", "_011",
    "50-54 years", "_012",
    "55-59 years", "_013",
    "60-64 years", "_014",
    "65-69 years", "_015",
    "70-74 years", "_016",
    "75-79 years", "_017",
    "80-84 years", "_018",
    "85+ years", "_019"
  ) %>%
    mutate(
      label = as_factor(label)
    )
  
  
  z <- left_join(
    x = y,
    y = age_sex_codes,
    by = c("variable_code" = "code")
  )
  
  z %>%
    mutate(
      estimate = if_else(
        condition = sex == "Male",
        true = estimate * (-1),
        false = estimate * 1
      )
    )
}

# tidy and transform each data set 
age_sex_2017 <- fnct_tidy_var_names(age_sex_2017)
age_sex_2018 <- fnct_tidy_var_names(age_sex_2018)
age_sex_2019 <- fnct_tidy_var_names(age_sex_2019)
age_sex_2020 <- fnct_tidy_var_names(age_sex_2020)
age_sex_2021 <- fnct_tidy_var_names(age_sex_2021)

# age_sex <- age_sex %>%
#   mutate(
#     sex = if_else(
#       condition = str_detect(
#         string = variable,
#         pattern = "S0101_C03_"
#       ),
#       true = "Male",
#       false = "Female"
#     )
#   ) %>%
#   mutate(
#     variable_code = case_when(
#       str_detect(
#         string = variable,
#         pattern = "_002"
#       ) ~ "_002",
#       str_detect(
#         string = variable,
#         pattern = "_003"
#       )  ~ "_003",
#       str_detect(
#         string = variable,
#         pattern = "_004"
#       )  ~ "_004",
#       str_detect(
#         string = variable,
#         pattern = "_005"
#       )  ~ "_005",
#       str_detect(
#         string = variable,
#         pattern = "_006"
#       )  ~ "_006",
#       str_detect(
#         string = variable,
#         pattern = "_007"
#       )  ~ "_007",
#       str_detect(
#         string = variable,
#         pattern = "_008"
#       )  ~ "_008",
#       str_detect(
#         string = variable,
#         pattern = "_009"
#       )  ~ "_009",
#       str_detect(
#         string = variable,
#         pattern = "_010"
#       )  ~ "_010",
#       str_detect(
#         string = variable,
#         pattern = "_011"
#       )  ~ "_011",
#       str_detect(
#         string = variable,
#         pattern = "_012"
#       )  ~ "_012",
#       str_detect(
#         string = variable,
#         pattern = "_013"
#       )  ~ "_013",
#       str_detect(
#         string = variable,
#         pattern = "_014"
#       )  ~ "_014",
#       str_detect(
#         string = variable,
#         pattern = "_015"
#       )  ~ "_015",
#       str_detect(
#         string = variable,
#         pattern = "_016"
#       )  ~ "_016",
#       str_detect(
#         string = variable,
#         pattern = "_017"
#       )  ~ "_017",
#       str_detect(
#         string = variable,
#         pattern = "_018"
#       )  ~ "_018",
#       str_detect(
#         string = variable,
#         pattern = "_019"
#       )  ~ "_019"
#     ) 
#   )
# 
# age_sex_codes <- tribble(
#   ~"label", ~"code",
#   "0-4 years", "_002",
#   "5-9 years", "_003",
#   "10-14 years", "_004",
#   "15-19 years", "_005",
#   "20-24 years", "_006",
#   "25-29 years", "_007",
#   "30-34 years", "_008",
#   "35-39 years", "_009",
#   "40-44 years", "_010",
#   "45-49 years", "_011",
#   "50-54 years", "_012",
#   "55-59 years", "_013",
#   "60-64 years", "_014",
#   "65-69 years", "_015",
#   "70-74 years", "_016",
#   "75-79 years", "_017",
#   "80-84 years", "_018",
#   "85+ years", "_019"
# ) %>%
#   mutate(
#     label = as_factor(label)
#   )
# 
# 
# age_sex <- left_join(
#   x = age_sex,
#   y = age_sex_codes,
#   by = c("variable_code" = "code")
# )
# 
# age_sex <- age_sex %>%
#   mutate(
#     estimate = if_else(
#       condition = sex == "Male",
#       true = estimate * (-1),
#       false = estimate * 1
#     )
#   )
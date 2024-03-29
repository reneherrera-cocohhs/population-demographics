# package libraries 
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)

# inspect the variables available 
# v16 <- load_variables(2016, "acs5/subject", cache = TRUE)
# v17 <- load_variables(2017, "acs5/subject", cache = TRUE)
# v18 <- load_variables(2018, "acs5/subject", cache = TRUE)
# v19 <- load_variables(2019, "acs5/subject", cache = TRUE)
# v20 <- load_variables(2020, "acs5/subject", cache = TRUE)
# v21 <- load_variables(2021, "acs5/subject", cache = TRUE)

fnct_qry_acs <- function(x){
  age_sex <- get_acs(
    geography = "county",
    survey = "acs5",
    cache_table = TRUE,
    year = x,
    state = "AZ",
    county = "Coconino",
    variables = c(
      str_c(
        "S0101_C03_00",
        seq(2, 9, 1)
      ),
      str_c(
        "S0101_C03_0",
        seq(10, 19, 1)
      ),
      str_c(
        "S0101_C05_00",
        seq(2, 9, 1)
      ),
      str_c(
        "S0101_C05_0",
        seq(10, 19, 1)
      )
    )
  ) 
}

fnct_qry_acs_2 <- function(x){
  age_sex <- get_acs(
    geography = "county",
    survey = "acs5",
    cache_table = TRUE,
    year = x,
    state = "AZ",
    county = "Coconino",
    variables = c(
      str_c(
        "S0101_C02_00",
        seq(2, 9, 1)
      ),
      str_c(
        "S0101_C02_0",
        seq(10, 19, 1)
      ),
      str_c(
        "S0101_C03_00",
        seq(2, 9, 1)
      ),
      str_c(
        "S0101_C03_0",
        seq(10, 19, 1)
      )
    )
  ) 
}

# age_sex <- get_acs(
#   geography = "county",
#   survey = "acs5",
#   cache_table = TRUE,
#   year = 2021,
#   state = "AZ",
#   county = "Coconino",
#   variables = c(
#     str_c(
#       "S0101_C03_00",
#       seq(2, 9, 1)
#     ),
#     str_c(
#       "S0101_C03_0",
#       seq(10, 19, 1)
#     ),
#     str_c(
#       "S0101_C05_00",
#       seq(2, 9, 1)
#     ),
#     str_c(
#       "S0101_C05_0",
#       seq(10, 19, 1)
#     )
#   )
# ) 
# 

# age_sex_2016 <- fnct_qry_acs_2(2016)
age_sex_2017 <- fnct_qry_acs(2017)
age_sex_2018 <- fnct_qry_acs(2018)
age_sex_2019 <- fnct_qry_acs(2019)
age_sex_2020 <- fnct_qry_acs(2020)
age_sex_2021 <- fnct_qry_acs(2021)


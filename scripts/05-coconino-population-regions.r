# package libraries ####
library(here)
library(tidyverse)
library(janitor)
library(tidycensus)
library(mapview)
library(knitr)
library(gtsummary)
library(ggthemes)
library(scales)

options(tigris_use_cache = TRUE)

# set variables ####
var_dhc <- load_variables(2020, dataset = "dhc", cache = TRUE)

var_dhc %>%
  distinct(concept) %>%
  filter(str_detect(concept, "INC"))
  print(n = 246)

var20_dp <- load_variables(2020, cache = TRUE, dataset = "dp")

var20_dp %>%
  distinct(label) %>%
  filter(str_detect(label, "INC"))

var20_ddhca <- load_variables(2020, cache = TRUE, dataset = "ddhca")

var20_ddhca %>%
  distinct(label) 

var20_pl <- load_variables(2020, cache = TRUE, dataset = "pl")

var20_pl %>%
  distinct(concept) %>%
  filter(str_detect(label, "POV"))

var20_acs5 <- load_variables(2020, cache = TRUE, dataset = "acs5")

var20_acs5 %>%
  filter(label == "Estimate!!Median monthly housing costs")
  distinct(label) %>%
  filter(str_detect(str_to_lower(label), "median")) %>%
  print(n = 249)


race_vars <- c(
  "White" = "P1_003N",
  "Black" = "P1_004N",
  "AIAN" = "P1_005N",
  "Asian" = "P1_006N",
  "NHPI" = "P1_007N",
  "Other" = "P1_008N"
)

sex_vars <- c(
  "Male" = "PCT13_002N",
  "Female" = "PCT13_026N"
)

age_sex_vars <- c(
  "Total" = "PCT13_001N",
  "Male" = "PCT13_002N",
  "M_0_4" = "PCT13_003N",
  "M_5_9" = "PCT13_004N",
  "M_10_14" = "PCT13_005N",
  "M_15_17" = "PCT13_006N",
  "M_18_19" = "PCT13_007N",
  "M_20" = "PCT13_008N",
  "M_21" = "PCT13_009N",
  "M_22_24" = "PCT13_010N",
  "M_25_29" = "PCT13_011N",
  "M_30_34" = "PCT13_012N",
  "M_35_39" = "PCT13_013N",
  "M_40_44" = "PCT13_014N",
  "M_45_49" = "PCT13_015N",
  "M_50_54" = "PCT13_016N",
  "M_55_59" = "PCT13_017N",
  "M_60_61" = "PCT13_018N",
  "M_62_64" = "PCT13_019N",
  "M_65_66" = "PCT13_020N",
  "M_67_69" = "PCT13_021N",
  "M_70_74" = "PCT13_022N",
  "M_75_79" = "PCT13_023N",
  "M_80_84" = "PCT13_024N",
  "M_85+" = "PCT13_025N",
  "Female" = "PCT13_026N",
  "F_0_4" = "PCT13_027N",
  "F_5_9" = "PCT13_028N",
  "F_10_14" = "PCT13_029N",
  "F_15_17" = "PCT13_030N",
  "F_18_19" = "PCT13_031N",
  "F_20" = "PCT13_032N",
  "F_21" = "PCT13_033N",
  "F_22_24" = "PCT13_034N",
  "F_25_29" = "PCT13_035N",
  "F_30_34" = "PCT13_036N",
  "F_35_39" = "PCT13_037N",
  "F_40_44" = "PCT13_038N",
  "F_45_49" = "PCT13_039N",
  "F_50_54" = "PCT13_040N",
  "F_55_59" = "PCT13_041N",
  "F_60_61" = "PCT13_042N",
  "F_62_64" = "PCT13_043N",
  "F_65_66" = "PCT13_044N",
  "F_67_69" = "PCT13_045N",
  "F_70_74" = "PCT13_046N",
  "F_75_79" = "PCT13_047N",
  "F_80_84" = "PCT13_048N",
  "F_85+" = "PCT13_049N"
)

# by region ####
coco_pop_sf <- get_decennial(
  geography = "tract",
  variables = "P1_001N", # !!Total:
  state = "AZ",
  geometry = TRUE,
  # sumfile = "dp",
  year = 2020
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))

coco_pop <- get_decennial(
  geography = "tract",
  # variables = "P1_001N", # !!Total:
  variables = c(
    "Total" = "P1_001N", 
    race_vars
  ),
  state = "AZ",
  geometry = FALSE,
  # sumfile = "dp",
  year = 2020
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))

class(coco_pop)
glimpse(coco_pop)

coco_pop_regions_v1 <- coco_pop %>%
  mutate(
    region = case_when(
      GEOID == "04005980000" ~ "North",
      GEOID == "04005002000" ~ "North",
      GEOID == "04005002102" ~ "North",
      GEOID == "04005002101" ~ "North",
      GEOID == "04005942202" ~ "North",
      GEOID == "04005942201" ~ "North",
      GEOID == "04005945000" ~ "North",
      GEOID == "04005002301" ~ "West",
      GEOID == "04005002302" ~ "West",
      GEOID == "04005001700" ~ "West",
      GEOID == "04005980100" ~ "West",
      GEOID == "04005945200" ~ "East",
      GEOID == "04005944900" ~ "East",
      GEOID == "04005945100" ~ "East",
      GEOID == "04005001500" ~ "East",
      TRUE ~ "South"
    )
  )

coco_pop_regions_v2 <- coco_pop %>%
  mutate(
    region = case_when(
      GEOID == "04005980000" ~ "North",
      GEOID == "04005002000" ~ "North",
      GEOID == "04005002102" ~ "North",
      GEOID == "04005002101" ~ "North",
      GEOID == "04005942202" ~ "North",
      GEOID == "04005942201" ~ "North",
      GEOID == "04005945000" ~ "North",
      GEOID == "04005002301" ~ "West",
      GEOID == "04005002302" ~ "West",
      GEOID == "04005001700" ~ "West",
      GEOID == "04005980100" ~ "West",
      GEOID == "04005002201" ~ "West", #v2
      GEOID == "04005945200" ~ "East",
      GEOID == "04005944900" ~ "East",
      GEOID == "04005945100" ~ "East",
      GEOID == "04005001500" ~ "East",
      GEOID == "04005001302" ~ "East", #v2
      TRUE ~ "South"
    )
  )

coco_pop_regions_v3 <- coco_pop %>%
  mutate(
    region = case_when(
      GEOID == "04005980000" ~ "North",
      GEOID == "04005002000" ~ "North",
      GEOID == "04005002102" ~ "North",
      GEOID == "04005002101" ~ "North",
      GEOID == "04005942202" ~ "East", #
      GEOID == "04005942201" ~ "East", # 
      GEOID == "04005945000" ~ "East", # 
      GEOID == "04005002301" ~ "West",
      GEOID == "04005002302" ~ "West",
      GEOID == "04005001700" ~ "West",
      GEOID == "04005980100" ~ "West",
      # GEOID == "04005002201" ~ "West", South
      GEOID == "04005945200" ~ "East",
      GEOID == "04005944900" ~ "East",
      GEOID == "04005945100" ~ "East",
      # GEOID == "04005001500" ~ "East", South
      # GEOID == "04005001302" ~ "East", South
      TRUE ~ "South"
    )
  )

mapview(coco_pop_sf, zcol = "value")

mapview(coco_pop_regions_v3, zcol = "region")

coco_pop_totals <- coco_pop_regions_v3 %>%
  filter(variable == "Total") %>%
  group_by(region) %>%
  summarise(total = sum(value)) %>%
  ungroup() 

coco_pop_totals %>%
  mutate(percent = 100 * (total / sum(total))) %>%
  kable(
    col.names = c("Region", "Total population", "Percent of total"),
    digits = 2
  )

geoid_by_region <- coco_pop_regions_v3 %>%
  distinct(GEOID, region)

coco_pop_sf %>%
  left_join(
    y = geoid_by_region,
    by = "GEOID"
  )  %>%
  ggplot() +
  geom_sf(
    mapping = aes(
      fill = region
    )
  ) +
  scale_fill_canva(palette = "Crisp and dramatic") +
  theme_map() +
  theme(aspect.ratio = 4/3)

ggsave(
  filename = "data-viz/coco-regions-v3.png",
  device = "png",
  width = 6/2,
  height = 8/2
)

# by demographic variable ####

## population by race - ethnicity #### 
coco_pop_totals <- coco_pop_totals %>%
  mutate(variable = "Total")

coco_pop_race <- coco_pop_regions_v3 %>%
  filter(variable != "Total") %>%
  # filter(variable != "Other") %>%
  group_by(region, variable) %>%
  summarise(total = sum(value)) %>%
  # mutate(percent = 100 * (total / sum(total))) %>%
  ungroup() 

coco_pop_race_oth <- coco_pop_race %>%
  filter(variable != "Other") %>%
  group_by(region) %>%
  summarise(race = sum(total)) %>%
  ungroup() %>%
  left_join(
    y = coco_pop_totals,
    by = c("region")
  ) %>%
  mutate(
    other = total - race,
    variable = "Other"
  ) %>%
  select(
    region,
    variable,
    total = other
  )
  
coco_pop_race %>%
  filter(variable != "Other") %>%
  full_join(
    y = coco_pop_race_oth,
    by = c("region", "total", "variable")
  ) %>%
  group_by(region) %>%
  mutate(percent = 100 * (total / sum(total))) %>%
  ungroup() %>%
  arrange(region, variable) %>%
  kable(
    col.names = c(
      "Region", 
      "Race", 
      "Population", 
      "Percent of region total"
    ),
    digits = 2
  )


## population by age & sex ####

coco_pop_sex <- get_decennial(
  geography = "tract",
  # variables = "P1_001N", # !!Total:
  variables = c(
    "Total" = "P1_001N", 
    sex_vars
  ),
  state = "AZ",
  geometry = FALSE,
  sumfile = "dhc",
  year = 2020
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))

coco_pop_sex %>%
  left_join(
    y = geoid_by_region,
    by = "GEOID"
  )  %>%
  filter(variable != "Total") %>%
  group_by(region, variable) %>%
  summarise(total = sum(value)) %>%
  mutate(percent = 100 * (total / sum(total))) %>%
  ungroup() %>%
  kable(
    col.names = c(
      "Region", 
      "Variable", 
      "Population", 
      "Percent of region total"
    ),
    digits = 2
  )

coco_pop_age_sex <- get_decennial(
  geography = "tract",
  # variables = "P1_001N", # !!Total:
  variables = c(
    age_sex_vars
  ),
  state = "AZ",
  geometry = FALSE,
  sumfile = "dhc",
  year = 2020
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))

coco_pop_age_sex <- coco_pop_age_sex %>% 
  mutate(
  sex = case_when(
    str_detect(variable, "Male|M_") ~ "Male",
    str_detect(variable, "Female|F_") ~ "Female"
  ),
  age_group = factor(case_when(
    str_detect(variable, "_0_4") ~ "0 to 4",
    str_detect(variable, "_5_9") ~ "5 to 17",
    str_detect(variable, "_10_14") ~ "5 to 17",
    str_detect(variable, "_15_17") ~ "5 to 17",
    str_detect(variable, "_18_19") ~ "18 to 24",
    str_detect(variable, "_20") ~ "18 to 24",
    str_detect(variable, "_21") ~ "18 to 24",
    str_detect(variable, "_22_24") ~ "18 to 24",
    str_detect(variable, "_25_29") ~ "25 to 44",
    str_detect(variable, "_30_34") ~ "25 to 44",
    str_detect(variable, "_35_39") ~ "25 to 44",
    str_detect(variable, "_40_44") ~ "25 to 44",
    str_detect(variable, "_45_49") ~ "45 to 64",
    str_detect(variable, "_50_54") ~ "45 to 64",
    str_detect(variable, "_55_59") ~ "45 to 64",
    str_detect(variable, "_60_61") ~ "45 to 64",
    str_detect(variable, "_62_64") ~ "45 to 64",
    str_detect(variable, "_65_66") ~ "65 to 84",
    str_detect(variable, "_67_69") ~ "65 to 84",
    str_detect(variable, "_70_74") ~ "65 to 84",
    str_detect(variable, "_75_79") ~ "65 to 84",
    str_detect(variable, "_80_84") ~ "65 to 84",
    str_detect(variable, "_85") ~ "85+"
  ), 
  ordered = TRUE,
  levels = c(
    "0 to 4",
    "5 to 17",
    "18 to 24",
    "25 to 44",
    "45 to 64",
    "65 to 84",
    "85+"
  )
  )
) %>%
  mutate(
    age_sex = factor(
      str_c(sex, age_group, sep = " - "), 
      ordered = TRUE,
      levels = c(
        "Female - 0 to 4",
        "Female - 5 to 17",
        "Female - 18 to 24",
        "Female - 25 to 44",
        "Female - 45 to 64",
        "Female - 65 to 84",
        "Female - 85+",
        "Male - 0 to 4",
        "Male - 5 to 17",
        "Male - 18 to 24",
        "Male - 25 to 44",
        "Male - 45 to 64",
        "Male - 65 to 84",
        "Male - 85+"
      )
    )
  )

levels(coco_pop_age_sex$age_sex)
levels(coco_pop_age_sex$age_group)

coco_pop_age_sex %>%
  left_join(
    y = geoid_by_region,
    by = "GEOID"
  )  %>%
  filter(!variable %in% c("Total", "Male", "Female")) %>%
  group_by(region, age_sex) %>%
  summarise(total = sum(value)) %>%
  mutate(percent = 100 * (total / sum(total))) %>%
  ungroup() %>%
  kable(
    col.names = c(
      "Region", 
      "Variable", 
      "Population", 
      "Percent of region total"
    ),
    digits = 2
  )

coco_pop_age_sex %>%
  left_join(
    y = geoid_by_region,
    by = "GEOID"
  )  %>%
  filter(!is.na(age_group)) %>%
  group_by(region, age_group) %>%
  summarise(total = sum(value)) %>%
  mutate(percent = 100 * (total / sum(total))) %>%
  ungroup() %>%
  kable(
    col.names = c(
      "Region", 
      "Age", 
      "Population", 
      "Percent of region total"
    ),
    digits = 2
  )

# by socioeconomic indicator ####


## poverty percent total population ####
poverty20 <- get_acs(
  geography = "tract",
  variables = c(
    "Total" = "B17020_001",
    "Below" = "B17020_002"
  ),
  year = 2020,
  output = "wide",
  state = "AZ",
  county = "Coconino"
)

poverty20 <- poverty20 %>%
  mutate(pov_pct = 100 * (BelowE / TotalE))

coco_pop %>%
  filter(variable == "Total") %>%
  right_join(
    y = poverty20
  )
  
left_join(
  x = poverty20,
  y = geoid_by_region
  ) %>%
  group_by(region) %>%
  summarize(
    total_est = sum(TotalE),
    pov_est = sum(BelowE)
  ) %>%
  ungroup() %>%
  mutate(
    pct_pov = 100 * (pov_est / total_est)
  ) %>%
  select(1, 3, 2, 4) %>%
  kable(
    col.names = c("Region", "Below poverty est.", "Total est.", "Percent below poverty est."),
    caption = "Data source: 2020 ACS5",
    digits = 2
  )

## median income

income20 <- get_acs(
  geography = "tract",
  variables = c(
    "med_ann_inc" = "B19326_001"
  ),
  year = 2020,
  output = "wide",
  state = "AZ",
  county = "Coconino"
)

income20 <- income20 %>%
  mutate(
    medinc_ll = med_ann_incE - med_ann_incM,
    medinc_ul = med_ann_incE + med_ann_incM,
  )

income20_tab <- left_join(
  x = income20,
  y = geoid_by_region
) %>%
  group_by(region) %>%
  summarize(
    mean_medinc_est = mean(med_ann_incE, na.rm = TRUE),
    mean_medinc_ll_est = mean(medinc_ll, na.rm = TRUE),
    mean_medinc_ul_est = mean(medinc_ul, na.rm = TRUE)
  ) %>%
  ungroup() 

income20_tab %>%
  kable(
    col.names = c("Region", "Mean of median income est.", "Lower limit est.", "Upper limit est."),
    caption = "Data source: 2020 ACS5",
    digits = 2
  )

income20_tab %>%
  ggplot() + 
  geom_col(
    mapping = aes(
      x = region,
      y = mean_medinc_est
    ), fill = "#cf6766",
    alpha = 6/7
  ) + 
  geom_errorbar(
    mapping = aes(
      x = region,
      ymin = mean_medinc_ll_est,
      ymax = mean_medinc_ul_est
    )
  ) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    x = "Region",
    y = "Mean of Median Annual Income (Estimate)",
    caption = "Data source: 2020 ACS5"
  ) +
  theme_few()

ggsave(
  filename = "data-viz/coco-regions-median-income.png",
  device = "png",
  width = 8/2,
  height = 6/2,
  scale = 5/3
)

## median monthly housing cost

housing20 <- get_acs(
  geography = "tract",
  variables = c(
    "var" = "B25105_001"
  ),
  year = 2020,
  output = "wide",
  state = "AZ",
  county = "Coconino"
)

housing20 <- housing20 %>%
  mutate(
    var_ll = varE - varM,
    var_ul = varE + varM,
  )

housing20_tab <- left_join(
  x = housing20,
  y = geoid_by_region
) %>%
  group_by(region) %>%
  summarize(
    mean_medhc_est = mean(varE, na.rm = TRUE),
    mean_medhc_ll_est = mean(var_ll, na.rm = TRUE),
    mean_medhc_ul_est = mean(var_ul, na.rm = TRUE)
  ) %>%
  ungroup() 

housing20_tab %>%
  kable(
    col.names = c("Region", "Mean of median monthly housing cost est.", "Lower limit est.", "Upper limit est."),
    caption = "Data source: 2020 ACS5",
    digits = 2
  )

housing20_tab %>%
  ggplot() + 
  geom_col(
    mapping = aes(
      x = region,
      y = mean_medhc_est
    ), fill = "#688b8a",
    alpha = 6/7
  ) + 
  geom_errorbar(
    mapping = aes(
      x = region,
      ymin = mean_medhc_ll_est,
      ymax = mean_medhc_ul_est
    )
  ) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    x = "Region",
    y = "Mean of median monthly housing cost (Estimate)",
    caption = "Data source: 2020 ACS5"
  ) +
  theme_few()

ggsave(
  filename = "data-viz/coco-regions-median-housing-cost.png",
  device = "png",
  width = 8/2,
  height = 6/2,
  scale = 5/3
)

library(tigris)
library(sf)

# county map
coco_county_sf <- counties(
  state = "AZ",
  year = 2020
) %>%
  filter(NAME == "Coconino")

coco_county_bb <- st_as_sfc(st_bbox(coco_county_sf), crs = "4269")

# zip code map
coco_zip_sf <- zctas(
  year = 2010,
  state = "AZ"
) 

coco_zip_bb_sf <- st_intersection(
  x = coco_county_bb,
  y = coco_zip_sf
)

# # $`Warm and rustic`
#  [1] "#fef2e4" "#fd974f" "#c60000" "#805a3b"

ggplot() + 
  geom_sf(
    data = coco_county_bb,
    fill = "white"
  ) +
  geom_sf(
    data = coco_county_sf,
    fill = "#fef2e4",
    linewidth = 2
  ) +
  geom_sf(
    data = coco_zip_bb_sf,
    fill = "#fd974f",
    alpha = 4/7
  ) +
  theme_map()

ggsave(
  filename = "data-viz/coco-zip-codes.png",
  device = "png",
  width = 8/2,
  height = 6/2
)

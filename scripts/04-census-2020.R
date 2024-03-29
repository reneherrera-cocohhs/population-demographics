library(tidycensus)
library(tidyverse)
library(mapview)

library(ggthemes)
library(scales)
library(janitor)

options(tigris_use_cache = TRUE)

# install.packages(c("tidycensus", "tidyverse", "mapview"))


## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)

pop20 <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = "P1_001N",
  year = 2020
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

# canva_palettes
# 
# fivenum(pop20$value)
# 
# pop20 %>%
#   mutate(value_pm = value / 1000000) %>%
#   mutate(
#     group_number = cut_number(value_pm, 4),
#     group_interval = cut_interval(value_pm, 4),
#     group_width = cut_number(value_pm, (50/4)),
#   ) %>%
#   # mutate(value_code = case_when(
#   #   value < (1816411 * 2.75) ~ "Q1",
#   #   value < 4371546 * 2 ~ "Q2",
#   #   value < 7428392 * 2 ~ "Q3",
#   #   TRUE ~ "Q4"
#   # )) %>%
#   group_by(group_interval) %>%
#   summarise(
#     total = sum(value)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     percent = total / sum(total)
#   )
# # reframe(
# #   fivenum(value)
# # )
# arrange(desc(value)) %>%
#   ggplot() +
#   # geom_histogram(mapping = aes(x = value))
#   geom_col(
#     mapping = aes(
#       x = reorder(NAME, value),
#       y = value,
#       fill = group_interval
#     )) +
#   coord_flip() +
#   # scale_fill_canva(palette = "Antique and clean") +
#   # scale_color_canva(palette = "Antique tones") +
#   theme_clean()
# print(n = 52)
table_p1 <- get_decennial(
  geography = "county",
  state = "AZ",
  table = "P2", 
  year = 2020
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

table_p1

var_pl <- load_variables(year = 2020, dataset = "pl")

table_p2 <- get_decennial(
  geography = "county",
  state = "AZ",
  table = "P2", 
  year = 2020
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

table_p2

coco_population <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  state = "AZ",
  sumfile = "dhc",
  year = 2020
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

coco_population

coco_blocks <- get_decennial(
  geography = "block",
  variables = "P1_001N",
  state = "AZ",
  county = "Coconino",
  sumfile = "dhc",
  year = 2020
)

coco_blocks

vars <- load_variables(2020, "dhc")

View(vars)

single_year_age <- get_decennial(
  geography = "county",
  state = "AZ",
  table = "PCT12",
  year = 2020,
  sumfile = "dhc"
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

single_year_age

single_year_age_wide <- get_decennial(
  geography = "county",
  state = "AZ",
  table = "PCT12",
  year = 2020,
  sumfile = "dhc",
  output = "wide" 
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

single_year_age_wide

coco_samesex <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = c(married = "DP1_0116P",
                partnered = "DP1_0118P"),
  year = 2020,
  sumfile = "dp",
  output = "wide"
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

coco_samesex

library(tidyverse)

tidyverse_logo()

library(tidycensus)
library(tidyverse)

coco_population <- get_decennial(
  geography = "tract",
  state = "AZ",
  variables = "P1_001N",
  year = 2020,
  sumfile = "dhc"
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))


arrange(coco_population, value)

arrange(coco_population, desc(value))

fivenum(coco_population$value)

ggplot(data = coco_population) +
  geom_histogram(mapping = aes(x = value))

coco_population %>%
  mutate(
    group_interval = cut_interval(value, 5),
    group_number = cut_number(value, 5),
    group_width = cut_width(value, median(coco_population$value)),
  ) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = reorder(NAME, value),
      y = value,
      fill = group_interval
    )) +
  coord_flip() +
  theme_clean()

  group_by(group_number) %>%
  summarise(
    total = sum(value)
  ) %>%
  ungroup() %>%
  mutate(
    percent = total / sum(total)
  )

below1000 <- filter(tx_population, value < 1000)

below1000

race_vars <- c(
  Hispanic = "P5_010N",
  White = "P5_003N",
  Black = "P5_004N",
  Native = "P5_005N",
  Asian = "P5_006N",
  HIPI = "P5_007N"
)

cd_race <- get_decennial(
  geography = "congressional district",
  variables = race_vars,
  summary_var = "P5_001N", 
  year = 2020,
  sumfile = "cd118"
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005")

cd_race

cd_race_percent <- cd_race %>%
  mutate(percent = 100 * (value / summary_value)) %>% 
  select(NAME, variable, percent) 

cd_race_percent

largest_group <- cd_race_percent %>%
  group_by(NAME) %>% 
  filter(percent == max(percent)) 

# Optionally, use `.by`: 
# largest_group <- cd_race_percent %>%
#   filter(percent == max(percent), .by = NAME) 

largest_group

cd_race_percent %>%
  group_by(variable) %>% 
  summarize(median_pct = median(percent, na.rm = TRUE)) 

coco_over_65 <- get_decennial(
  geography = "tract",
  variables = "DP1_0024P",
  state = "AZ",
  geometry = TRUE,
  sumfile = "dp",
  year = 2020
) %>%
  filter(str_detect(str_to_lower(NAME), "coconino"))

coco_over_65

library(mapview)

mapview(coco_over_65)

mapview(coco_over_65, zcol = "value")

mapview(coco_over_65, zcol = "value",
        layer.name = "% age 65 and up<br>Census tracts in COCO")


library(viridisLite)

mapview(iowa_over_65, zcol = "value",
        layer.name = "% age 65 and up<br>Census tracts in Iowa",
        col.regions = inferno(100))

## library(htmlwidgets)
## 
## m1 <- mapview(iowa_over_65, zcol = "value",
##         layer.name = "% age 65 and up<br>Census tracts in Iowa",
##         col.regions = inferno(100))
## 
## saveWidget(m1@map, "iowa_over_65.html")
## 

mn_population_groups <- get_decennial(
  geography = "state",
  variables = "T01001_001N",
  state = "MN",
  year = 2020,
  sumfile = "ddhca",
  pop_group = "all",
  pop_group_label = TRUE
)


mn_population_groups

az_population_groups <- get_decennial(
  geography = "state",
  variables = "T01001_001N",
  state = "AZ",
  year = 2020,
  sumfile = "ddhca",
  pop_group = "all",
  pop_group_label = TRUE
)

az_population_groups %>%
  arrange(desc(value)) %>%
  filter(!is.na(value)) %>%
  mutate(percent = value / sum(value))


available_groups <- get_pop_groups(2020, "ddhca")

get_decennial(
  geography = "county",
  variables = "T02001_001N",
  state = "MN",
  county = "Hennepin",
  pop_group = "1325",
  year = 2020,
  sumfile = "ddhca"
)

check_ddhca_groups(
  geography = "county", 
  pop_group = "1325", 
  state = "MN", 
  county = "Hennepin"
)

library(tidycensus)

hennepin_somali <- get_decennial(
  geography = "tract",
  variables = "T01001_001N",
  state = "MN",
  county = "Hennepin",
  year = 2020,
  sumfile = "ddhca",
  pop_group = "1325",
  pop_group_label = TRUE,
  geometry = TRUE
)


mapview(hennepin_somali, zcol = "value")

somali_dots <- as_dot_density(
  hennepin_somali,
  value = "value",
  values_per_dot = 25
)

mapview(somali_dots, cex = 0.01, layer.name = "Somali population<br>1 dot = 25 people",
        col.regions = "navy", color = "navy")

county_pop_10 <- get_decennial(
  geography = "county",
  variables = "P001001", 
  year = 2010,
  sumfile = "sf1"
)


county_pop_10

county_pop_10_clean <- county_pop_10 %>%
  select(GEOID, value10 = value) 

county_pop_10_clean

county_pop_20 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  sumfile = "dhc"
) %>%
  select(GEOID, NAME, value20 = value)

county_joined <- county_pop_20 %>%
  left_join(county_pop_10_clean, by = "GEOID") 

county_joined

county_change <- county_joined %>%
  mutate( 
    total_change = value20 - value10, 
    percent_change = 100 * (total_change / value10) 
  ) 


county_change

filter(county_change, is.na(value10))



# bonus chart: age distributions in Coconino County, AZ ####
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)

ages <- c(0:99, rep(100, 3))

male_vars <- paste0("PCT012", str_pad(3:105, 3, "left", "0"))
female_vars <- paste0("PCT012", 107:209)

names(male_vars) <- ages
names(female_vars) <- ages

all_vars <- c(male_vars, female_vars)

pull00 <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = all_vars,
  year = 2000
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005") %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2000")

pull10 <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = all_vars,
  year = 2010
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005") %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2010")

male_vars20 <- paste0("PCT12_", str_pad(3:105, 3, "left", "0"), "N")
female_vars20 <- paste0("PCT12_", 107:209, "N")

names(male_vars20) <- ages
names(female_vars20) <- ages

all_vars20 <- c(male_vars20, female_vars20)

pull20 <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = all_vars20,
  year = 2020,
  sumfile = "dhc"
) %>%
  filter(NAME == "Coconino County, Arizona" | GEOID == "04005") %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2020")

all_years <- bind_rows(pull00, pull10, pull20)

ggplot(all_years, aes(x = as.numeric(variable), y = value, color = year,
                      group = year)) + 
  geom_line() + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") + 
  scale_y_continuous(labels = scales::label_comma()) + 
  labs(y = "Population",
       x = "Single-year age",
       color = "Year",
       title = glue::glue("Age distributions in Coconino County, Arizona"))

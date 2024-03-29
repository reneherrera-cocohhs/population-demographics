# package libraries 
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)
library(ggthemes)
library(gganimate)

# source script 
source(file = "scripts/02-tidy-transform.R")

age_sex <- mget(ls(pattern = "age_sex_")) %>%
  bind_rows(.id = "data_name") %>%
  mutate(
    year = (str_extract(
      string = data_name,
      pattern = "[:digit:]{4}"
    ))
  )

# 

age_sex_plot <- age_sex %>%
  ggplot(
    mapping = aes(
      x = label,
      y = estimate,
      fill = sex
    )
  ) +
  geom_col(
    data = subset(age_sex, sex == "Male"), alpha = 5/6
  ) +
  geom_col(
    data = subset(age_sex, sex == "Female"), alpha = 5/6
  ) +
  scale_y_continuous(
    breaks = seq(-12500, 12500, 2500),
    labels = paste0(as.character(c(seq(12.5, 0, -2.5), seq(2.5, 12.5, 2.5))), "k")
  ) +
  scale_fill_brewer(palette = "Set1") + 
  coord_flip() +
  labs(
    title = "Coconino County Population Pyramid",
    # subtitle = "2017-2021 5-Year US Census ACS",
    x = "Age",
    y = "Population",
    fill = "Sex"
  ) +
  theme_fivethirtyeight() +
  theme(aspect.ratio = 9/16)

ggsave(
  filename = "data-viz/population-pyramid.png"
)

# animated gif 
age_sex_plot +
  transition_states(
    states = year, 
    transition_length = 1
  ) +
  labs(subtitle = "US Census ACS 5-Year Population Estimate: {closest_state}")

anim_save(
  filename = "data-viz/population-pyramid.gif"
)

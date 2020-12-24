library(tidyverse)
library(aocodeR)

### Input
# Read input with aocodeR
# Melt rows into tibble with readr
# Pull value as numeric with dplyr
d01 <- aoc_get_input(day = 1, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>% melt_csv() %>% pull(value) %>% as.numeric()

### Part 1
# Deduped combination expansion of d01
# Then filter for sum
crossing(x = d01, y = d01) %>%
  mutate(sum = x + y, prod = x * y) %>%
  filter(x > y, sum == 2020)

### Part 2
# This time 3 variables combined
crossing(x = d01, y = d01, z = d01) %>%
  mutate(sum = x + y + z, prod = x * y * z) %>%
  filter(x > y, y > z, sum == 2020)

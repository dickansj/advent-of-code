library(tidyverse)
library(aocodeR)

### Input

d06 <- aoc_get_input(day = 6, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>%
  melt_csv() %>%
  mutate(group = cumsum(is.na(value)) + 1) %>%
  drop_na() %>%
  add_count(group, name ="grsize") %>%
  separate_rows(value, sep = "") %>%
  filter(value != "")

### Part 1

# To clean the data:
d06_unique  <- d06 %>%
  group_by(group) %>%
  distinct(value, .keep_all = TRUE) # Within groups, eliminate rows with nonunique "value", keep all variables

nrow(d06_unique)

# Or since count() counts unique values, just use:
d06 %>% count(group, value) %>% nrow()

### Part 2
d06 %>% 
  count(group, grsize, value) %>%
  filter(n == grsize) %>%
  count()

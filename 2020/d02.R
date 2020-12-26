library(tidyverse)
library(aocodeR)

### Input
d02 <- aoc_get_input(day = 2, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>%
  read_table("pwd_input") %>% # Read whitespaced colums into tibble
  extract(pwd_input, c("low", "high", "char", "pwd"), "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)") %>% # Regex extract into columns
  mutate(across(low:high, as.integer)) # Since they're extracted as chars

### Part 1
# Compare char pattern count to valid range
validCheckA <- function(l, h, c, p) {
  str_count(p, c) %>% between(l, h)
}

# Pass all rows to validCheck with parallel map (purrr) and tally
d02 %>%
  mutate(valid = pmap_lgl(list(low, high, char, pwd), validCheckA)) %>%
  tally(valid)

### Part 2
# Compare substrings to required char and check for exactly 1 valid position
validCheckB <- function(l, h, c, p) {
  checkpos1 <- str_sub(p, l, l) == c
  checkpos2 <- str_sub(p, h, h) == c
  xor(checkpos1, checkpos2)
}

# parallel map with new fn
d02 %>%
  mutate(valid = pmap_lgl(list(low, high, char, pwd), validCheckB)) %>%
  tally(valid)
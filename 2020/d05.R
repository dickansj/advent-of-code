library(tidyverse)
library(aocodeR)
library(magrittr)

### Input
# Convert to binary, then parse row, column, seat
d05 <- aoc_get_input(day = 5, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>%
  read_table("board_passes") %>%
  mutate(binary = board_passes %>% str_replace_all(c("F" = "0", "B" = "1", "L" = "0", "R" = "1"))) %>%
  separate(binary, c("rowbin", "colbin"), 7) %>%
  mutate(
    row = strtoi(rowbin, 2),
    col = strtoi(colbin, 2),
    seat = row * 8 + col
    )

### Part 1
# Get seat max
# or use `summary(d05)` for number
# or `max(d05$seat)`
d05 %>% slice_max(seat)

### Part 2
# My seat is the only one missing, so find max delta betweeen a seat and its lag
# subtract() in pipe chain requires magrittr
d05 %>% arrange(seat) %>% slice_max(seat - lag(seat)) %>%
  pull(seat) %>% subtract(1)

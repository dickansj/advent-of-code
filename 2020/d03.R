library(tidyverse)
library(aocodeR)

### Input
d03 <- aoc_get_input(day = 3, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>%
  read_table("tree_layout")

### Tree Tally Function
# Pass each row of layout and delta x/y
# Get position pos relative to pattern start (width 31)
# Check/tally substring at pos, but disregard if non-integer coord

tallyTrees <- function(tl, x, y) {
  tl %>%
    mutate(
      pos = (x/y * (row_number() - 1)) %% 31 + 1,
      tree = ifelse(pos %% 1 == 0, str_sub(tree_layout, pos, pos), ".")
    ) %>%
    tally(tree == "#")
}

### Part 1
d03 %>% tallyTrees(3,1)

### Part 2
# Pass tibble of delta x/y's and tibble of layout to pmap_int of tallyTrees
tribble(
  ~x, ~y,
  #--|--
   1, 1,
   3, 1,
   5, 1,
   7, 1,
   1, 2
) %>%
  expand_grid(tl = list(d03)) %>%
  mutate(trees_hit = pmap_int(list(tl, x, y), compose(unlist, tallyTrees))) %>% # need to unlist to get int before tallyTrees
  pull(trees_hit) %>%
  prod()
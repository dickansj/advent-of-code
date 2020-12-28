library(tidyverse)
library(aocodeR)

### Input

# Parse passports into key/value pairs
d04 <- aoc_get_input(day = 4, year = 2020, cookie_path = "~/Projects/aoc_session_cookie.txt") %>%
  melt_csv() %>% # Melted gives NA value row for empty row in between passports
  separate_rows(value, sep = " ") %>% # One key-val pair per line
  mutate(pcount = cumsum(is.na(value)) +1) %>%
  drop_na() %>%
  separate(value, into = c("key", "val"), sep = ":") %>%
  select(key:pcount) %>%
  pivot_wider(names_from = key, values_from = val) # Pivot from narrow to wide, easier assignment

### Part 1

# Eliminate invalid passports
passlist1 <- d04 %>%
  select(-cid) %>% # Since cid is optional
  drop_na() %>%
  mutate(validA = TRUE)

passlist1 %>% count(validA)

### Part 2

# Add validation rules to passlist1
passlist2 <- passlist1 %>%
  mutate(
    # byr (Birth Year) - four digits; at least 1920 and at most 2002.
    byr_ok = between(as.integer(byr), 1920, 2002),
    # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    iyr_ok = between(as.integer(iyr), 2010, 2020),
    # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    eyr_ok = between(as.integer(eyr), 2020, 2030),
    # hgt (Height) - a number followed by either cm or in:
    # If cm, the number must be at least 150 and at most 193.
    # If in, the number must be at least 59 and at most 76.
    # case_when helpfully vectorizes multiple if_else statements rather than nesting
    # parse_number drops non-num char before/after 1st num (ex. units) and grouping mark (ex. , in 1,000)
    hgt_ok = case_when(
      str_detect(hgt, "cm") & between(parse_number(hgt), 150, 193) ~ TRUE,
      str_detect(hgt, "in") & between(parse_number(hgt), 59, 76) ~ TRUE,
      TRUE ~ FALSE # Gives catchall value for all unmatched
    ),
    # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    hcl_ok = str_detect(hcl, "^#[0-9a-f]{6}"),
    # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ecl_ok = str_detect(ecl, "^amb|blu|brn|gry|grn|hzl|oth$"),
    # pid (Passport ID) - a nine-digit number, including leading zeroes.
    pid_ok = str_detect(pid, "^[0-9]{9}$"),
    # New valid variable
    validB = byr_ok & iyr_ok & eyr_ok & hgt_ok & hcl_ok & ecl_ok & pid_ok
  )

passlist2 %>% count(validB)

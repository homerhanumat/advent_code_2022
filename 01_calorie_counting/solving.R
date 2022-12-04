library(tidyverse)

## Part One ----
## elf with most calories

df <-data.frame(
    calories = readLines(
      con = "input.txt"
    ) %>% 
      parse_number()
  ) %>% 
  mutate(is_dividing_line = ifelse(
    is.na(calories),
    1, 0
  )) %>% 
  mutate(elf = cumsum(is_dividing_line) + 1) %>% 
  drop_na() %>% 
  group_by(elf) %>% 
  summarize(total_calories = sum(calories))

answer1 <-
  df %>% 
  slice_max(n = 1, order_by = total_calories) %>% 
  pull(total_calories)

answer1

## Part Two ----
## top three elves

answer2 <-
  df %>% 
  slice_max(n = 3, order_by = total_calories) %>% 
  pull(total_calories) %>% 
  sum()

answer2

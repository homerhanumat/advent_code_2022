library(tidyverse)
## data from https://adventofcode.com/2022/day/1/input
## placed in file "data/day_01.txt")

## Part One ----
## elf with most calories

df <-data.frame(
    row = 1:length(info),
    calories = readLines(
      con = "data/day_01.txt"
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

answer_1 <-
  df %>% 
  slice_max(n = 1, order_by = total_calories) %>% 
  pull(total_calories)

answer_1

## Part Two ----
## top three elves

answer_2 <-
  df %>% 
  slice_max(n = 3, order_by = total_calories) %>% 
  pull(total_calories) %>% 
  sum()

answer_2

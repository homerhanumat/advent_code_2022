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

## Thoughts ----

## Nothing wrong with a loopy solution:

calories <- readLines(
  con = "input.txt"
) |>
  as.numeric()


rucksack_sums <- numeric()
elf <- 1
item <- 1
n <- length(calories)

while (item <= n) {
  sum <- 0
  while (!is.na(calories[item]) & item <= n) {
    sum <- sum + calories[item]
    item <- item + 1
  }
  rucksack_sums[elf] <- sum
  ## move past the NA:
  item <- item + 1
  elf <- elf + 1
}

sorted_sacks <- sort(rucksack_sums, decreasing = TRUE)

answer_1 <- sorted_sacks[1]
answer_2 <- sorted_sacks[1:3] |> sum()

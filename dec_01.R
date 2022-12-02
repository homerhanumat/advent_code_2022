token <-
  paste0("53616c7465645f5fd1593d974503b6cd0a0ddeb",
         "14ad5462f66a9fe354dbe9181c13d5fdd2fd4df3","
         45dbd05c2cfa6c112c92c256425ee12e8ca9b2d28437e2aa2"
  )

Sys.setenv(
  AOC_SESSION = token
)



library(tidyverse)
library(aor)

aor::day_start("2022-12-01", "aoc2022/")



## Part One ----
## elf with most calories

df <-data.frame(
    calories = readLines(
      con = "aoc2022/01_calorie_counting/input.txt"
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

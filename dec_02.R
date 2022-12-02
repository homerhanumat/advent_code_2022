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

aor::day_start("2022-12-02", "aoc2022/")



## Part One ----

input <- read_table("aoc2022/02_rock_paper_scissors/input.txt", 
                    col_names = FALSE)
names(input) <- c("other", "you")

score_table <- data.frame(
  plays = c(
    "AX",
    "AY",
    "AZ",
    "BX",
    "BY",
    "BZ",
    "CX",
    "CY",
    "CZ"
  ),
  outcome = c(
    3,
    6,
    0,
    0,
    3,
    6,
    6,
    0,
    3
  )
)

key <- score_table$outcome
names(key) <- score_table$plays

answer1 <-
  input %>% 
  mutate(together = str_c(other, you, sep = "")) %>% 
  mutate(shape_score = recode(you, X = 1, Y = 2, Z = 3)) %>% 
  mutate(outcome_score = recode(together, !!!key)) %>% 
  mutate(score = shape_score + outcome_score) %>% 
  summarize(total = sum(score)) %>% 
  pull(total)

answer1

## Part 2 ----

aor::day_continue("2022-12-02", "aoc2022/02_rock_paper_scissors/puzzle.R")

decryption <- c(
  AX = "Z",
  AY = "X",
  AZ = "Y",
  BX = "X",
  BY = "Y",
  BZ = "Z",
  CX = "Y",
  CY = "Z",
  CZ = "X"
)

answer2 <-
  input %>% 
  mutate(outcome_score = recode(you, X =  0, Y = 3, Z = 6)) %>% 
  mutate(together = str_c(other, you, sep = "")) %>% 
  mutate(your_shape = recode(together, !!!decryption)) %>% 
  mutate(shape_score = recode(your_shape, X = 1, Y = 2, Z = 3)) %>% 
  mutate(score = shape_score + outcome_score) %>% 
  summarize(total = sum(score)) %>% 
  pull(total)




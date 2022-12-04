library(tidyverse)

## Part One ----

input <- read_table(
  "input.txt", 
  col_names = FALSE
  )
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




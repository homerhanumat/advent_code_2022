library(tidyverse)

## Part One ----

input <- read_table(
  "input.txt", 
  col_names = FALSE
  )
names(input) <- c("opponent", "you")

outcomes <-
  c(3, 6, 0, 0, 3, 6, 6, 0, 3)
names(outcomes) <- c(
  "AX", "AY", "AZ", "BX", "BY", "BZ", "CX", "CY", "CZ"
)

input_1 <-
  input %>% 
  mutate(together = str_c(opponent, you, sep = "")) %>% 
  mutate(outcome_score = outcomes[together]) %>% 
  mutate(shape_score = recode(you, X = 1, Y = 2, Z = 3)) %>% 
  mutate(score = outcome_score + shape_score)

answer_1 <-
  input_1 %>% 
  pull(score) %>% 
  sum()

## Part 2 ----

your_play_decrypted <- c(
  AX = "scissors",
  AY = "rock",
  AZ = "paper",
  BX = "rock",
  BY = "paper",
  BZ = "scissors",
  CX = "paper",
  CY = "scissors",
  CZ = "rock"
)

input_2 <-
  input %>% 
  mutate(outcome_score = recode(you, X =  0, Y = 3, Z = 6)) %>% 
  mutate(together = str_c(opponent, you, sep = "")) %>% 
  mutate(your_object = your_play_decrypted[together]) %>% 
  mutate(shape_score = recode(
    your_object, "rock" = 1, "paper" = 2, "scissors" = 3)) %>% 
  mutate(score = shape_score + outcome_score)

answer_2 <-
  input_2 %>% 
  pull(score) %>% 
  sum()




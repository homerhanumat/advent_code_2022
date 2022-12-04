library(tidyverse)

## Part One ----

input <- read_table(
  "input.txt", 
  col_names = FALSE
  )
names(input) <- c("opponent", "you")

rules <- matrix(
  c(
    3, 6, 0,
    0, 3, 6,
    6, 0, 3
  ),
  nrow = 3,
  byrow = TRUE
)
objects <- c("rock", "paper", "scissors")
rownames(rules) <- objects
colnames(rules) <- objects

input_1 <-
  input %>% 
  mutate(opponent = recode(
    opponent, A = "rock", B = "paper", C = "scissors")
  )%>% 
  mutate( you = recode(
    you, X = "rock", Y = "paper", Z = "scissors")
  ) %>% 
  mutate(outcome_score = pmap_dbl(
    list(opponent, you),
    function(x, y) rules[x, y]
  )) %>% 
  mutate(shape_score = recode(
    you,
    "rock" = 1, "paper" = 2, "scissors" = 3
    )
  ) %>% 
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
  mutate(your_object = recode(together,
                              AX = "scissors",
                              AY = "rock",
                              AZ = "paper",
                              BX = "rock",
                              BY = "paper",
                              BZ = "scissors",
                              CX = "paper",
                              CY = "scissors",
                              CZ = "rock"
                              )) %>% 
  mutate(shape_score = recode(
    your_object, "rock" = 1, "paper" = 2, "scissors" = 3)) %>% 
  mutate(score = shape_score + outcome_score)

answer_2 <-
  input_2 %>% 
  pull(score) %>% 
  sum()




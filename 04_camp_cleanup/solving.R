library(tidyverse)



## Part One ----

input <- read_csv("input.txt", col_names = FALSE)
names(input) <- c("A","B")
i2 <- input %>% 
  extract(A, into = c("A_min", "A_max"), regex = "(\\d+)-(\\d+)") %>% 
  extract(B, into = c("B_min", "B_max"), regex = "(\\d+)-(\\d+)") %>% 
  mutate(A_min = parse_number(A_min)) %>% 
  mutate(B_min = parse_number(B_min)) %>% 
  mutate(A_max = parse_number(A_max)) %>% 
  mutate(B_max = parse_number(B_max)) %>% 
  mutate(AinB = A_min >= B_min & A_max <= B_max) %>% 
  mutate(BinA = A_min <= B_min & A_max >= B_max) %>% 
  mutate(contain = AinB | BinA) %>% 
  mutate(
    overlap = 
      (A_max >= B_min & A_max <= B_max) | 
      (B_max >= A_min & B_max <= A_max)
  )

answer1 <-
  i2 %>% 
  pull(contain) %>% 
  sum()
  
## Part 2 ----

answer2 <-
  i2 %>% 
  pull(overlap) %>% 
  sum()


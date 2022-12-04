library(tidyverse)

## Part One ----

input <- readLines("input.txt")

priority <- 1:52
names(priority) <- c(letters, LETTERS)

get_priority <- function(str) {
  n <- str_length(str)
  chars <- str_split(str, pattern = "") %>% unlist()
  left <- chars[1:(n / 2)]
  right <- chars[(n / 2 + 1):n]
  repeated_char <- intersect(left, right)
  priority[repeated_char]
}

priorities <- 
  input %>% 
  map_dbl(get_priority)

answer1 <- 
  priorities %>% 
  sum()

## Part Two ----

priority <- 1:52
names(priority) <- c(letters, LETTERS)
teams <- matrix(input, nrow = 3) %>% as.data.frame()

get_priority2 <- function(strs) {
  str_split(strs, pattern = "") %>% 
    reduce(intersect) %>% 
    priority[.]
}

answer2 <-
  teams %>% 
  map_dbl(get_priority2) %>% 
  sum()





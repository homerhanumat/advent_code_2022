library(tidyverse)

## Part One ----

input <- read_lines("input.txt")

marker_end <- function(input_str, len) {
  
  n <- input %>% str_length()
  start <- 1:(n-(len - 1))
  end <- start + len - 1
  strs <- 
    input %>% 
    str_sub(start = start, end = end)
  
  data.frame(
      start = start,
      end = end,
      string = strs
    ) %>% 
    mutate(chars_distinct = !str_detect(
      string, pattern = "(\\w).*(\\1)")
    ) %>% 
    filter(chars_distinct) %>% 
    slice_head(n = 1) %>% 
    pull(end)
}

answer_1 <-
  marker_end(input_str = input, len = 4)

## Part 2 ----

answer_2 <-
  marker_end(input_str = input, len = 14)



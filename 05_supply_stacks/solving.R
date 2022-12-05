library(tidyverse)

## Part One ----

input <- read_lines("input.txt")

divider <- which(input == "")
cols <-
  input[divider - 1] %>% 
  str_split(pattern = "\\s+") %>% 
  unlist() %>% 
  parse_number() %>% 
  .[!is.na(.)]

ncol <- length(cols)

## not sure how cartons would be arranged
## on top of a number with two or more digits.
## I have only 9 columns.

## letters are stacked on top of each digit
stack_indices <- seq(from = 2, by = 4, length.out = ncol)

arrangement <- vector(mode = "list", length = ncol)
for (col in 1:ncol) {
  level <- divider - 2
  current_stack <- character()
  stack_index <- stack_indices[col]
  while(level >= 1) {
    carton <-
      input[level] %>% 
      str_sub(start = stack_index, end = stack_index)
    print(carton)
    if (carton == " ") break
    current_stack <- 
      c(current_stack, carton)
    level <- level - 1
  }
  print(current_stack)
  arrangement[[col]] <- current_stack
}

move <- function() {
  direction <- divider + 1
  while(direction <= length(input)) {
    numbers <- 
      input[direction] %>% 
      str_match_all(pattern = "\\d+") %>% 
      unlist() %>% 
      parse_integer()
    how_many <- numbers[1]
    from <- numbers[2]
    to <- numbers[3]
    from_stack <- arrangement[[from]]
    arrangement[[to]] <<-
      c(
        arrangement[[to]],
        rev(from_stack)[1:how_many]
      )
    arrangement[[from]] <<- from_stack[1:(length(from_stack) - how_many)]
    direction <- direction + 1
  }
}


debug(move)

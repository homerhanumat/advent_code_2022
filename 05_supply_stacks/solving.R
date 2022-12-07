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

get_arrangement <- function() {
  arrangement <- vector(mode = "list", length = ncol)
  for (col in 1:ncol) {
    level <- divider - 2
    current_stack <- character()
    stack_index <- stack_indices[col]
    while(level >= 1) {
      carton <-
        input[level] %>% 
        str_sub(start = stack_index, end = stack_index)
      if (carton == " ") break
      current_stack <- 
        c(current_stack, carton)
      level <- level - 1
    }
    print(current_stack)
    arrangement[[col]] <- current_stack
  }
  arrangement
}

initial_arrangement <-
  get_arrangement()

move <- function(arrangement) {
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
    arrangement[[to]] <-
      c(
        arrangement[[to]],
        rev(from_stack)[1:how_many]
      )
    arrangement[[from]] <- from_stack[1:(length(from_stack) - how_many)]
    direction <- direction + 1
  }
  arrangement
}

final_arrangement <- move(initial_arrangement)

answer_1 <-
  final_arrangement %>% 
  map_chr(function(x) x[length(x)]) %>% 
  str_c(collapse = "")

answer_1

### Note:  Fully-Vectorized Stack-Parsing ----

## this is absurd, but one can get the initial
## restricting oneself to vectorized operations only,
## no native R loops or iteration, just some
## matrix-fu

input <- read_lines("input.txt")
divider <- which(input == "")

## read in the arrangement as a matrix of single characters:
stacks_matrix_cluttered <- 
  input[1:(divider - 1)] %>% 
  ## each element of the character vector above has the
  ## same number of characters, so we can split them out
  ## and construct a matrix:
  str_split(pattern = "") %>% 
  unlist() %>% 
  matrix(, ncol = str_length(input[divider - 1]), byrow = TRUE)


n <- nrow(stacks_matrix_cluttered)
## the bottom row has the digits:
digits_row <- stacks_matrix_cluttered[n, ]

initial_arrangement <- 
  ## remove the chuff in between (" ", "[", and "]"):
  stacks_matrix_cluttered[ , digits_row != " "] %>% 
  ## the carton-stacks are upside down, so reverse them:
  ## keeping the stack-numbers on the bottom:
  .[c((n - 1):1, n), ] %>% 
  ## back out to a single character vector:
  as.vector() %>% 
  ## then into a single string:
  str_c(collapse = "") %>% 
  ## get rid of the spaces that once filled out
  ## the tops of the stacks:
  str_replace_all(pattern = " ", replacement = "") %>% 
  ## get rid of the final stack-number:
  str_sub(start = 1, end = str_length(.) - 1) %>% 
  ## in the above string, stacks of cartons are
  ## separated by stack-numbers, so split by them:
  str_split(pattern = "\\d") %>% 
  unlist() %>% 
  ## we now have a character vector where each element
  ## is a string that represents the cartons in a
  ## single stack.  we now split each stack into
  ## a vector of cartons:
  str_split(pattern = "")

initial_arrangement

## Part 2 ----

move_2 <- function(arrangement) {
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
    len <- length(from_stack)
    arrangement[[to]] <-
      c(
        arrangement[[to]],
        from_stack[(len - how_many + 1):len]
      )
    arrangement[[from]] <- from_stack[1:(len - how_many)]
    direction <- direction + 1
  }
  arrangement
}

answer_2 <-
  move_2(initial_arrangement) %>% 
  map_chr(function(x) x[length(x)]) %>% 
  str_c(collapse = "")

answer_2

### Refactor to Make more "Functional" ----

## I am not yet satisfied with this, but ...

input <- read_lines("input.txt")

divider <- which(input == "")

input_arrangment <- input[1:(divider - 1)]
directions <- input[(divider + 1):length(input)]


parse_arrangement <- function(vec) {
  
  ## fiind column-numbers of arrangment:
  cols <-
    input[length(vec)] %>% 
    str_split(pattern = "\\s+") %>% 
    unlist() %>% 
    parse_number() %>% 
    .[!is.na(.)]
  ncol <- length(cols)
  
  ## unable to say how stacks would be placed
  ## over a column number with more than 2 digits.
  ## assuming one-digit collumn-numbers:
  stack_indices <- seq(from = 2, by = 4, length.out = ncol)
  
  ## arrangement will be a list of character vectors:
  arrangement <- vector(mode = "list", length = ncol)
  
  parse_stack <- function(col) {
    level <- length(vec) - 1
    stack <- character()
    stack_index <- stack_indices[col]
    while(level >= 1) {
      carton <-
        input[level] %>% 
        str_sub(start = stack_index, end = stack_index)
      if (carton == " ") break
      stack <- c(stack, carton)
      level <- level - 1
    }
    stack
  }
  
  arrangment <-
    cols %>% 
    map(parse_stack)
  
  arrangment
}

initial_arrangement <-
  parse_arrangement(input_arrangment)


move_cartons <- function(arr, direction, model) {
    numbers <-
      direction %>% 
      str_match_all(pattern = "\\d+") %>% 
      unlist() %>% 
      parse_integer()
    
    how_many <- numbers[1]
    from <- numbers[2]
    to <- numbers[3]
    
    from_stack <- arr[[from]]
    len <- length(from_stack)
    if (model == 9000) {
      arr[[to]] <-
        c(
          arr[[to]],
          rev(from_stack)[1:how_many]
        )
    } else if (model == 9001) {
      arr[[to]] <-
        c(
          arr[[to]],
          from_stack[(len - how_many + 1):len]
        )
    }
    
    arr[[from]] <- from_stack[1:(len - how_many)]
    arr
  }

rearrange <- function(arr, model) {
  for (direction in directions) {
    arr <- move_cartons(arr, direction, model)
  }
  arr
}

answer_1 <-
  rearrange(initial_arrangement, model = 9000) %>% 
  map_chr(function(x) x[length(x)]) %>% 
  str_c(collapse = "")

answer_2 <-
  rearrange(initial_arrangement, model = 9001) %>% 
  map_chr(function(x) x[length(x)]) %>% 
  str_c(collapse = "")



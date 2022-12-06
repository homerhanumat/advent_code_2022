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

### Refactor ---

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



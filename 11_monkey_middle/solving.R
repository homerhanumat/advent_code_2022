library(tidyverse)

## Part One ----

input <- readLines("input.txt")

blanks <- seq(from = 7, by = 7, 
              length.out = length(input) / 7)
start <- blanks - 6
end <- start + 5

## list of character vectors:
raw <-
  list(start, end) %>% 
  pmap(function(s, e) input[s:e])

## function to make one monkey
## from character vector:
parse_monkey <- function(vec) {
  
  id <- str_extract(vec[1], "\\d")
  
  items <- str_extract_all(vec[2], "\\d+") %>% 
    unlist() %>% parse_integer()
  
  op_maker <- function(text) {
    function(old) {
      str <- eval(parse(text = text))
      eval(parse(text = str))
    }
  }
  op <- op_maker(str_extract(vec[3], "(?<=new = ).*$"))
  
  t_maker <- function(v) {
    ns <- 
      str_extract(v, "\\d+") %>% 
      unlist()
    function(x) {
      ifelse(x %% parse_integer({{ns[1]}}) == 0, 
             {{ns[2]}}, {{ns[3]}})
    }
  }
  test <- t_maker(vec[4:6])
  
  monkey <- list(
    id = id,
    items = items,
    op = op,
    test = test,
    inspected = 0
  )
  monkey
}

## make the list of monkeys:
monkeys <- list()
for (i in 1:length(raw)) {
  monkeys[[as.character(i - 1)]] <-
    parse_monkey(raw[[i]])
}

process_item<- function(id, monkeys) {
  monkey <- monkeys[[id]]
  items <- monkey$items
  n <- length(items)
  item <- items[n]
  monkeys[[id]]$items <- items[-n]
  item <- floor(monkey$op(item) / 3)
  target <- monkey$test(item)
  monkeys[[target]]$items <-
    c(monkeys[[target]]$items, item)
  monkeys[[id]]$inspected <- monkey$inspected + 1
  monkeys
}

process_all <- function(id, monkeys) {
  for (i in seq_along(monkeys[[id]]$items)) {
    monkeys <- process_item(id, monkeys)
  }
  monkeys
}

do_round <- function(monkeys) {
  m <- length(monkeys)
  for (i in 0:(m - 1)) {
    id <- as.character(i)
    monkeys <- process_all(id, monkeys)
  }
  monkeys
}

new_monkeys <- monkeys
for (i in 1:20) {
  new_monkeys <- do_round(new_monkeys)
}

df <-
  new_monkeys %>% 
  map_dfr(function (m) {
    list(id = m$id, inspected = m$inspected)
  }) %>% 
  arrange(desc(inspected))

## answer 1:
df %>% slice_head(n = 2) %>% 
  pull(inspected) %>% prod()

## Part Two ----

## the divisors involved in the tests:
divisors <- 
  raw %>% 
  map_dbl(function(vec) {
    str_extract(vec[4], "\\d+") %>% 
      parse_number()
  })
## cool, divisors are distinct primes
N <- prod(divisors)

## control worry-size by 
## working modulo product of the divisors
process_item <- function(id, monkeys) {
  monkey <- monkeys[[id]]
  items <- monkey$items
  n <- length(items)
  item <- items[n]
  monkeys[[id]]$items <- items[-n]
  worry <- monkey$op(item)
  adjusted_worry <-worry %% N
  target <- monkey$test(adjusted_worry)
  monkeys[[target]]$items <-
    c(monkeys[[target]]$items, adjusted_worry)
  monkeys[[id]]$inspected <- monkey$inspected + 1
  monkeys
}

new_monkeys <- monkeys
for (i in 1:10000) {
  new_monkeys <- do_round(new_monkeys)
  if ( i %% 1000 == 0) {
    cat("On round ", i, "...\n")
    dft <- new_monkeys %>% 
      map_dfr(function (m) {
        list(id = m$id, inspected = m$inspected)
      })
    print(dft)
  }
}



df <-
  new_monkeys %>% 
  map_dfr(function (m) {
    list(id = m$id, inspected = m$inspected)
  }) %>% 
  arrange(desc(inspected))

## answer 2:
df %>% slice_head(n = 2) %>% 
  pull(inspected) %>% prod()

library(tidyverse)
library(jsonlite)

## Part One ----

input <- readLines("input.txt")
sep <- seq(from = 3, to = length(input) + 1, by = 3)
lc <- input[sep - 2]
rc <- input[sep - 1]

## Woohoo, json to the rescue:

left <- 
  lc %>% 
  map(parse_json)

right <- 
  rc %>% 
  map(parse_json)

## this was a slog:
correct_order <- function(l, r) {

  if (mode(l) == "numeric" & mode(r) == "numeric") {
    m <- min(length(l), length((r)))
    for (i in 1:m) {
      if (l[i] > r[i]) return(1)
      if (l[i] < r[i]) return(-1)
    }
    if (length(l) > length(r)) return(1)
    if (length(l) < length(r)) return(-1)
    return(0)
  }
  if (mode(l) == "list" & mode(r) == "list") {
    if (length(l) == 0 & length(r) > 0) return(-1)
    if (length(r) == 0 & length(l) > 0) return(1)
    if (length(r) == 0 & length(l) == 0) return(0)
    m <- min(length(l), length(r))
    for (i in 1:m) {
      result <- Recall(l[[i]], r[[i]])
      if (result != 0) return(result)
    }
    if (length(l) > length(r)) return(1)
    if (length(l) < length(r)) return(-1)
    return(0)
  }
  if (mode(l) == "numeric") l <- list(l)
  if (mode(r) == "numeric") r <- list(r)
  result <- Recall(l, r)
  return(result)
}

## answer_1:
list(left, right) %>% 
  pmap_dbl(correct_order) %>% 
  `%in%`(c(0,-1)) %>% 
  which() %>% 
  sum()


## Part Two ----

altogether <- 
  list(
    left,
    right,
    parse_json("[[2]]"),
    parse_json("[[6]]")) %>% 
  unlist(recursive = FALSE)

## haven't written a bubble sort since 10th grade:
sorted <- function(items) {
  n <- length(items)
  for (i in 1:(n - 1)){
    for (j in 1:(n - 1)) {
      x <- items[j]
      y <- items[j + 1]
      if (correct_order(x, y) == 1) {
        temp <- y
        y <- x
        x <- temp
        items[j] <- x
        items[j + 1] <- y
      }
    }
  }
  items
}

all_sorted <- sorted(altogether)

## answer 2: 
all_sorted %>% 
  map_lgl(function(x) {
    (correct_order(x, parse_json("[[2]]")) == 0)|
      (correct_order(x, parse_json("[[6]]")) == 0)
  }) %>% 
  which() %>% 
  prod()

library(tidyverse)

input <- readLines("input.txt") %>% 
  str_extract_all(pattern = "(\\d+)")

dr_line <- function(start, end) {
 data.frame(
   r = start[1]:end[1],
   c = rep(start[2], times = abs(end[1] - start[1]) + 1)
 )
}

dc_line <- function(start, end) {
  df <- dr_line(rev(end), rev(start))[, 2:1]
  df <- df[nrow(df):1, ]
  names(df) <- rev(names(df))
  df
}

rs <- function(path) {
  path <- parse_number(path)
  df <- data.frame(r = numeric(), c = numeric())
  for (i in seq(1, to = length(path) - 3, by = 2)) {
    cs <- path[i]
    rs <- path[i + 1]
    ce <- path[i + 2]
    re <- path[i + 3]
    start <- c(rs, cs)
    end <- c(re, ce)
    if (rs == re) {
      df <-
        bind_rows(df, dc_line(start, end))
    } else {
      df <-
        bind_rows(df, dr_line(start, end))
    }
  }
  distinct(df)
}

make_rock_spots <- function(lst) {
  lst %>% 
    map_dfr(rs)
}

## Part One ----

make_grid <- function(input) {
  spots <- make_rock_spots(input)
  grid <- matrix(".", nrow = max(spots$r) +1, 
                 ncol = max(spots$c) + 1)
  for (i in 1:nrow(spots)) {
    grid[spots$r[i], spots$c[i]] <- "#"
  }
  grid
}

spots <- make_rock_spots(input)
grid <- make_grid(input)

source <- c(0, 500)
max_rock_level <- 
  (rowSums(grid == "#") > 0) %>% 
  which() %>% 
  max()

## side effect:  modifies `grid`
move_sand <- function(sand_pos) {
  rs <- sand_pos[1]
  cs <- sand_pos[2]
  at_source <- (rs == source[1] && cs == source[2])
  if (grid[rs + 1, cs] == ".") {
    grid[rs + 1, cs] <<- "o"
    if (!at_source) grid[rs, cs] <<- "."
    new_pos <- c(rs + 1, cs)
  } else if (grid[rs + 1, cs - 1] == ".") {
    grid[rs + 1, cs - 1] <<- "o"
    if (!at_source) grid[rs, cs] <<- "."
    new_pos <- c(rs + 1, cs - 1)
  } else if (grid[rs + 1, cs + 1] == ".") {
    grid[rs + 1, cs + 1] <<- "o"
    if (!at_source) grid[rs, cs] <<- "."
    new_pos <- c(rs + 1, cs +1)
  } else {
    new_pos <- sand_pos
  }
  new_pos
}

watch_grain <- function() {
  current <- source
  repeat {
    new <- move_sand(current)
    if (all(new == current)) break
    current <- new
    if (new[1] == max_rock_level) break
  }
  current
}

grid <- make_grid(input)
count <- 0
repeat {
  new <- watch_grain()
  if (new[1] >= max_rock_level) break
  count <- count + 1
}

## answer_1
count

## Part Two ----

## expand the grid horizontally to the right, 
## add rock floor:
margin_right <- max_rock_level 
grid2 <- cbind(
  grid,
  matrix(".", nrow = nrow(grid), ncol = margin_right)
)
grid2 <-
  rbind(
    grid2,
    matrix("#", nrow = 1, ncol = margin_right + ncol(grid))
  )

source <- c(0, 500)
max_rock_level <- Inf
count <- 0
grid <- grid2
system.time(
  repeat {
    new <- watch_grain()
    if (identical(new, source)) break
    count <- count + 1
  }
)


## answer_2 (include grain stuck at source):
count + 1
## took 14.85 seconds on my 2020 Macbook Pro

library(tidyverse)

input <- readLines("input.txt")

## Part One ----

x <- 1
for (i in 1:length(input)) {
  comm <- input[i]
  if (str_detect(comm, "noop")) {
    x <- c(x, 0)
  } else {
    x <- c(x, 0, parse_number(comm))
  }
}
xs <- cumsum(x)
strength <- xs * 1:length(xs)
wanted <- seq(20, 220, by = 40)
answer_1 <- sum(strength[wanted])
answer_1

## Part 2 ----

xs <- xs[-1]

pos <- rep(1:40, 6)
lit <- abs(pos - xs) <= 1

scr <- ifelse(lit, "#", " ")
mscr <- matrix(scr, nrow = 6, byrow = TRUE)
View(mscr)

## RGZEHURK, but we are missing the vertical bar of the first R

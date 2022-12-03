# token <-
#   paste0("53616c7465645f5fd1593d974503b6cd0a0ddeb",
#          "14ad5462f66a9fe354dbe9181c13d5fdd2fd4df3","
#          45dbd05c2cfa6c112c92c256425ee12e8ca9b2d28437e2aa2"
#   )
# 
# Sys.setenv(
#   AOC_SESSION = token
# )



library(tidyverse)
library(aor)

# aor::day_start("2022-12-03", "aoc2022/")



## Part One ----

input <- readLines("aoc2022/03_rucksack_reorganization/input.txt")

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





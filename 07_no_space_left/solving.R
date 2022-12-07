library(tidyverse)

## Part One ----

input <- readLines("input.txt")

after_cd <-
  input %>% 
  str_subset(pattern = "\\$ cd ") %>% 
  str_extract(pattern = "(?<=cd ).*")

## the only cd moves are one up or one down

file_system <- list(
  list(
    type = "dir",
    pathname = "/",
    parent = NA,
    children = numeric(),
    size = 0,
    listed = FALSE
  )
)

parse_ls <- function(fs, dir_id, output) {
  dir <- fs[[dir_id]]
  if (dir$listed) {
    return(fs)
  }
  n <- length(fs)
  for (line in output) {
   reports_subdir <- str_detect(line, pattern = "^dir")
   if (reports_subdir) {
     subdirname <- str_extract(line, pattern = "(?<= ).+$")
     lst <- list(
       type = "dir",
       pathname = str_c(dir$pathname, subdirname, collapse = "/"),
       parent = dir_id,
       children = numeric(),
       size = 0,
       listed = FALSE
     )
   } else {
     size <- str_extract(line, pattern = "\\d+") %>% 
       parse_number()
     filename <- str_extract(line, pattern = "(?<= ).+$")
     lst <- list(
       type = "file",
       pathname = str_c(dir$pathname, filename, collapse = "/"),
       parent = dir_id,
       children = NA,
       size = size
     )
   }
   n <- n+1
   fs[[n]] <- lst
   fs[[dir_id]]$children <- 
     c(
       fs[[dir_id]]$children,
       n
     )
   fs[[dir_id]]$listed <- TRUE
  }
  fs
}

parse_ls(file_system, 1, input[3:6])

commands <- list()

for (line in input) {
  is_command <- str_detect(line, pattern = "$")
  if (is_command) {
    
  }
}

parse_command <- function(command) {
  
}


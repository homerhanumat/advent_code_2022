## Nearly wrote an interpreter here, must be a quicker way.

library(tidyverse)

## Part One ----

input <- readLines("input.txt")


after_cd <-
  input %>% 
  str_subset(pattern = "\\$ cd ") %>% 
  str_extract(pattern = "(?<=cd ).*")
## the only cd moves are one up or one down!


command_indices <- which(str_detect(input, pattern = "\\$"))
next_indicies <- c(command_indices[-1], length(input) + 1)
output_lens <- next_indicies - command_indices - 1

parse_command <- function(index,len) {
  command <- input[index]
  type <- ifelse(
    str_detect(command, patter = "\\$ cd"),
    "cd",
    "ls"
  )
  arg <- str_extract(command, pattern = "(?<=(cd|ls) ).*")
  if (len > 0) {
    outputs <- input[(index + 1):(index + len)]
  } else {
    outputs <- character()
  }
  list(
    type = type,
    arg = arg,
    outputs = outputs
  )
}

## a history of thhe commands (along with its output):
commands <- 
  list(command_indices, output_lens) %>% 
  pmap(parse_command)

## start off the file-system.
#3 It will eeventually be a list of all
#3 items foudn in our command lines explorations,
## with linkks to parent and chil items.
file_system <- list(
  list(
    type = "dir",
    name = 1,
    parent = NA,
    children = numeric(),
    size = 0,
    listed = FALSE
  )
)

## this function uses info from an ls command
## to extedn the file-system according to the output
## of ls:
parse_ls <- function(fs, dir_id, output) {
  if (length(output) == 0) return(fs)
  dir <- fs[[dir_id]]
  if (dir$listed) {
    return(fs)
  }
  for (line in output) {
   n <- length(fs)
   reports_subdir <- str_detect(line, pattern = "^dir")
   if (reports_subdir) {
     subdirname <- str_extract(line, pattern = "(?<= ).+$")
     lst <- list(
       type = "dir",
       name = subdirname,
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
       name = filename,
       parent = dir_id,
       children = NA,
       size = size
     )
   }
   new_id <- n+1
   fs[[new_id]] <- lst
   fs[[dir_id]]$children <- 
     c(
       fs[[dir_id]]$children,
       new_id
     )
   fs[[dir_id]]$listed <- TRUE
  }
  fs
}

## respond to a cd command:
update_wd <-function(fs, cwd, arg) {
  if (arg == "/") return(1)
  dir <- fs[[cwd]]
  if (arg == "..") {
    return(dir$parent)
  } else {
    child_ids <- dir$children
    child_count <- 1
    for (child in fs[child_ids]) {
      if (child$type == "dir" & child$name == arg) break
      child_count <- child_count + 1
    }
    return(child_ids[child_count])
  }
}

## buold the file-system:
wd <- 1
for (command in commands) {
  if (command$type == "cd") {
    wd <- update_wd(
      fs = file_system,
      cwd = wd,
      arg = command$arg
    )
  } else {
    file_system <-
      parse_ls(
        fs = file_system,
        dir_id = wd,
        output = command$output
      )
  }
}

## function to compute size of a directory:
size <- function(fs, item) {
  if (item$type == "file") return(item$size)
  if (item$type == "dir") {
    kids <- item$children
    sum <- 0
    for (index in kids) {
      kid <- fs[[index]]
      if (kid$type == "file") {
        sum <- sum + kid$size
      } else {
        sum <- sum + Recall(fs, kid)
      }
    }
    return(sum)
  }
}

## the directories in the fule-system:
dirs <- 
  file_system %>% 
  keep(.p = function(x) x$type == "dir")

## their sizes:
dir_sizes <-
  dirs %>% 
  map_dbl(size, fs = file_system)

answer_1 <- 
  dir_sizes[dir_sizes <= 10^5] %>% 
  sum()

answer_1


## Part 2 ----

unused_space <- 70000000 - size(
  fs = file_system,
  item = file_system[[1]]
  )

required_size <- 30000000 - unused_space

answer_2 <-
  dir_sizes %>% 
  .[. >= required_size] %>% 
  min()

answer_2


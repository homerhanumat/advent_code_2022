library(tidyverse)

input <- readLines("input.txt")

from_snafu <- function(s) {
  x <- str_split(s, "") %>% unlist()
  dec <- case_when(
    x == "2" ~ 2,
    x == "1" ~ 1,
    x == "0" ~ 0,
    x == "-" ~ -1,
    x == "=" ~ -2
  )
  sum(dec * 5^((length(x) - 1):0))
}


snafu_switch <- function(s) {
  x <- str_split(s, "") %>% unlist()
  y <- case_when(
    x == "2" ~ "=",
    x == "1" ~ "-",
    x == "0" ~ "0",
    x == "-" ~ "1",
    x == "=" ~ "2"
  )
  str_c(y, collapse = "")
}

to_snafu <- function(x) {
  if (x == -2) return("=")
  if (x == -1) return("-")
  if (x == 0) return("0")
  if (x == 1) return("1")
  if (x == 2) return("2")
  m <- floor(log(2 * x) / log(5) - 1)
  lower <- (5^(m + 1) - 1) / 2 + 1
  upper <- (5^(m + 1) - 1) / 2 + 2 * 5^(m + 1)
  len <- m + 2
  if (x >= lower & x < 5^(m + 1)) {
    end <- snafu_switch(Recall(5^(m + 1) - x))
    padding_len <- len - str_length(end) - 1
    zeroes <- ifelse(
      padding_len > 0,
      str_c(rep("0", times = padding_len)),
      ""
    )
    snafu <- str_c(
      "1", zeroes,
      end, collapse = ""
    )
    return(snafu)
  } else if (x >= 5^(m + 1) & x < 5^(m + 1) + lower) {
    end <- Recall(x - 5^(m + 1))
    padding_len <- len - str_length(end) - 1
    zeroes <- ifelse(
      padding_len > 0,
      str_c(rep("0", times = padding_len)),
      ""
    )
    snafu <- str_c(
      "1", zeroes,
      end, collapse = ""
    )
    return(snafu)
  } else if (x >= 5^(m + 1) + lower & x < 2 * 5^(m + 1)) {
    end <- snafu_switch(Recall(2 * 5^(m + 1) - x))
    padding_len <- len - str_length(end) - 1
    zeroes <- ifelse(
      padding_len > 0,
      str_c(rep("0", times = padding_len)),
      ""
    )
    snafu <- str_c(
      "2", zeroes,
      end, collapse = ""
    )
    return(snafu)
  } else{
    end <- Recall(x - 2 * 5^(m + 1))
    padding_len <- len - str_length(end) - 1
    zeroes <- ifelse(
      padding_len > 0,
      str_c(rep("0", times = padding_len)),
      ""
    )
    snafu <- str_c(
      "2", zeroes,
      end, collapse = ""
    )
    return(snafu)
  }
}

## answer_1:
input %>% 
  map_dbl(from_snafu) %>% 
  sum() %>% 
  to_snafu()

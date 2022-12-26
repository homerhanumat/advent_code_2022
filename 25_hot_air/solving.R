library(tidyverse)

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

to_snafu <- function(n) {
  if (n == 0) return("0")
  if (n == 1) return("1")
  if (n == 2) return("2")
  if (n == 3) return("1=")
  if (n == 4) return("1-")
  remainders <- 0:4
  digit <- c("0", "1", "2", "=", "-")
  carry <- c(0, 0, 0, 1, 1)
  rem <- n %% 5
  quot <- n %/% 5
  snafu <- str_c(
    Recall(quot + carry[remainders == rem]), 
    digit[remainders == rem], sep = "")
  return(snafu)
}

## answer_1:
readLines("input.txt") %>% 
  map_dbl(from_snafu) %>% 
  sum() %>% 
  to_snafu()





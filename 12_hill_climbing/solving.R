library(tidyverse)

input <- readLines("test.txt")
chars <- input %>% 
  str_split("") %>% 
  unlist() %>% 
  matrix(ncol = length(input)) %>% 
  t()


end_row <- which(rowSums(chars == "E") == 1)
end_col <- which(colSums(chars == "E") == 1)

codes <- c(1:26, 1, 26)
names(codes) <- c(letters, "S", "E")

height <- 
  input %>% 
  str_split("") %>% 
  unlist() %>% 
  recode(!!!codes) %>% 
  matrix(ncol = length(input)) %>%
  t()

## Part One ----


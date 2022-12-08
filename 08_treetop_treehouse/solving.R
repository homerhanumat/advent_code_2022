library(tidyverse)

## Part One ----

input <- readLines("test.txt")

mat <-
  input %>% 
  str_split(patter = "") %>% 
  unlist() %>% 
  parse_number() %>% 
  matrix(nrow = sqrt(length(.)), byrow = TRUE)

vis <- function(mat, i, j) {
  m <- nrow(mat)
  n <- ncol(mat)
  val <- mat[i, j]
  list(
    i = i,
    j = j,
    vis_top = ifelse(i == 1, TRUE, val > max(mat[1:(i - 1), j])),
    vis_bot = ifelse(i == m, TRUE, val > max(mat[(i + 1):m, j])),
    vis_left = ifelse(j == 1, TRUE, val > max(mat[i, 1:(j - 1)])),
    vis_right = ifelse(j == n, TRUE, val > max(mat[i, (j + 1):n]))
  )
}


df <- expand(
  data.frame(i = 1:nrow(mat), j = 1:ncol(mat)),
  i, j
) %>% 
  as.list() %>% 
  pmap_dfr(vis, mat = mat) %>% 
  mutate(vis = vis_top | vis_left | vis_bot | vis_right)

answer_1 <- 
  df %>% 
  pull(vis) %>% 
  sum()

## Part Two ----

score <- function(x, vec) {
  if (all(x > vec)) return(length(vec))
  which(x <= vec)[1]
}
score(5, c(5, 0))

view_score <- function(mat, i, j) {
  m <- nrow(mat)
  n <- ncol(mat)
  val <- mat[i, j]
  list(
    i = i,
    j = j,
    s_top = ifelse(i == 1, 0, score(val, rev(mat[1:(i - 1), j])),
    s_bot = ifelse(i == m, 0, score(val, mat[(i + 1):m, j])),
    s_left = ifelse(j == 1, 0, score(val, mat[i, 1:(j - 1)])),
    s_right = ifelse(j == n, 0, score(val, rev(mat[i, (j + 1):n])))
  )
}

df <- expand(
  data.frame(i = 1:nrow(mat), j = 1:ncol(mat)),
  i, j
) %>% 
  as.list() %>% 
  pmap_dfr(view_score, mat = mat) %>% 
  mutate(view_score = s_top * s_bot * s_left * s_right)

answer_2 <-
  df %>% 
  pull(view_score) %>% 
  max()

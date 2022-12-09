library(tidyverse)

## Part One ----

df0 <- read_table("input.txt", col_names = FALSE)
names(df0) <- c("dir", "amount")

df1 <-
  df0 %>% 
  mutate(ay = case_when(
    dir %in% c("L", "R") ~ 0,
    dir == "U" ~ amount,
    dir == "D" ~ -amount
  )) %>% 
  mutate(ax = case_when(
    dir %in% c("U", "D") ~ 0,
    dir == "R" ~ amount,
    dir == "L" ~ -amount
  ))

make_steps <- function(ax, ay) {
  if (ax == 0) {
    nrows <- abs(ay)
    dx <- rep(0, nrows)
    dy <- rep(sign(ay), nrows)
  } else {
    nrows <- abs(ax)
    dy <- rep(0, nrows)
    dx <- rep(sign(ax), nrows)
  }
  list(dx = dx, dy = dy)
}

df2 <- 
  df1 %>% 
  select(ax, ay) %>% 
  pmap_dfr(make_steps) %>% 
  mutate(
    xh = cumsum(dx),
    yh = cumsum(dy)
  )

hxs <- hys <- txs <- tys <- numeric(nrow(df2) + 1)
for (i in 1:nrow(df2)) {
  hxs[i + 1] <- df2$xh
  hys[i + 1] <- df2$yh
  
}







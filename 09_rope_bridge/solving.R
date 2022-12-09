library(tidyverse)

## Part One ----

df0 <- read_table("input.txt", col_names = FALSE)
names(df0) <- c("dir", "amount")

## history of moves, expressed as changes in coordinates:
df1 <-
  df0 %>% 
  mutate(ax = case_when(
    dir %in% c("L", "R") ~ 0,
    dir == "U" ~ amount,
    dir == "D" ~ -amount
  )) %>% 
  mutate(ay = case_when(
    dir %in% c("U", "D") ~ 0,
    dir == "R" ~ amount,
    dir == "L" ~ -amount
  ))

## convert a single move into a sequence of one-unit steps:
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

## history of positions occupied by the head:
head <- 
  df1 %>% 
  select(ax, ay) %>% 
  pmap_dfr(make_steps) %>% 
  mutate(
    x = cumsum(dx),
    y = cumsum(dy)
  ) %>% 
  rbind(
    data.frame(dx = NA, dy = NA, x = 0, y = 0),
    .
  )

## given new head position, and current tail position,
## find new tail position:
tail_move <- function(head, tail) {
  diff <- head - tail
  manhattan <- sum(abs(diff))
  if (manhattan <= 1) return(round(tail))
  diag <- all(diff != 0)
  if (!diag) {
    same_dim <- which(diff != 0)
    tail[same_dim] <- mean(c(tail[same_dim], head[same_dim]))
    return(round(tail))
  }
  dim_max_diff <- which(abs(diff) > 1)
  other_dim <- 3 - dim_max_diff
  tail[other_dim] <- head[other_dim]
  tail[dim_max_diff] <- mean(c(tail[dim_max_diff], head[dim_max_diff]))
  round(tail)
}


## history of positions occupied by the tail:
n <- nrow(head)
tx <- ty <- numeric(n)
for (i in 2:n) {
  new_tail <- tail_move(
    head = c(head$x[i], head$y[i]),
    tail = c(tx[i - 1], ty[i - 1])
  )
  tx[i] <- new_tail[1]
  ty[i] <- new_tail[2]
}

tail <- data.frame(x = tx, y = ty)

answer_1 <-
  tail %>% 
  distinct() %>% 
  nrow()


## Part Two ----

## when head is tail wrt another item
## the head can move diagonally to follow that item so
## manhattan distance from head to its own tail 
## can be as much as 4, the two being lying along a diagonal.
## Hence must generalize tail_move function:
tail_move_gen <- function(head, tail) {
  diff <- head - tail
  manhattan <- sum(abs(diff))
  if (manhattan <= 1) return(round(tail))
  if (manhattan == 4) {
    ## tail moves diagonally toward head:
    return(
      round(
        tail + 0.5 * diff
      )
    )
  }
  diag <- all(diff != 0)
  if (!diag) {
    same_dim <- which(diff != 0)
    tail[same_dim] <- mean(c(tail[same_dim], head[same_dim]))
    return(round(tail))
  }
  dim_max_diff <- which(abs(diff) > 1)
  other_dim <- 3 - dim_max_diff
  tail[other_dim] <- head[other_dim]
  tail[dim_max_diff] <- mean(c(tail[dim_max_diff], head[dim_max_diff]))
  round(tail)
}

## functionalize the previous work to generate tail-history:
tail_positions <- function(prev) {
  n <- nrow(prev)
  tx <- ty <- numeric(n)
  for (i in 2:n) {
    new_tail <- tail_move_gen(
      head = c(prev$x[i], prev$y[i]),
      tail = c(tx[i - 1], ty[i - 1])
    )
    tx[i] <- new_tail[1]
    ty[i] <- new_tail[2]
  }
  data.frame(x = tx, y = ty)
}

## compute history of each successive tail:
current <- head
for (i in 2:10) {
  print(i)
  current <- tail_positions(current)
}

answer_2 <-
  current %>% 
  distinct() %>% 
  nrow()

## Had trouble at first, incorrectly assuming start was at (1, 1).

library(tidyverse)

input <- readLines("input.txt")
chars <- input %>% 
  str_split("") %>% 
  unlist() %>% 
  matrix(ncol = length(input)) %>% 
  t()

start_row <- which(rowSums(chars == "S") == 1)
start_col <-  which(colSums(chars == "S") == 1)

end_row <- which(rowSums(chars == "E") == 1)
end_col <- which(colSums(chars == "E") == 1)

codes <- c(1:26, 1, 26)
names(codes) <- c(letters, "S", "E")

height <- 
  input %>% 
  str_split("") %>% 
  unlist() %>% 
  recode(!!!codes)

## Part One ----

nc <- ncol(chars)
nr <- nrow(chars)

get_neighbors <- function(row_number) {
  info <- df %>% slice(row_number)
  r <- info$x
  c <- info$y
  h <- info$h
  ng <- expand.grid(x = r + c(-1, 1), y = c) %>% 
    bind_rows(expand.grid(x = r, y = c + c(-1, 1)))
  rows <- numeric()
  for (i in 1:4) {
    x <- ng$x[i]
    y <- ng$y[i]
    if (x %in% c(0, nrow(chars) + 1)) next
    if (y %in% c(0, ncol(chars) + 1)) next
    row <- which(df$x == x & df$y == y)
    height <- df[row, ]$height
    if (height - h > 1) next
    rows <- c(rows, row)
  }
  rows
}

df <-
  expand.grid(y = 1:nc, x = 1:nr) %>% 
  mutate(height = height) %>% 
  mutate(d = rep(Inf, nr * nc))

row_begin <- which(df$x == start_row & df$y == start_col)

df[row_begin, "d"] <- 0

neighbors <-
  1:nrow(df) %>% 
  map(get_neighbors)

## learned about Dyjkstra:

sptSet <- logical(length = nrow(df))

row_dest <- which(df$x == end_row & df$y == end_col)

while (!sptSet[row_dest]) {
  verts <- which(!sptSet)
  min_dist <- min(df[verts, "d"])
  u <- verts[df[verts, "d"] == min_dist][1]
  sptSet[u] <- TRUE
  for (pt in neighbors[[u]]) {
    if (df[pt, "d"] > 1 + df[u, "d"]) {
      df[pt, "d"] <- 1 + df[u, "d"]
    }
  }
}

## answer_1:
df[row_dest, "d"]



############################## -----

## source:
## https://github.com/AdroMine/AdventOfCode/blob/main/2022/Day12/d12_solution.R
## Helped me catch my error. worth further study as it makes better use of
## matrices.


# file <- 'sample.txt'
file <- 'input.txt'
width <- nchar(readLines(file, n = 1))
input <- as.matrix(read.fwf(file, widths = rep(1, width)))


dijkstra <- function(graph, start = 'S', goal = 'E', part2 = FALSE){
  
  N <- nrow(graph)
  C <- ncol(graph)
  goal <- which(graph == goal, arr.ind = TRUE)
  start <- which(graph == start, arr.ind = TRUE)
  
  if(!part2){
    graph[start] <- 'a'
    graph[goal] <- 'z'
  } else {
    graph[start] <- 'z'
  }
  
  openset <- collections::priority_queue()
  openset$push(start, priority = 0)
  
  # score
  mindist <- matrix(Inf, N, C)
  mindist[start[1], start[2]] <- 0
  
  neighbours <- function(xy){
    x <- xy[1]
    y <- xy[2]
    cur <- utf8ToInt(graph[x, y])
    nbrs <- list(
      c(x , y - 1),  # left
      c(x , y + 1),  # right
      c(x + 1, y ),  # down
      c(x - 1, y)    # up
    )
    possible <- sapply(nbrs, function(cord){
      nx <- cord[1]
      ny <- cord[2]
      if(nx > N || ny > C || nx < 1 || ny < 1){
        FALSE
      } else {
        pt <- utf8ToInt(graph[nx, ny])
        if(!part2) (pt - cur) <= 1 else (pt - cur) >= -1
      }
    })
    nbrs[possible]
  }   
  
  while(openset$size() > 0){
    
    curr <- openset$pop()
    cx <- curr[1]
    cy <- curr[2]
    d <- mindist[cx,cy]
    
    if(part2){
      if(graph[cx, cy] == 'a') return(d)
    } else if(all(curr == goal)) break
    
    adjacent <- neighbours(curr)
    
    for(nbr in adjacent){
      nx <- nbr[1]
      ny <- nbr[2]
      
      alt <- d + 1 #graph[nx,ny]
      
      if(alt < mindist[nx,ny]){
        mindist[nx,ny] <- alt
        openset$push(nbr, priority = -alt)
      }
    }      
  }
  
  mindist[goal[1], goal[2]]
}

# Part 1
dijkstra(input)

# Part 2 
# find shortest path from E to any 'a'
dijkstra(input, 'E', 'a', part2 = TRUE)



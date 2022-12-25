library(tidyverse)

input <- readLines("input.txt") %>% 
  str_extract_all(pattern = "-?\\d+") %>% 
  map(parse_number)

sensors <- 
  input %>% 
  map(function(l) {
    c(x = l[1], y = l[2])
  })

radii <-
  input %>% 
  map_dbl(function(l) {
    abs(l[1] - l[3]) + abs(l[2]-l[4])
  })

beacons <-
  input %>% 
  map(function(l) {
    c(x = l[3], y = l[4])
  })

sbmat <- input %>% 
  map_dfr(function(l) {
    data.frame(
      sx = l[1], sy = l[2],
      bx = l[3], by = l[4]
    )
  })

xmin <- min(sbmat$sx, sbmat$bx)
xmax <- max(sbmat$sx, sbmat$bx)
ymin <- min(sbmat$sy, sbmat$by)
ymax <- max(sbmat$sy, sbmat$by)

find_x <- function(sensor_id, row) {
  sensor <- sensors[[sensor_id]]
  sx <- sensor[1]
  sy <- sensor[2]
  margin <- radii[i] - abs(row - sy)
  if (margin < 0) return(numeric())
  return((sx - margin):(sx + margin))
}

row <- 2000000
lst <- vector(mode = "list", length = length(sensors))
for (i in 1:length(sensors)) {
  lst[[i]] <- find_x(i, row)
}

bx_row <- sbmat$bx[sbmat$by == row] %>% unique()

## answer_1
lst %>% 
  reduce(union) %>% 
  setdiff(bx_row) %>% 
  length()

end <- 4000000

find_x_2 <- function(sensor_id, row) {
  sensor <- sensors[[sensor_id]]
  sx <- sensor[1]
  sy <- sensor[2]
  margin <- radii[sensor_id] - abs(row - sy)
  if (margin < 0) return(numeric())
  left <- max(sx - margin, 0)
  right <- min(sx + margin, end)
  return(c(left,right))
}

find_x_ints <- function(row) {
  df <- data.frame(s = numeric(), e = numeric())
  for (i in 1:length(sensors)) {
    xs <- find_x_2(i, row)
    if (length(xs) == 0) next
    if (xs[2] < 0) next
    if (xs[1] > end) next
    df <- bind_rows(
      df, 
      data.frame(
        s = max(0, xs[1]), 
        e = min(xs[2], end)))
  }
  df
}

have_gap <- function(a, b) {
  (a[2] < b[1] -1) | (a[1] > b[2] + 1)
}

first_gap <- function(intervals) {
  df <- arrange(intervals, s, e)
  prev <- df[1, ]
  i <- 1
  while (i < nrow(df)) {
    new <- df[i + 1, ]
    if (have_gap(
      c(prev$s, prev$e),
      c(new$s, new$e)
      )
    ) {
      return(prev$e + 1)
    } else {
      prev <-
        data.frame(
          s = min(prev$s, new$s),
          e = max(prev$e, new$e)
        )
    }
    i <- i + 1
  }
  return(NULL)
}

ds <- function() {
  row <- 0
  while (row <= end) {
    if (row %% 10000 == 0) cat(row, "\n")
    df <- find_x_ints(row)
    mins <- min(df$s)
    if (mins > 0) return(list(x = mins - 1, row = row))
    maxe <- max(df$e)
    if (maxe < end) return(list(x = maxe + 1, row = row))
    x <- first_gap(df)
    if (!is.null(x)) return(list(x = x, row = row))
    row <- row + 1
  }
}

place <- ds()
## good through 1750000

## answer_2
(place$x * 4000000) + place$row

find_x_3 <- function(sensor_id, row) {
  sensor <- sensors[[sensor_id]]
  sx <- sensor[1]
  sy <- sensor[2]
  margin <- radii[sensor_id] - abs(row - sy)
  if (margin < 0) {
    return(list(
      s = NA,
      e = NA,
      dy = sy - row
    ))
  }
  left <- sx - margin
  right <- sx + margin
  return(list(
    s = left,
    e = right,
    dy = sy - row
    )
  )
}

ints <- function(row) {
  1:length(sensors) %>% 
    map_dfr(find_x_3, row = row)
}

print(ints(0), n = 24)

initialize <- function(row) {
  
}
get_floor <- function(directions) {
  # another way would be to user stringr::str_count and stringr::str_length
  directions <- strsplit(directions, "")[[1]]
  floor <- 0
  for (direction in directions) {
    if (direction == "(") {
      floor <- floor + 1
    } else {
      floor <- floor - 1
    }
  }
  return(floor)
}

get_basement_position <- function(directions) {
  directions <- strsplit(directions, "")[[1]]
  floor <- 0
  for (i in seq_along(directions)) {
    direction <- directions[i]
    if (direction == "(") {
      floor <- floor + 1
    } else {
      floor <- floor - 1
    }
    if (floor == -1) break
  }
  return(i)
}

day01.run_part1 <- function(filepath) {
  input <- file(filepath, open="r")
  directions <- readLines(input, n=1)
  floor <- get_floor(directions)
  return(floor)
}

day01.run_part2 <- function(filepath) {
  input <- file(filepath, open="r")
  directions <- readLines(input, n=1)
  pos <- get_basement_position(directions)
  return(pos)
}

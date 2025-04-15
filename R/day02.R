split_dimensions <- function(dimensions) {
  dims <- strsplit(dimensions, "x")[[1]]
  dims <- strtoi(dims)
  return(dims)
}

calculate_paper_area <- function(dimensions) {
  dims <- split_dimensions(dimensions)
  sides <- c(dims[1]*dims[2], dims[2]*dims[3], dims[3]*dims[1])
  extra <- min(sides)
  area = 2 * sum(sides) + extra
  return(area)
}

calculate_ribbon_length <- function(dimensions) {
  dims <- split_dimensions(dimensions)
  smallest_dims <- dims[-which.max(dims)]  # remove max value
  length <- 2 * sum(smallest_dims)
  length <- length + prod(dims)
  return(length)
}

day02.run_part1 <- function(filepath) {
  input <- file(filepath, open="r")
  dimensions <- readLines(input)
  total <- 0
  for (dims in dimensions) {
    total <- total + calculate_paper_area(dims)
  }
  return(total)
}

day02.run_part2 <- function(filepath) {
  input <- file(filepath, open="r")
  dimensions <- readLines(input)
  total <- 0
  for (dims in dimensions) {
    total <- total + calculate_ribbon_length(dims)
  }
  return(total)
}

update_location <- function(x, y, direction) {
  if (direction == ">") {
    x <- x + 1
  } else if (direction == "<") {
    x <- x - 1
  } else if (direction == "^") {
    y <- y + 1
  } else {
    y <- y - 1
  }
  return(c(x, y))
}

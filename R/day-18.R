ex1 <- "[1,2]"
ex2 <- "[[1,2],3]"
ex3 <- "[9,[8,7]]"
ex4 <- "[[1,2],[3,4]]"

unwrap_pair <- function(str) substr(str, 2, nchar(str) - 1)

is_number <- function(x) {
  suppressWarnings(!is.na(as.numeric(x))) && !is.list(x)
}

split_pair <- function(inside) {
  commas <- unlist(gregexpr(",", inside))

  left_char <- substr(inside, 1, 1)
  right_char <- substr(inside, nchar(inside), nchar(inside))

  # get position of the comma splitting the pair in two parts
  if (is_number(left_char) && !is_number(right_char)) {
    mid_comma <- 1
  } else if (!is_number(left_char) && is_number(right_char)) {
    mid_comma <- length(commas)
  } else if ((left_char == "[" && right_char == "]") |
             (is_number(left_char) && is_number(right_char))) {
    mid_comma <- ceiling(length(commas) / 2)
  } else
    stop("Invalid pair detected", call. = FALSE)

  midpoint <- commas[mid_comma]

  left <- substr(inside, 1, midpoint - 1)
  right <- substr(inside, midpoint + 1, nchar(inside))

  list(left = left, right = right)
}

parse_pair <- function(input, depth = 1) {
  # first unwrap the enclosing brackets
  inside <- unwrap_pair(input)

  # parse the pair at the current level
  pair <- split_pair(inside)
  # upack its left and right components
  left <- pair$left
  right <- pair$right

  # parse left and right components recursively if needed
  if (!is_number(left))
    left <- parse_pair(left, depth + 1)
  else
    left <- as.numeric(left)

  if (!is_number(right))
    right <- parse_pair(right, depth + 1)
  else
    right <- as.numeric(right)

  list(left = left, right = right, depth = depth)
}

increment_depth <- function(pair) {
  left <- pair$left
  right <- pair$right

  if (!is_number(left)) left <- increment_depth(left)
  if (!is_number(right)) right <- increment_depth(right)

  list(left = left, right = right, depth = pair$depth + 1)
}

add_pair <- function(p1, p2) {
  left <- increment_depth(p1)
  right <- increment_depth(p2)
  list(left = left, right = right, depth = max(left$depth, right$depth) - 1)
}

print_pair <- function(pair) {
  left <- pair$left
  right <- pair$right

  left_str <- ifelse(is_number(left), as.character(left), print_pair(left))
  right_str <- ifelse(is_number(right), as.character(right), print_pair(right))

  output <- sprintf("[%s,%s]", left_str, right_str)
  output
}

# parsing
w <- parse_pair(ex1)
x <- parse_pair(ex2)
y <- parse_pair(ex3)
z <- parse_pair(ex4)

# printing
print_pair(w)
print_pair(x)
print_pair(y)
print_pair(z)


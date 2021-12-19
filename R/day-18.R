# Compute the magnitude of the given snailfish number -- first build a recursive
# tree structure of the number and then evaluate it bottom-up
compute_magnitude <- function(number) {
  parse_number(number) |> recursive_magnitude()
}

recursive_magnitude <- function(pair) {
  left <- pair$left
  right <- pair$right

  if (is_integer(left))
    left_value <- left
  else
    left_value <- recursive_magnitude(left)

  if (is_integer(right))
    right_value <- right
  else
    right_value <- recursive_magnitude(right)

  3 * left_value + 2 * right_value
}

# Parse a given snailfish number recursively into a tree structure
parse_number <- function(input, depth = 1) {
  # first unwrap the enclosing brackets
  inside <- unwrap_pair(input)

  # parse the pair at the current level
  pair <- split_inside(inside)
  # upack its left and right components
  left <- pair$left
  right <- pair$right

  # parse left and right components recursively if needed
  if (!is_integer(left))
    left <- parse_number(left, depth + 1)
  else
    left <- as.numeric(left)

  if (!is_integer(right))
    right <- parse_number(right, depth + 1)
  else
    right <- as.numeric(right)

  list(left = left, right = right, depth = depth)
}

is_integer <- function(x) {
  !is.list(x) && suppressWarnings(!is.na(as.numeric(x)))
}

# Unwrap parentheses surrounding a given pair of numbers
unwrap_pair <- function(str) substr(str, 2, nchar(str) - 1)

# Split the unwrapped input snailfish number into two halves
split_inside <- function(inside) {
  open <- 0
  i <- 1
  repeat {
    char <- substr(inside, i, i)
    if (char == "[")
      open <- open + 1
    else if (char == "]")
      open <- open - 1

    i <- i + 1
    if (i > 1 && open == 0) break
  }
  list(
    left = substr(inside, 1, i - 1),
    right = substr(inside, i + 1, nchar(inside))
  )
}
#
# increment_depth <- function(pair) {
#   left <- pair$left
#   right <- pair$right
#
#   if (!is_integer(left)) left <- increment_depth(left)
#   if (!is_integer(right)) right <- increment_depth(right)
#
#   list(left = left, right = right, depth = pair$depth + 1)
# }

add_number <- function(x, y) sprintf("[%s,%s]", x, y)
#
# add_number <- function(p1, p2) {
#   p1 <- parse_number(p1)
#   p2 <- parse_number(p2)
#   left <- increment_depth(p1)
#   right <- increment_depth(p2)
#   result <- list(left = left, right = right, depth = 1)
#   print_number(result)
# }

# Reduce the given recursive snailfish number back to a string representation
print_number <- function(pair) {
  if (is_integer(pair)) return(as.character(pair))

  left <- pair$left
  right <- pair$right

  left_str <- ifelse(is_integer(left), as.character(left), print_number(left))
  right_str <- ifelse(is_integer(right), as.character(right), print_number(right))

  output <- sprintf("[%s,%s]", left_str, right_str)
  output
}

# Find the deepest pair in the nested string-based snailfish number
# representation
find_deepest <- function(str) {
  current_depth <- 0
  max_pair_depth <- max_pair_pos <- 0
  i <- 1
  while (TRUE) {
    if (i > nchar(str)) break
    char <- substr(str, i, i)
    if (char == "[") {
      current_depth <- current_depth + 1
    } else if (char == "]") {
      current_depth <- current_depth - 1
    } else if (is_integer(char) && current_depth > max_pair_depth) {
      max_pair_depth <- current_depth
      max_pair_pos <- i
    }
    # cat(i, " ", substr(str, i, i),
    #     ", current depth", current_depth,
    #     ", max depth", max_pair_depth, "\n")
    i <- i + 1
  }
  # cat(max_pair_depth, ": ", max_pair_pos, "\n")
  # cat(substr(str, max_pair_pos, max_pair_pos + 2), "\n")

  # find the complete pair by splitting the remainder of the number string
  # to the right of the pair's start by "]" and extracting the first such match
  pair <- strsplit(substr(str, max_pair_pos, nchar(str)), "]")[[1]][1]
  values <- strsplit(pair, ",")[[1]]
  list(pair = pair,
       depth = max_pair_depth,
       left_value = as.integer(values[1]),
       right_value = as.integer(values[2]),
       start = max_pair_pos - 1,
       end = max_pair_pos + nchar(pair))
}

# Find the first number in the nested snailfish string representation
find_number <- function(str, reverse) {
  i <- 1
  start <- end <- 0
  repeat {
    char <- substr(str, i, i)
    if (is_integer(char) && start == 0) start <- i
    if (!is_integer(char) && start != 0) {
      end <- i - 1
      break
    }
    if (i == nchar(str)) break

    i <- i + 1
  }
  # cat(i, "-", start, "-", end, "\n")
  number_str <- substr(str, start, end)
  if (reverse)
    number_str <- strsplit(number_str, "")[[1]] |> rev() |> paste(collapse = "")

  number_str
}

# Add a given value to the left-most number in the given snailfish string
add_right <- function(str, value) {
  closest <- find_number(str, reverse = FALSE)
  if (closest != "") {
    replace <- as.integer(closest) + value
    str <- sub(closest, replace, str)
  }
  str
}

# Add a given value to the right-most number in the given snailfish string
add_left <- function(str, value) {
  rev_str <- strsplit(str, "")[[1]] |> rev() |> paste(collapse = "")
  closest <- find_number(rev_str, reverse = TRUE)
  if (closest != "") {
    replace <- as.character(as.integer(closest) + value)
    replace <- strsplit(replace, "")[[1]] |> rev() |> paste(collapse = "")
    rev_closest <- strsplit(closest, "")[[1]] |> rev() |> paste(collapse = "")
    rev_str <- sub(rev_closest, replace, rev_str)
  }
  strsplit(rev_str, "")[[1]] |> rev() |> paste(collapse = "")
}

# Explode the deepest pair in the given snailfish number
explode_number <- function(number) {
  deepest <- find_deepest(number)
  if (deepest$depth > 4) {
    # left_part <- substr(number, 1, deepest$start - nchar(deepest$pair) + 2)
    left_part <- substr(number, 1, deepest$start - 1)
    # add the pair's right value to the leftmost number to the right
    # right_part <- substr(number, deepest$end - nchar(deepest$pair), nchar(number))
    right_part <- substr(number, deepest$end + 1, nchar(number))
    number <- sprintf("%s%s%s",
                      add_left(left_part, deepest$left_value),
                      "0",
                      add_right(right_part, deepest$right_value))
  }
  number
}

# Split the left-most high value in the given snailfish number
split_number <- function(number) {
  # get largest number in the structure
  candidates <-  strsplit(number, "\\[|\\]|,")[[1]] |>
    (\(list) Filter(\(x) x != "", list))() |>
    as.integer() |>
    (\(list) Filter(\(x) x > 9, list))()

  if (length(candidates)) {
    pick <- candidates[1]
    left <- floor(pick / 2)
    right <- ceiling(pick / 2)
    pair <- sprintf("[%s,%s]", left, right)
    number <- sub(as.character(pick), pair, number)
  }

  number
}

# Repeatedly perform explosion and splitting until a snailfish number is
# completely reduced
reduce_number <- function(number, log = FALSE) {
  while (TRUE) {
    if (log) cat(number, " (start)\n")
    exploded_number <- explode_number(number)
    if (exploded_number != number) {
      if (log) cat(exploded_number, " (exploded)\n")
      number <- exploded_number
      next
    } else {
      if (log) cat("explosion not necessary - splitting...\n")
    }
    splitted_number <- split_number(exploded_number)
    if (log) cat(splitted_number, " (splitted)\n")
    if (splitted_number != exploded_number) {
      number <- splitted_number
      next
    } else
      break
  }
  if (log) cat("reduction done\n")
  splitted_number
}

# Sum a series of snailfish numbers
sum_numbers <- function(numbers) {
  sum <- numbers[[1]]
  for (i in seq(2, length(numbers))) {
    # cat("adding ", numbers[[i]], "\n")
    sum <- add_number(sum, numbers[[i]]) |> reduce_number()
    # cat("=", sum, "\n")
  }
  sum
}

# Find the sum of the largest magnitude
compute_max_magnitude <- function(numbers) {
  max_magnitude <- -Inf
  for (x in numbers) {
    for (y in numbers) {
      if (x == y) next
      magnitude <- add_number(x, y) |> reduce_number() |> compute_magnitude()
      if (magnitude > max_magnitude)
        max_magnitude <- magnitude
    }
  }
  max_magnitude
}

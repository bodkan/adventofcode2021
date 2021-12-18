unwrap_pair <- function(str) substr(str, 2, nchar(str) - 1)

is_number <- function(x) {
  !is.list(x) && suppressWarnings(!is.na(as.numeric(x)))
}

split_pair <- function(inside) {
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
  if (is_number(pair)) return(as.character(pair))

  left <- pair$left
  right <- pair$right

  left_str <- ifelse(is_number(left), as.character(left), print_pair(left))
  right_str <- ifelse(is_number(right), as.character(right), print_pair(right))

  output <- sprintf("[%s,%s]", left_str, right_str)
  output
}

should_explode <- function(x) {
  flat <- unlist(x)
  any(flat[grep("depth", names(flat))] > 4)
}

explode_pair <- function(pair, explode = NULL) {
  left <- pair$left
  right <- pair$right

  if (!is.null(explode) && !sum(explode[c("left", "right")]) && explode["any"] != 0) {
    if (is_number(left)) {
      left <- left + explode["any"]
      explode["any"] <- 0
      return(list(left = left, right = right, explode = explode))
    } else if (is_number(right)) {
      right <- right + explode["any"]
      explode["any"] <- 0
      return(list(left = left, right = right, explode = explode))
    }
  }

  # We reached a pair of numbers-- check if it needs to be exploded and if so,
  # carry the information about explosion one level higher
  if (is_number(left) && is_number(right)) {
    # if (left == 3 && right == 2) browser()
    if (pair$depth > 4 && is.null(explode))
      return(list(replace = 0, explode = c(left = left, right = right)))
    else
      return(pair)
  }

  # recursively check for explosion on the left branch
  if (!is_number(left)) {
    left <- explode_pair(left, explode)
    if (is.null(explode)) explode <- left$explode
  }

  # recursively check for explosion on the right
  if (!is_number(right)) {
    right <- explode_pair(right, explode)
    if (is.null(explode)) explode <- right$explode
  }

  if (!is.null(explode)) {
    if (is_number(left) && explode["left"] != 0) {
      left <- left + explode["left"]
      explode["left"] <- 0
    }
    if (is_number(right) && explode["right"] != 0) {
      right <- right + explode["right"]
      explode["right"] <- 0
    }
  }

  if (!is_number(left) && !is.null(left$replace)) left <- left$replace
  if (!is_number(right) && !is.null(right$replace)) right <- right$replace

  if (pair$depth == 1 && !is.null(explode) && any(explode > 0)) {
    if (!is_number(left) && !is.null(left$explode) && explode["right"] > 0) {
      explode["any"] <- explode["right"]
      explode["right"] <- 0
      right <- explode_pair(right, explode)
    }
    if (!is_number(right) && !is.null(right$explode) && explode["left"] > 0) {
      explode["any"] <- explode["left"]
      explode["left"] <- 0
      left <- explode_pair(left, explode)
    }
  }

  list(left = left, right = right, depth = pair$depth, explode = explode)
}


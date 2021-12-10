# Return error score for a given mismatched character
error_score <- function(x) switch(x, ")" = 3, "]" = 57, "}" = 1197, ">" = 25137)

# Return score for autocompleting a given character
autocomplete_score <- function(x) switch(x, "(" = 1, "[" = 2, "{" = 3, "<" = 4)

get_match <- function(x) switch(x, ")" = "(", "]" = "[", "}" = "{", ">" = "<")

#' This is a really ugly implementation of a stack (FIFO)
#'
#' Ugly because the input line is being used, at the same time, as both the
#' input that is being scanned and the stack. This is done by tracking the
#' position of the currently read character and the "top of the stack" using two
#' different indices, and by modifying the vector of character in place (each
#' time a matching character pair is found, we remove both characters from the
#' scanned input).
run_stack <- function(line, mode = c("error", "autocomplete")) {
  mode <- match.arg(mode)

  stack_line <- line

  # index of a current character and the index of a top of the stack
  stack_i <- 1; i <- 2
  corrupt_i <- -1
  # counter of how many matching pairs have been closed already
  n_shifts <- 0

  while (TRUE && i <= length(stack_line)) {
    # on encountering an opening character, simply move along to processing
    # next character, incrementing the stack index as well
    if (stack_line[i] %in% c("(", "[", "{", "<")) {
      i <- i + 1
      stack_i <- stack_i + 1
    } # on encountering a closing character
    else if (get_match(stack_line[i]) == stack_line[stack_i]) {
      # remove the whole matched pair from the stack (and the whole input
      # vector as well)
      stack_line <- stack_line[-c(stack_i, i)]
      # and decrement both pointers because the input/stack has shifted by one
      stack_i <- stack_i - 1
      i <- i - 1
      # another pair of characters has been removed from the input
      n_shifts <- n_shifts + 2
    } else {
      # anything else means that an invalid closing character has been reached
      corrupt_i <- i + n_shifts
      break
    }
  }

  if (corrupt_i != -1 && mode == "error") {
    return(line[corrupt_i])
  } else if (corrupt_i == -1 && mode == "autocomplete") {
    return(rev(stack_line))
  } else
    return(NULL)
}

# Sum up scores for all autocompleted closing characters
sum_autocomplete <- function(chars) {
  total <- 0
  for (i in chars) {
    total <- total * 5 + autocomplete_score(i)
  }
  total
}

# Sum up errors for each corrupt line
report_errors <- function(file) {
  readLines(file) |>
    strsplit("") |>
    sapply(run_stack, mode = "error") |>
    unlist() |>
    sapply(error_score) |>
    sum()
}

# Report the middle autocomplete score across all incomplete lines
report_autocomplete <- function(file) {
  # scan all lines, keep only incomplete ones and complete them (ignore the
  # corrupted ones and those that are complete)
  autocomplete <- readLines(file) |>
    strsplit("") |>
    sapply(run_stack, mode = "autocomplete") |>
    (\(x) Filter(length, x))()

  # sum up the scores of all completed lines, return the middle result
  scores <- sapply(autocomplete, sum_autocomplete) |> sort()
  scores[ceiling(length(scores) / 2)]
}

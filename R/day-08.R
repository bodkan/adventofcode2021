read_digits <- function(file) {
  readLines(file) |>
    strsplit(" \\| ") |>
    lapply(function(x) list(
      patterns = strsplit(x[1], " ")[[1]],
      digits = strsplit(x[2], " ")[[1]]
    ))
}

# Assign observed segment patterns to 0-9 digits, return the four-digit code
assign_digits <- function(input) {
  patterns <- sort_string(input$patterns)
  digits <- sort_string(input$digits)

  lengths <- vapply(patterns, nchar, integer(1))

  names(patterns)[lengths == 2] <- "1"
  names(patterns)[lengths == 3] <- "7"
  names(patterns)[lengths == 4] <- "4"
  names(patterns)[lengths == 7] <- "8"

  names(patterns)[find_match(patterns, 6, 4)] <- "9"
  names(patterns)[find_match(patterns, 6, 7)] <- "0"
  names(patterns)[lengths == 6 & is.na(names(patterns))] <- "6"

  names(patterns)[find_match(patterns, 5, 1)] <- "3"
  names(patterns)[find_match(patterns, 5, 6, 5)] <- "5"
  names(patterns)[is.na(names(patterns))] <- "2"

  lookup <- names(patterns)
  names(lookup) <- patterns

  as.integer(paste(lookup[digits], collapse = ""))
}

# Determine which of the given segment patterns of a specified length shares all
# patterns with the segment patterns of the given digit
find_match <- function(patterns, len, digit, required = NULL) {
  digit <- patterns[[as.character(digit)]]
  if (is.null(required)) required <- nchar(digit)
  dig <- str2vec(digit)
  sapply(patterns, function(x) {
    nchar(x) == len &
      is.na(names(patterns)[patterns == x]) &
      length(intersect(str2vec(x), dig)) == required
  }) |> which()
}

str2vec <- function(str) strsplit(str, "")[[1]]

sort_string <- function(x) as.vector(sapply(x, \(y) paste(sort(str2vec(y)), collapse = "")))

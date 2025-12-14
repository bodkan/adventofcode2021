parse_code <- function(file) {
  readLines(file) |>
    lapply(\(l) {
      tokens <- strsplit(l, " ")[[1]]

      args <- list(tokens[2])
      if (length(tokens) > 2) {
        args[[2]] <- tokens[3]
        if (!is.na(strtoi(args[[2]]))) {
          args[[2]] <- as.integer(args[[2]])
        }
      }

      list(
        instruction = tokens[[1]],
        args = args
      )
    })
}

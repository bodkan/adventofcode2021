read_bingo <- function(file) {
  lines <- readLines(file)

  draws <- strsplit(lines[1], ",") |> (\(.) .[[1]])() |> as.integer()

  bingo_numbers <- lines[3:length(lines)] |>
    strsplit(" ") |>
    unlist() |>
    ( \(.) .[. != ""] )() |>
    as.integer()

  # a single bingo square is always 5x5 in size, as per specification
  n_bingos <- length(bingo_numbers) / 25

  bingos <- array(bingo_numbers, dim = c(5, 5, n_bingos)) |>
    aperm(perm = c(2, 1, 3))

  list(draws = draws, bingos = bingos)
}

solve_bingo <- function(draws, bingos) {
  n_bingos <- dim(bingos)[3]

  marks <- array(FALSE, dim = dim(bingos))
  solved <- NULL

  for (number in draws) {
    marks <- marks | (bingos == number)

    col_marks <- colSums(marks, dims = 1)
    row_marks <- aperm(marks, perm = c(2, 1, 3)) |> colSums(dims = 1)

    col_hit <- which(col_marks == 5, arr.ind = TRUE)
    row_hit <- which(row_marks == 5, arr.ind = TRUE)

    if (length(col_hit)) solved <- col_hit[2]
    if (length(row_hit)) solved <- row_hit[2]

    if (!is.null(solved)) break
  }

  unmarked <- bingos[, , solved][!marks[, , solved]]
  sum(unmarked) * number
}


solve_bingo2 <- function(draws, bingos) {
  n_bingos <- dim(bingos)[3]

  marks <- array(FALSE, dim = dim(bingos))
  solved <- c()

  for (number in draws) {
    # if (number == 13) browser()
    marks <- marks | (bingos == number)

    col_marks <- colSums(marks, dims = 1)
    row_marks <- aperm(marks, perm = c(2, 1, 3)) |> colSums(dims = 1)

    col_hit <- which(col_marks == 5, arr.ind = TRUE)
    row_hit <- which(row_marks == 5, arr.ind = TRUE)

    if (length(col_hit)) {
      next_solved <- col_hit[, 2]
      solved <- c(solved, next_solved[!next_solved %in% solved])
    }
    if (length(row_hit)) {
      next_solved <- row_hit[, 2]
      solved <- c(solved, next_solved[!next_solved %in% solved])
    }

    if (length(unique(solved)) == n_bingos) break
  }

  last_solved <- solved[length(solved)]

  unmarked <- bingos[, , last_solved][!marks[, , last_solved]]
  sum(unmarked) * number
}

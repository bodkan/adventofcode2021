#' Read the list of drawn numbers and all bingo squares from the input file.
#' Return both as a list of two elements: a vector of drawn numbers, and a
#' three-dimensional array of bingo squares.
read_bingo <- function(file) {
  lines <- readLines(file)

  # read individual draws
  draws <- strsplit(lines[1], ",") |> (\(.) .[[1]])() |> as.integer()

  # read numbers of all bingo squares into a single vector
  bingo_numbers <- lines[3:length(lines)] |>
    strsplit(" ") |>
    unlist() |>
    ( \(.) .[. != ""] )() |>
    as.integer()

  # compute the number of 5x5 bingo squares in the input
  n_bingos <- length(bingo_numbers) / 25

  # convert the bingo numbers into `n_bingos` 5x5 matrices (i.e.
  # a three-dimensional array)
  bingos <- array(bingo_numbers, dim = c(5, 5, n_bingos)) |>
    aperm(perm = c(2, 1, 3))

  list(draws = draws, bingos = bingos)
}

#' Find the solution to the puzzle: either based on the first solved bingo
#' square (part 1), or the last puzzle solved (part 2)
solve_bingo <- function(draws, bingos, find = c("first", "last")) {
  # are we looking for the first solved bingo or the last?
  find <- match.arg(find)

  n_bingos <- dim(bingos)[3]

  # create a logical "mask" of positions with numbers that have
  # been already drawn
  marks <- array(FALSE, dim = dim(bingos))

  # collection of indices of completed bingo squares
  solved <- c()

  for (number in draws) {
    # flip the positions in bingos carrying the number to TRUE
    marks <- marks | (bingos == number)

    # count the number of hits in all columns/rows of every bingo
    # square in the array of all squares at once
    col_marks <- colSums(marks, dims = 1)
    row_marks <- aperm(marks, perm = c(2, 1, 3)) |> colSums(dims = 1)

    # detect which bingo squares have all five positions of their
    # columns/rows marked
    col_hit <- which(col_marks == 5, arr.ind = TRUE)
    row_hit <- which(row_marks == 5, arr.ind = TRUE)

    # next potential solved squares
    next_solved <- list()
    if (length(col_hit)) next_solved[[1]] <- as.integer(col_hit[, 2])
    if (length(row_hit)) next_solved[[2]] <- as.integer(row_hit[, 2])
    next_solved <- unique(unlist(next_solved))

    # add those squared just solved (which have not already been
    # solved before) to the list of solutions
    if (!is.null(next_solved))
      solved <- c(solved, next_solved[!next_solved %in% solved])

    if ((find == "first" && length(solved) > 0) ||
        (find == "last" && length(solved) == n_bingos))
      break
  }

  # get the first (or last) solved bingo
  solved_bingo <- bingos[, , solved[length(solved)]]
  solved_marks <- marks[, , solved[length(solved)]]

  unmarked_numbers <- solved_bingo[!solved_marks]

  sum(unmarked_numbers) * number
}

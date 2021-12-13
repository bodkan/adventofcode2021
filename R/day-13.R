# Execute all folding instructions on a given sheet of paper
fold_all <- function(m, folds) {
  for (instruction in folds)
    m <- fold_paper(m, instruction)
  m
}

# Make the generated ASCII image of the code readable on terminal
decode_day13 <- function(m) {
  lines <- sapply(
    seq_len(nrow(m)),
    \(i) paste(as.integer(m[i, ]), collapse = "")
  ) |>
    (\(x) gsub("1", "#", x))() |>
    (\(x) gsub("0", " ", x))()
  lines
}

# Execute one folding instruction
fold_paper <- function(m, instruction) {
  axis <- instruction[[1]]
  position <- instruction[[2]]

  if (axis == "y") {
    rows <- list(1:(position - 1), (position + 1):nrow(m))
    cols <- list(1:ncol(m), 1:ncol(m))
  } else {
    rows <- list(1:nrow(m), 1:nrow(m))
    cols <- list(1:(position - 1), (position + 1):ncol(m))
  }

  half1 <- m[rows[[1]], cols[[1]]]
  half2 <- m[rows[[2]], cols[[2]]] |> flip_paper(axis)

  half1 | half2
}

# Read the input file, return a list with a dotted sheet of paper
# and the series of folding instructions
read_day13 <- function(file) {
  lines <- file |> readLines()

  blank <- which(lines == "")

  dots <- lines[1:(blank - 1)] |> strsplit(",") |> lapply(as.integer)
  folds <- lines[(blank + 1):length(lines)] |>
    strsplit(" ") |>
    lapply(function(x) {
      tokens <- strsplit(x[3], "=")[[1]]
      list(tokens[1], as.integer(tokens[2]) + 1)
    })

  # set up dots on the paper
  xdim <- max(sapply(dots, `[[`, 1)) + 1
  ydim <- max(sapply(dots, `[[`, 2)) + 1

  paper <- matrix(FALSE, nrow = ydim, ncol = xdim)
  for (dot in dots) {
    paper[dot[2] + 1, dot[1] + 1] <- TRUE
  }

  list(paper = paper, folds = folds)
}

# Flip either the bottom half of the paper up, or the right half of
# the paper to the left
flip_paper <- function(m, axis) {
  if (axis == "x")
    m[, rev(seq_len(ncol(m)))]
  else if (axis == "y")
    m[rev(seq_len(nrow(m))), ]
  else
    stop("Invalid axis '", axis, "'", call. = FALSE)
}

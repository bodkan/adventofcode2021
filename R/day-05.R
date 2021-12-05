#' Read coordinates of lines which indicate the position/direction of vents
#'
#' @param file Input file, one line for each start-end coordinate of a line
#' @param ... Optional list of predicate function that can be applied to filter
#'   the lines to only those of a given class
read_lines <- function(file, ...) {
  lines <- readLines(file) |>
    strsplit(" -> ") |>
    lapply(parse_line)

  filter_funs <- list(...)
  if (length(filter_funs))
    lines <- Filter(\(x) apply_filters(x, filter_funs), lines)

  lines
}

#' Parse a single line into two coordinates of two points
parse_line <- function(x) {
  as.integer(unlist(strsplit(x, ",")))
}

#' Predicates used for filtering for certain classes of lines
is_horizontal <- function(line) line[2] == line[4]
is_vertical <- function(line) line[1] == line[3]
is_diagonal <- function(line) abs(line[3] - line[1]) == abs(line[4] - line[2])

#' Helper function combining different predicates into one
apply_filters <- function(x, funs) any(vapply(funs, \(f) f(x), logical(1)))

#' Generate coordinates of all points which form a line
line_points <- function(l) {
  names(l) <- c("x1", "y1", "x2", "y2")
  if (is_horizontal(l)) {
    xs <- seq(l["x1"], l["x2"])
    ys <- rep(l["y1"], length(xs))
  } else if (is_vertical(l)) {
    ys <- seq(l["y1"], l["y2"])
    xs <- rep(l["x1"], length(ys))
  } else if (is_diagonal(l)) {
    xs <- seq(l["x1"], l["x2"])
    ys <- seq(l["y1"], l["y2"])
  }
  else
    stop("Weird line detected", call. = FALSE)

  matrix(c(xs, ys), ncol = 2)
}

#' Count at how many points do all lines overlap
count_overlaps <- function(points) {
  nrow(unique(points[duplicated(points), ]))
}

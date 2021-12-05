parse_line <- function(x) {
  as.integer(unlist(strsplit(x, ",")))
}

#' Read coordinates of lines which indicate the position/direction of vents
read_lines <- function(file, ...) {
  lines <- readLines(file) |>
    strsplit(" -> ") |>
    lapply(parse_line)

  filter_funs <- list(...)
  if (length(filter_funs))
    lines <- Filter(\(x) apply_filters(x, filter_funs), lines)

  lines
}

is_horizontal <- function(line) line[2] == line[4]
is_vertical <- function(line) line[1] == line[3]
is_diagonal <- function(line) abs(line[3] - line[1]) == abs(line[4] - line[2])

apply_filters <- function(x, funs) any(vapply(funs, \(f) f(x), logical(1)))

make_map <- function(lines) {
  max_x <- max(sapply(lines, function(l) max(c(l[1], l[3]))))
  max_y <- max(sapply(lines, function(l) max(c(l[2], l[4]))))
  matrix(0, nrow = max_x + 1, ncol = max_y + 1, byrow = TRUE)
}

line_coords <- function(l) {
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

count_overlaps1 <- function(points) {
  sum(duplicated(points))
}

count_overlaps2 <- function(map, points) {
  for (i in seq_len(nrow(points))) {
    p <- points[i, ] + 1
    map[p[2], p[1]] <- map[p[2], p[1]] + 1
  }
  sum(map > 1)
}

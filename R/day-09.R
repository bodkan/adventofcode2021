# Read the integer matrix of heights at each position from the input file
read_heights <- function(file) {
  lines <- readLines(file) |> lapply(strsplit, "")
  lines |> unlist() |> as.integer() |> matrix(nrow = length(lines), byrow = TRUE)
}

# For a given matrix, generate a matrix shifted by one row/column
# up/down or left/right.
shift <- function(m, direction = c("up", "down", "left", "right")) {
  direction <- match.arg(direction)

  new_row <- matrix(Inf, ncol = ncol(m))
  new_col <- matrix(Inf, nrow = nrow(m))

  switch(direction,
         up     = rbind(m[-1, ], new_row),
         down   = rbind(new_row, m[-nrow(m), ]),
         right  = cbind(m[, -1], new_col),
         left  = cbind(new_col, m[, -ncol(m)]))
}

# Detect lowpoints by finding those positions of a matrix which carry a number
# smaller then all four shifted matrices.
find_lowpoints <- function(heightmap) {
  (
    heightmap < shift(heightmap, "up") &
    heightmap < shift(heightmap, "down") &
    heightmap < shift(heightmap, "right") &
    heightmap < shift(heightmap, "left")
  ) |>
    which(arr.ind = TRUE)
}

# Does the given point lie within a map/matrix?
in_map <- function(p, m) {
  p[1] > 0 && p[1] <= nrow(m) &&
  p[2] > 0 && p[2] <= ncol(m)
}

# Does the next point lie at a higher elevation than the current point,
# and is it lower than 9 as per the puzzle specification?
is_higher <- function(p_now, p_next, m) m[p_now] < m[p_next] && m[p_next] < 9

# Recursively explore a given matrix to find all locations which are higher
# than the location of entry (depth-first search)
#
# Locations already visited are tracked through the recursive dives via
# the `visited` logical matrix
find_basin <- function(p, m, visited) {
  visited[p] <- TRUE

  directions <- list(
    "up" = c(-1, 0),
    "down" = c(1, 0),
    "left" = c(0, -1),
    "right" = c(0, 1)
  )

  nodes <- sprintf("%d-%d", p[1], p[2])

  for (dir in directions) {
    p_next <- p + dir
    if (in_map(p_next, m) && is_higher(p, p_next, m) && !visited[p_next]) {
      result <- find_basin(p_next, m, visited)
      nodes <- c(nodes, result$nodes)
      visited <- result$visited
    }
  }

  list(nodes = nodes, visited = visited)
}

# Detect all lowpoints in a given height matrix and find their associated
# basin coordinates
find_all_basins <- function(heightmap) {
  lowpoints <- find_lowpoints(heightmap)

  # get logical mask for recording visited coordinates
  visited <- matrix(FALSE, nrow = nrow(heightmap), ncol = ncol(heightmap))

  basins <- list()
  for (i in seq_len(nrow(lowpoints))) {
    result <- find_basin(lowpoints[i, , drop = FALSE], heightmap, visited)
    basins[[i]] <- result$nodes
    visited <- result$visited
  }

  basins
}

#' Compute lowpoint risk measure for part 1
compute_risk <- function(heightmap) {
  lowpoints <- find_lowpoints(heightmap)
  sum(heightmap[lowpoints] + 1)
}

#' Find the three largest basins and compute the product of their sizes
#' as required by part 2
compute_basin_product <- function(heightmap) {
  find_all_basins(heightmap) |>
    sapply(length) |>
    sort(decreasing = TRUE) |>
    head(3) |>
    prod()
}

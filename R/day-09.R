read_heights <- function(file) {
  lines <- readLines(file) |> lapply(strsplit, "")
  lines |> unlist() |> as.integer() |> matrix(nrow = length(lines), byrow = TRUE)
}

# Vectorized, matrix-based solution to part 1 of the puzzle.
#
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

in_map <- function(p, m) {
  p[1] > 0 && p[1] <= nrow(m) &&
  p[2] > 0 && p[2] <= ncol(m)
}

is_higher <- function(p_now, p_next, m) m[p_now] < m[p_next] && m[p_next] < 9

# Recursively explore a given matrix to find all locations which are higher
# than the location of entry
#
# Locations already visited are tracked through the recursive dives via
# the `visited` logical matrix
dive <- function(p, m, visited) {
  visited[p] <- TRUE

  up <- p + c(-1, 0)
  down <- p + c(1, 0)
  left <- p + c(0, -1)
  right <- p + c(0, 1)

  nodes <- sprintf("%d-%d", p[1], p[2])

  if (in_map(up, m) && is_higher(p, up, m) && !visited[up]) {
    result <- dive(up, m, visited)
    nodes <- c(nodes, result$nodes)
    visited <- result$visited
  }
  if (in_map(down, m) && is_higher(p, down, m) && !visited[down]) {
    result <- dive(down, m, visited)
    nodes <- c(nodes, result$nodes)
    visited <- result$visited
  }
  if (in_map(left, m) && is_higher(p, left, m) && !visited[left]) {
    result <- dive(left, m, visited)
    nodes <- c(nodes, result$nodes)
    visited <- result$visited
  }
  if (in_map(right, m) && is_higher(p, right, m) && !visited[right]) {
    result <- dive(right, m, visited)
    nodes <- c(nodes, result$nodes)
    visited <- result$visited
  }

  list(nodes = nodes, visited = visited)
}

find_basins <- function(heightmap) {
  lowpoints <- find_lowpoints(heightmap)

  # get logical mask for recording visited coordinates
  visited <- matrix(FALSE, nrow = nrow(heightmap), ncol = ncol(heightmap))

  basins <- list()
  for (i in seq_len(nrow(lowpoints))) {
    result <- dive(lowpoints[i, , drop = FALSE], heightmap, visited)
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
  find_basins(heightmap) |>
    sapply(length) |>
    sort(decreasing = TRUE) |>
    head(3) |>
    prod()
}

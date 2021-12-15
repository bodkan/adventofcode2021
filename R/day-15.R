# Calculate the cost of travel from the upper left corner of the map to the
# bottom right corner using Dijkstra's algorithm
#
# This is a very naive implementation and quite slow too. On the full input of
# AoC part 2 it runs in about 30 minutes. This could be be improved by adding a
# real priority queue and by being a bit smarter about detecting adjacent nodes.
# Currently, on visiting a position in the matrix we "look around" for valid
# neighboring cells which is extremely inefficient. Encoding the matrix in a
# real graph structure would be much better.
compute_cost <- function(map, log = FALSE) {
  # matrix for tracking nodes already visited
  visited <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
  # map with continualy updated travel costs
  costs <- matrix(Inf, nrow = nrow(map), ncol = ncol(map))
  costs[1, 1] <- 0

  while (TRUE) {
    # take a node with the smallest cost among all unvisited nodes
    min_cost <- min(costs[!visited])
    current <- which(costs == min_cost & !visited, arr.ind = TRUE)[1, , drop = FALSE]

    if (log && sum(visited) %% 1000 == 0)
      cat(sum(visited), "/", prod(dim(map)), "nodes visited\r")

    # terminate if we arrived at the final destination
    if (all(current == dim(map))) break

    # get all unvisited neighbors of the current node
    neighbors <- get_neighbors(current, visited)
    # calculate the cost of travel to those nodes if we were to travel to
    # the via the current node
    potential_costs <- costs[current] + map[neighbors]
    # if those cost would be lower than what we have computed so far,
    # update the costs
    smaller_costs <- costs[neighbors] > potential_costs
    costs[neighbors[smaller_costs, , drop = FALSE]] <- potential_costs[smaller_costs]

    # mark the current node as visited
    visited[current] <- TRUE
  }

  if (log) cat("All", prod(dim(map)), "nodes visited                        \n")

  costs[nrow(costs), ncol(costs)]
}

# Read the map from the input file into a matrix
read_day15_map <- function(file) {
  lines <- readLines(file) |> lapply(strsplit, "")
  lines |> unlist() |> as.integer() |> matrix(nrow = length(lines), byrow = TRUE)
}

# Get all neighbors of a given node
get_neighbors <- function(pos, visited) {
  # create coordinates of all potential neighbors
  adj_i <- pos[1] + c(0, -1, 1, 0)
  adj_j <- pos[2] + c(-1, 0, 0, 1)
  # check if they lie within the bounds of the map
  valid <- adj_i > 0 & adj_i <= nrow(visited) & adj_j > 0 & adj_j <= ncol(visited)
  # return only those that have not been visited yet
  coords <- matrix(c(adj_i[valid], adj_j[valid]), ncol = 2)
  coords[!visited[coords], , drop = FALSE]
}

# Create a larger tiled map for part 2 of the puzzle
tile_map <- function(map) {
  tiles <- list(map)
  # generate shifted tiles
  for (i in 2:9) {
    next_m <- tiles[[i - 1]] + 1
    next_m[next_m > 9] <- 1
    tiles[[i]] <- next_m
  }
  # bind them all into a larger matrix
  rbind(
    cbind(tiles[[1]], tiles[[2]], tiles[[3]], tiles[[4]], tiles[[5]]),
    cbind(tiles[[2]], tiles[[3]], tiles[[4]], tiles[[5]], tiles[[6]]),
    cbind(tiles[[3]], tiles[[4]], tiles[[5]], tiles[[6]], tiles[[7]]),
    cbind(tiles[[4]], tiles[[5]], tiles[[6]], tiles[[7]], tiles[[8]]),
    cbind(tiles[[5]], tiles[[6]], tiles[[7]], tiles[[8]], tiles[[9]])
  )
}

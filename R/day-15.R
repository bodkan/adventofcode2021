# Calculate the cost of travel from the upper left corner of the map to the
# bottom right corner
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

    if (log && sum(visited) %% 500 == 0)
      cat(sum(visited), " - ", current, "\n")

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

  costs[nrow(costs), ncol(costs)]
}
read_day15_map <- function(file) {
  lines <- readLines(file) |> lapply(strsplit, "")
  lines |> unlist() |> as.integer() |> matrix(nrow = length(lines), byrow = TRUE)
}

# Get all neighbors of a given node
get_neighbors <- function(pos, visited) {
  directions <- matrix(c(       0, -1,
                                -1, 0,        1, 0,
                                0, 1       ), ncol = 2, byrow = TRUE)
  coords <- pos[rep(1, 4), ] + directions
  coords <- coords[
    coords[, 1] > 0 & coords[, 1] <= nrow(visited) &
      coords[, 2] > 0 & coords[, 2] <= ncol(visited), , drop = FALSE]

  coords[!visited[coords], , drop = FALSE]
}

# Create a larger tiled map for part 2 of the puzzle
tile_map <- function(map) {
  map1 <- map

  map2 <- map1 + 1
  map2[map2 > 9] <- 1

  map3 <- map2 + 1
  map3[map3 > 9] <- 1

  map4 <- map3 + 1
  map4[map4 > 9] <- 1

  map5 <- map4 + 1
  map5[map5 > 9] <- 1

  map6 <- map5 + 1
  map6[map6 > 9] <- 1

  map7 <- map6 + 1
  map7[map7 > 9] <- 1

  map8 <- map7 + 1
  map8[map8 > 9] <- 1

  map9 <- map8 + 1
  map9[map9 > 9] <- 1

  rbind(
    cbind(map1, map2, map3, map4, map5),
    cbind(map2, map3, map4, map5, map6),
    cbind(map3, map4, map5, map6, map7),
    cbind(map4, map5, map6, map7, map8),
    cbind(map5, map6, map7, map8, map9)
  )
}

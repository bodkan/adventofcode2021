# First condition for traversing the graph
can_enter1 <- function(node, visits) {
  toupper(node) == node | !visits[node]
}

# Second condition for traversing the graph
can_enter2 <- function(node, visits) {
  toupper(node) == node |
    !visits[node] |
    (node != "start" &&
       visits[node] == 1 &&
       all(visits[!names(visits) %in% c("start", node) &
                    names(visits) == tolower(names(visits))
       ] != 2))
}

# Read the adjacency matrix from the input file of graph edges
read_caves <- function(file) {
  # read edges from the input file
  edges <- readLines(file) |> strsplit("-")
  # get nodes of the graph
  nodes <- sort(unique(unlist(edges)))

  # generate empty adjacency matrix ...
  adj <- matrix(FALSE, nrow = length(nodes), ncol = length(nodes))
  rownames(adj) <- colnames(adj) <- nodes
  # ... and fill it accordingly
  for (e in edges) {
    adj[e[1], e[2]] <- TRUE
    adj[e[2], e[1]] <- TRUE
  }
  adj
}

# Recursively visit each node of the graph assuming a given condition
# function provided in `can_enter`
visit <- function(node, adjacency, visits, path, result, condition) {
  if (node == "end") {
    result$paths <- append(result$paths, list(c(path, node)))
    return()
  }

  visits[node] <- visits[node] + 1

  neighbors <- names(which(adjacency[node, ]))
  for (neighbor in neighbors) {
    if (condition(neighbor, visits))
      visit(neighbor, adjacency, visits, path = c(path, node), result, condition)
  }
}

# Find all paths through the graph specified in the input file using
# the give recursive condition
find_paths <- function(file, condition) {
  # create new environment for saving the paths detected deep in the
  # recursion calls using reference semantics
  result <- new.env()
  result$paths <- list()

  # read the graph as an adjacency matrix
  adjacency <- read_caves(file)

  # create a vector for tracking the number of visits of each node
  visits <- rep(0, nrow(adjacency))
  names(visits) <- unique(rownames(adjacency))

  # start exploring the graph from the node 'start'
  visit(node = "start", adjacency, visits, path = c(), result, condition)

  result$paths
}

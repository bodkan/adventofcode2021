# Recursively visit each node of the graph assuming a given condition
visit <- function(node, adjacency, visits, path, result, condition) {
  if (node == "end") {
    # the puzzle only requires counting of the paths, no need to track them
    # explicitly (this significantly speeds up the solution)
    # result$paths <- append(result$paths, list(c(path, node)))
    result$paths <- result$paths + 1
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
  result$paths <- 0

  # read the graph as an adjacency matrix
  adjacency <- read_caves(file)

  # create a vector for tracking the number of visits of each node
  visits <- rep(0, nrow(adjacency))
  names(visits) <- unique(rownames(adjacency))

  # start exploring the graph from the node 'start'
  visit(node = "start", adjacency, visits, path = c(), result, condition)

  result$paths
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

# First condition for traversing the graph (part 1)
can_enter1 <- function(node, visits) {
  # a big node can be visited multiple times, small node only once
  toupper(node) == node | visits[node] == 0
}

# Second condition for traversing the graph (part 2)
can_enter2 <- function(node, visits) {
  # four conditions under which a node can be visited more than once
  big_cave <- toupper(node) == node    # a big cave
  not_visited <- !visits[node]         # cave not yet visited
  not_start <- node != "start"         # not the starting point
  second_visit <- visits[node] == 1 && # no other small cave visited twice
    all(visits[names(visits) != node &
               names(visits) == tolower(names(visits))] <= 1)

  big_cave || not_visited || (not_start && second_visit)
}

read_cucumbers <- function(file) {
  split_lines <- lapply(readLines(file), \(l) strsplit(l, "")[[1]])
  east <- do.call(rbind, lapply(split_lines, \(x) x == ">"))
  south <- do.call(rbind, lapply(split_lines, \(x) x == "v"))
  list(east = east, south = south)
}

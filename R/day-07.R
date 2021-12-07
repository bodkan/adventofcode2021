read_crabs <- function(file) {
  scan(file, what = integer(), sep = ",", quiet = TRUE)
}

#' Compute the fuel requirements to move all crabs to one of the given final
#' positions in a vectorized way
#'
#' @param crabs Vector of integer positions of all crabs
#' @param options Vector of potential final positions
#' @param fuel_fun Function to transform the distances into fuel
#'
#' @return Vector of fuel requirements to get to each position among the
#'   \code{options}
compute_fuel <- function(crabs, options, fuel_fun = identity) {
  colSums(fuel_fun(abs(outer(crabs, options, "-"))))
}

incremental_fuel <- function(i) (1 + i) * i / 2

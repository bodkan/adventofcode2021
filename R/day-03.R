read_matrix <- function(file) {
  if (!file.exists(file))
    stop("File '", file, "' does not exist", call. = FALSE)

  m <- readLines(file) |>
    sapply(strsplit, "") |>
    sapply(as.integer) |>
    t()

  rownames(m) <- colnames(m) <- NULL

  m
}

# Convert a given binary digit (vector of 0 and 1) into decimal
bin_to_dec <- function(bits) sum(bits * 2^(seq_along(bits) - 1))

compute_gamma_rate <- function(m) {
  bits <- get_bits(m, `>`)
  bin_to_dec(bits)
}

compute_epsilon_rate <- function(m) {
  bits <- get_bits(m, `<`)
  bin_to_dec(bits)
}

compute_oxygen_rating <- function(m) {
  bits <- find_rating(m, `>=`)
  bin_to_dec(bits)
}

compute_co2_rating <- function(m) {
  bits <- find_rating(m, `<=`)
  bin_to_dec(bits)
}

# Determine which binary number satisfies the given condition
get_bits <- function(m, op) {
  proportions <- colSums(m) / nrow(m)
  result <- rep(NA, length(proportions))

  # what bit should we assign to positions with equal numbers of 0 and 1
  # is determined by whether we're computing the oxygen rating (1) or the
  # CO2 rating (0), which is implied by the comparison operator used
  equal <- if (identical(op, `>=`)) 1 else 0

  result[proportions == 0.5] <- equal
  result[proportions != 0.5] <- op(proportions[proportions != 0.5], 0.5)

  result |> rev()
}

# Narrow down the matrix of binary numbers to the oxygen/CO2 rating
# determined by the comparison operator (favoring 1s or 0s as per
# the problem specification)
find_rating <- function(m, op) {
  # begin by creating a logical mask allowing all binary numbers to
  # be considered as the final rating
  mask <- rep(TRUE, nrow(m))

  # iterate along individual bits, looking for numbers matching 1 or 0
  # at a given position
  for (i in seq_len(ncol(m))) {
    # if only one number remains, this is the final rating value
    if (length(which(mask)) == 1) break
    # otherwise narrow down the list of numbers by expanding the logical mask
    bit <- get_bits(m[mask, i, drop = FALSE], op)
    mask <- as.vector(mask & m[, i] == bit)
  }

  bin_to_dec(rev(m[mask, ]))
}

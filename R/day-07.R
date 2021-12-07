read_crabs <- function(file) {
  scan(file, what = integer(), sep = ",", quiet = TRUE)
}

sum_fuel <- function(i) (1 + i) * i / 2

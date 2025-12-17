suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-24.txt", package = "adventofcode2021")

program <- parse_program(input_file)

# input_number <- process_input(13579246899999)
#
# init_alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
# run_program(init_alu, program, input_number, debug = F)

starts <- c(which(sapply(program, \(x) x$instruction == "inp")), length(program) + 1)
subprograms <- lapply(seq_len(length(starts) - 1), \(i) program[starts[i] : (starts[i + 1] - 1)])


find <- function(solution, target_z, subprograms) {
  if (length(solution) == 6)
    return(solution)

  program <- subprograms[[14 - length(solution)]]

  for (digit in 9:1) {
    for (z in seq(5e3) + target_z * program[[5]]$args[[2]]) {
      result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), program, bit64::as.integer64(digit))
      if (result$z == target_z) {
        cat("Recursing at:", length(solution) + 1, "while exploring digit =", digit, "on z =", z, "[ solution so far:", paste(solution, collapse = ""), "]...\n")
        next_solution <- find(solution = c(solution, digit), target_z = z, subprograms)
        if (!is.null(next_solution))
          return(next_solution)
      }
    }
  }

  return(NULL)
}

debug(find)
paste(find(solution = list(), target_z = 0, subprograms), collapse = "")







# 14th
sub_i <- 14
combs_14 <- NULL
for (z in seq(5000)) {
  if (z %% 1000 == 0) cat(z, "\r")
for (digit in 1:9) {
  result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
  # cat("digit =", digit, ":", concatenate(result), "\n")
  combs_14 <- rbind(combs_14, c(digit, z, as.numeric(result$z)))
}
}
valid <- combs_14[combs_14[, 3] == 0, ]
next_z <- valid[which.max(valid[, 1]), 2]


# 13th
sub_i <- 13
combs_13 <- NULL
for (z in seq(5000) + next_z * 26) {
  if (z %% 1000 == 0) cat(z, "\r")
  for (digit in 1:9) {
    result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
    # cat("digit =", digit, ":", concatenate(result), "\n")
    combs_13 <- rbind(combs_13, c(digit, z, as.numeric(result$z)))
  }
}
valid <- combs_13[combs_13[, 3] %in% 535, ]
next_z <- valid[which.max(valid[, 1]), 2]


# 12th
sub_i <- 12
combs_12 <- NULL
for (z in seq(5000) + next_z * 26) {
  if (z %% 1000 == 0) cat(z, "\r")
  for (digit in 1:9) {
    result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
    # cat("digit =", digit, ":", concatenate(result), "\n")
    combs_12 <- rbind(combs_12, c(digit, z, as.numeric(result$z)))
  }
}
valid <- combs_12[combs_12[, 3] %in% next_z, ]
next_z <- valid[which.max(valid[, 1]), 2]



# 11th
sub_i <- 11
combs_11 <- NULL
for (z in seq(5000) + next_z * 26) {
  if (z %% 1000 == 0) cat(z, "\r")
  for (digit in 1:9) {
    result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
    # cat("digit =", digit, ":", concatenate(result), "\n")
    combs_11 <- rbind(combs_11, c(digit, z, as.numeric(result$z)))
  }
}
valid <- combs_11[combs_11[, 3] %in% next_z, ]
next_z <- valid[which.max(valid[, 1]), 2]




# 10th
sub_i <- 10
combs_10 <- NULL
for (z in seq(5000) + next_z * 26) {
  if (z %% 1000 == 0) cat(z, "\r")
  for (digit in 1:9) {
    result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
    # cat("digit =", digit, ":", concatenate(result), "\n")
    combs_10 <- rbind(combs_10, c(digit, z, as.numeric(result$z)))
  }
}
valid <- combs_10[combs_10[, 3] %in% next_z, ]
next_z <- valid[which.max(valid[, 1]), 2]





# 9th
sub_i <- 9
combs_9 <- NULL
for (z in seq(5000)) {
  if (z %% 1000 == 0) cat(z, "\r")
  for (digit in 1:9) {
    result <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = z), subprograms[[sub_i]], bit64::as.integer64(digit))
    # cat("digit =", digit, ":", concatenate(result), "\n")
    combs_9 <- rbind(combs_9, c(digit, z, as.numeric(result$z)))
  }
}
valid <- combs_9[combs_9[, 3] %in% next_z, ]
# next_z <- valid[which.max(valid[, 1]), 2]





# part1 <-
#
# print_result(day = 24, part = 1, part1)
# check_answer(day = 24, part = 1, part1)
#
# part2 <-
#
# print_result(day = 24, part = 2, part2)
# check_answer(day = 24, part = 2, part2)
#
# print_sep()

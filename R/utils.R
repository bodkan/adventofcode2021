print_result <- function(day, part, result) {
  cat(sprintf("Day %d, part %d result: %s\n", day, part, result))
}

print_sep <- function() cat("-------\n")

#' Double check solutions for full puzzle inputs after refactoring
#' and other retrospective code updates
check_answer <- function(day, part, result) {
  answers <- list(
    "1-1" = 1400,
    "1-2" = 1429,
    "2-1" = 1488669,
    "2-2" = 1176514794,
    "3-1" = 4103154,
    "3-2" = 4245351,
    "4-1" = 31424,
    "4-2" = 23042,
    "5-1" = 6548,
    "5-2" = 19663,
    "6-1" = 390011,
    "6-2" = 1746710169834
  )
  if (answers[sprintf("%d-%d", day, part)] != result)
    stop("Wrong answer for day ", day, " part ", part, call. = FALSE)
}

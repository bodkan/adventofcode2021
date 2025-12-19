print_result <- function(day, part, result) {
  cat(sprintf("Day %d, part %d result: %s\n", day, part, result))
}

print_sep <- function() cat("-------\n")

test_name <- function(day, part, subtitle = "") {
  if (subtitle != "") subtitle <- sprintf("(%s)", subtitle)
  sprintf("Day %d, part %d adheres to the specification %s", day, part, subtitle)
}

# Save the example string input from Advent of Code to a file
create_test_file <- function(test_input) {
  test_file <- tempfile()
  writeLines(test_input, test_file)
  test_file
}

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
    "6-2" = 1746710169834,
    "7-1" = 356179,
    "7-2" = 99788435,
    "8-1" = 387,
    "8-2" = 986034,
    "9-1" = 545,
    "9-2" = 950600,
    "10-1" = 240123,
    "10-2" = 3260812321,
    "11-1" = 1659,
    "11-2" = 227,
    "12-1" = 4167,
    "12-2" = 98441,
    "13-1" = 850,
    "13-2" = c(
      " ##  #  #  ##   ##  ###   ##   ##  #  # ",
      "#  # #  # #  # #  # #  # #  # #  # #  # ",
      "#  # #### #    #    #  # #    #  # #  # ",
      "#### #  # # ## #    ###  # ## #### #  # ",
      "#  # #  # #  # #  # #    #  # #  # #  # ",
      "#  # #  #  ###  ##  #     ### #  #  ##  "
    ), # read: AHGCPGAU
    "14-1" = 3284,
    "14-12" = 4302675529689,
    "15-1" = 609,
    "15-2" = 2925,
    "16-1" = 852,
    "16-2" = 19348959966392,
    "17-1" = 15931,
    "17-2" = 2555,
    "18-1" = 4469,
    "18-2" = 4770,
    "19-1" = 408,
    "19-2" = 13348,
    "20-1" = 5316,
    "20-2" = 16728,
    "21-1" = 918081,
    "21-2" = 158631174219251,
    "22-1" = 607573,
    "22-2" = 1267133912086024,
    "24-1" = 97919997299495
  )
  if (!all(answers[sprintf("%d-%d", day, part)][[1]] == result))
    stop("Wrong answer for day ", day, " part ", part, call. = FALSE)
}

read_trench_map <- function(file) {
  lines <- readLines(file)
  delimiter <- grep("^$", lines)
  algo <- gsub("\\.", "0", gsub("#", "1", strsplit(lines[1], "")[[1]])) |> as.integer() |> as.logical()
  algo <- lines[1]
  image_string <- paste(lines[3:length(lines)], collapse = "") |>
    strsplit("") |>
    (\(x) gsub("\\.", "0", gsub("#", "1", x[[1]])))() |>
    as.integer() |>
    as.logical()
  image_string <- paste(lines[3:length(lines)], collapse = "") |> strsplit("")
  image <- matrix(image_string[[1]], nrow = sqrt(length(image_string[[1]])), byrow = TRUE)
  list(
    algo = algo,
    image = image
  )
}

char_to_bin <- function(x) {
  gsub("\\.", "0", gsub("#", "1", x)) |> paste(collapse = "")
}

expand_image <- function(image, inf_value) {
  nr <- nrow(image)
  nc <- ncol(image)
  col <- rep(inf_value, nr)
  row <- rep(inf_value, nc + 6)
  new_image <- rbind(row, row, row, cbind(col, col, col, image, col, col, col), row, row, row)
  colnames(new_image) <- rownames(new_image) <- NULL
  new_image
}

determine_output <- function(i, j, image, algo) {
  # generate indices of the square centered at the given position
  col_shift <- c(rep(-1, 3), c(rep(0, 3), c(rep(1, 3))))
  row_shift <- rep(c(-1, 0, 1), 3)
  square_indices <- matrix(c(i + col_shift, j + row_shift), ncol = 2)
  # get the values of pixels in the square and convert to the index into
  # the algorithm vector
  square <- image[square_indices]
  algo_index <- decimal(char_to_bin(square)) + 1 # 0-based indexing
  # extract the output pixel from the algorithm vector
  output_pixel <- substr(algo, algo_index, algo_index)
  output_pixel
}

plot_image <- function(m) image(t(apply(m, 2, rev)) == "#")

enhance_image_iter <- function(input, steps) {
  algo <- input$algo
  image <- input$image

  inf_value <- "."
  for (step_i in seq(1, steps)) {
    image <- expand_image(image, inf_value)
    # determine the binary square numbers of all surrounding pixels for each
    # position at once in a vectorized manner, convert them to decimal and
    # lookup the enhanced pixel value from the algorithm input
    image <- paste(
      shift(image, "downright", inf_value),
      shift(image, "down", inf_value),
      shift(image, "downleft", inf_value),
      shift(image, "left", inf_value),
      image,
      shift(image, "right", inf_value),
      shift(image, "upright", inf_value),
      shift(image, "up", inf_value),
      shift(image, "upleft", inf_value),
      sep = ""
    ) |>
      matrix(nrow = nrow(image)) |>
      apply(c(1, 2), function(bin) {
        i <- decimal(char_to_bin(bin)) + 1
        substr(input$algo, i, i)
      })

    # get the new value of the infinite canvas for the next iteration
    inf_value <- image[1, 1]
  }

  image
}

enhance_image_mapply <- function(input, steps) {
  algo <- input$algo
  image <- input$image

  inf_value <- "."
  for (step_i in seq(1, steps)) {
    image <- expand_image(image, inf_value)
    enhanced_image <- matrix(inf_value, nrow = nrow(image), ncol = ncol(image))
    for (i in seq(2, nrow(image) - 1)) {
      for (j in seq(2, ncol(image) - 1)) {
        # if (i == 4 && j == 4) browser()
        enhanced_image[i, j] <- determine_output(i, j, image, algo)
      }
    }
    inf_value <- enhanced_image[2, 2]
    enhanced_image[1, ] <- enhanced_image[nrow(image), ] <-
      enhanced_image[, 1] <- enhanced_image[, ncol(image)] <- inf_value

    image <- enhanced_image
  }

  image
}

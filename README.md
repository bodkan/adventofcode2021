# Advent of Code 2021 in R

This repository contains my solutions to the Advent of Code 2021 puzzles in R. The goal
is to solve every puzzle using nothing else but base R functions.

The "solutions as an R package" approach is modeled after [@tjmahr](https://github.com/tjmahr/adventofcode17) R package solution for Advent of Code
2017. Briefly, each script under `R/` contains the functions needed to solve the puzzles
on a given day. Unit tests under `tests/` verify that my solutions adhere to the
requirements given by each puzzle specification, using provided testing data. Finally,
`inst/` contains R scripts which solve the full puzzle using data stored under
`inst/extdata`.

# Advent of Code 2021 in R

This repository contains my solutions to the Advent of Code 2021 puzzles in R. The goal
is to solve every puzzle using nothing else but base R functions.

The "solutions as an R package" approach is modeled after [@tjmahr](https://github.com/tjmahr/adventofcode17) R package solution for Advent of Code
2017. Briefly, each script under `R/` contains the functions needed to solve the puzzles
on a given day. Unit tests under `tests/` verify that my solutions adhere to the
requirements given by each puzzle specification, using provided testing data. Finally,
`inst/` contains R scripts which solve the full puzzle using data stored under
`inst/extdata`.

**Why on Earth would you use R for this?**

Several reasons. First, R is my favourite programming language. Yes, I said it.

Second, I spend nearly all of my time doing data analysis and research&mdash;nearly everything I work with on a daily basis are (gigantic) tables, the usual output of my work are figures and statistical models. Advent of Code presents a very different class of problems that I rarely get to work with these days and it seemed like a fun challenge trying to solve them in R. Moreover, as an additional challenge and a character building experience I decided to restrict myself to only use features available in base R 4.x without any additional packages (again, something that is quite unusual when working with R).

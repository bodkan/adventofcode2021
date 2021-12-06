# Advent of Code 2021 in R

This repository contains my solutions to the Advent of Code 2021 puzzles in R. The goal is to solve every puzzle using nothing else but base R functions.

The "solutions as an R package" approach is modeled after [\@tjmahr](https://github.com/tjmahr/adventofcode17) R package solution for Advent of Code 2017. Briefly, each script under `R/` contains the functions needed to solve the puzzles on a given day. Unit tests under `tests/` verify that my solutions adhere to the requirements given by each puzzle specification, using provided testing data. Finally, `inst/` contains R scripts which solve the full puzzle using data stored under `inst/extdata`.

### Solutions

- [Day 1 puzzle](https://adventofcode.com/2021/day/1) - [my solution](R/day-01.R)
- [Day 2 puzzle](https://adventofcode.com/2021/day/2) - [my solution](R/day-02.R)
- [Day 3 puzzle](https://adventofcode.com/2021/day/3) - [my solution](R/day-03.R)
- [Day 4 puzzle](https://adventofcode.com/2021/day/4) - [my solution](R/day-04.R)
- [Day 5 puzzle](https://adventofcode.com/2021/day/5) - [my solution](R/day-05.R)
- [Day 6 puzzle](https://adventofcode.com/2021/day/6) - [my solution](R/day-06.R)

### Why on Earth would you use R for this?

Several reasons. First, R is my favourite programming language. Yes, I said it.

Second, I spend nearly all of my time doing data analysis and research—nearly everything I work with on a daily basis are (gigantic) tables, the usual output of my work are figures and statistical models. Advent of Code presents a very different class of problems that I rarely get to work with these days and it seemed like a fun challenge trying to solve them in R. Moreover, as an additional challenge and a character building experience I decided to restrict myself to only use features available in base R 4.x without any additional packages (again, something that is quite unusual when working with R).

### Personal stats

I'm clearly not speed-running this one. 😎 🏖 🍹 Mostly working on these puzzles during breaks throughout the day.

![image](https://user-images.githubusercontent.com/16516593/144813799-3856f09d-758b-44cf-9290-34f357c81c9d.png)

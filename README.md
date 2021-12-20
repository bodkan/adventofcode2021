# 🎄 Advent of Code 2021 in R 🎄

This repository contains my solutions to the [Advent of Code 2021](https://adventofcode.com/2021) in R. My goal is to solve every puzzle using nothing else but base R functions and _without any data frames_ or data frame manipulation functions (matrices, n-dimensional arrays and linear algebra operations are OK).

The "solutions as an R package" method is inspired by [\@tjmahr](https://github.com/tjmahr/adventofcode17)'s approach to Advent of Code 2017. Briefly, each script under `R/` contains the functions needed to solve the puzzle on a given day. Unit tests under `tests/` verify that my solutions adhere to the requirements given by each puzzle specification, using the tiny testing data on the Advent of Code website. Finally, `inst/` contains small standalone R scripts which solve the full puzzles stored in text files under `inst/extdata` and output the results to the terminal. All are executed by a master script in `inst/run-all.R`.

### Solutions

- [Day 1 puzzle](https://adventofcode.com/2021/day/1) - [my solution](R/day-01.R)
- [Day 2 puzzle](https://adventofcode.com/2021/day/2) - [my solution](R/day-02.R)
- [Day 3 puzzle](https://adventofcode.com/2021/day/3) - [my solution](R/day-03.R)
- [Day 4 puzzle](https://adventofcode.com/2021/day/4) - [my solution](R/day-04.R)
- [Day 5 puzzle](https://adventofcode.com/2021/day/5) - [my solution](R/day-05.R)
- [Day 6 puzzle](https://adventofcode.com/2021/day/6) - [my solution](R/day-06.R)
- [Day 7 puzzle](https://adventofcode.com/2021/day/7) - [my solution](R/day-07.R)
- [Day 8 puzzle](https://adventofcode.com/2021/day/8) - [my solution](R/day-08.R)
- [Day 9 puzzle](https://adventofcode.com/2021/day/9) - [my solution](R/day-09.R)
- [Day 10 puzzle](https://adventofcode.com/2021/day/10) - [my solution](R/day-10.R)
- [Day 11 puzzle](https://adventofcode.com/2021/day/11) - [my solution](R/day-11.R)
- [Day 12 puzzle](https://adventofcode.com/2021/day/12) - [my solution](R/day-12.R)
- [Day 13 puzzle](https://adventofcode.com/2021/day/13) - [my solution](R/day-13.R)
- [Day 14 puzzle](https://adventofcode.com/2021/day/14) - [my solution](R/day-14.R)
- [Day 15 puzzle](https://adventofcode.com/2021/day/15) - [my solution](R/day-15.R)
- [Day 16 puzzle](https://adventofcode.com/2021/day/16) - [my solution](R/day-16.R)
- [Day 17 puzzle](https://adventofcode.com/2021/day/17) - [my solution](R/day-17.R)
- [Day 18 puzzle](https://adventofcode.com/2021/day/18) - [my solution](R/day-18.R)
- [Day 20 puzzle](https://adventofcode.com/2021/day/20) - [my solution](R/day-20.R)

### Why on Earth would you use R for this?

Several reasons. First, R is my favourite programming language. There, I said it.

Second, I spend nearly all time at my job doing data analysis and research&mdash;nearly everything I work with on a daily basis are (gigantic) tables, the usual output of my work are figures and statistical models. Advent of Code presents a very different class of problems that I rarely get to work with these days and it seemed like a fun challenge trying to solve them in R. Moreover, as an additional challenge and a character building experience I decided to restrict myself to only use features available in base R 4.x without any additional packages and without using any data frames or functions that manipulate them (again, something that is very unusual when working with R).

### Personal stats

As you can see, I've been approaching this very casually. 😎 🏖 🍹 Working on this during breaks throughout the day, so there's no way I will ever score well. ¯\\\_(ツ)\_/¯ 

![image](https://user-images.githubusercontent.com/16516593/146600560-37c98eb6-d4ca-4f4d-9330-6bc4561a202c.png)

suppressMessages(devtools::load_all("."))

part1 <- part_1_game(10, 9)

print_result(day = 21, part = 1, part1)
check_answer(day = 21, part = 1, part1)

part2 <- part_2_game(10, 9)

print_result(day = 21, part = 2, part2)
check_answer(day = 21, part = 2, part2)

print_sep()

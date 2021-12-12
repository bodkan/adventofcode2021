test_file1 <- create_test_file("start-A
start-b
A-c
A-b
b-d
A-end
b-end")

test_file2 <- create_test_file("dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

test_file3 <- create_test_file("fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

test_that(test_name(day = 12, part = 1), {
  cond <- can_enter1
  expect_true(find_paths(test_file1, cond) == 10)
  expect_true(find_paths(test_file2, cond) == 19)
  expect_true(find_paths(test_file3, cond) == 226)
})

test_that(test_name(day = 12, part = 2), {
  cond <- can_enter2
  expect_true(find_paths(test_file1, cond) == 36)
  expect_true(find_paths(test_file2, cond) == 103)
  expect_true(find_paths(test_file3, cond) == 3509)
})

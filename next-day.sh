#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "A single argument with the day number must be provided"
    exit
fi

# session ID from Developer Tools/Network/Headers in Chrome
export SESSION="`cat SESSION`"

printf -v day "day-%02d" $1
input_path="inst/extdata/${day}.txt"
test_path="tests/testthat/test-${day}.R"
run_path="inst/run-${day}.R"

curl -s https://adventofcode.com/2021/day/$1/input --cookie "session=$SESSION" -o ${input_path}

if grep -Eq '404 Not Found|endpoint' $input_path; then
    echo "Puzzle for the given day ${1} not found"
    rm $input_path
    exit 1
else
    echo "Puzzle input downloaded and saved as ${input_path}"
fi

echo "-----"

if [[ ! -f $test_path ]]; then
    sed "s/__day__/${1}/g" inst/extdata/test-template.R > $test_path
    echo "Test file created at ${test_path}"
else
    echo "Test file already present at ${test_path}"
fi

echo "-----"

if grep -q "${day}" inst/run-all.R; then
    echo "Run code for the day already present in inst/run-all.R"
else
    echo "source(\"inst/run-${day}.R\", local = TRUE)" >> inst/run-all.R
fi

echo "-----"

if [[ ! -f $run_path ]]; then
    sed "s/__DAY__/${day}/g; s/__day__/${1}/g" inst/extdata/run-template.R \
      > $run_path
    echo "Run script created at ${run_path}"
else
    echo "Run script already present at ${run_path}"
fi

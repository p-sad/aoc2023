#!/usr/bin/env python3

import sys
import re


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    [times, distances] = [(int(num) for num in re.findall(r'[0-9]+', line)) for line in lines]
    ways_to_win = 1
    for time, distance in zip(times, distances):
        found_solutions = 0
        pivot = time // 2
        while pivot * (time - pivot) > distance:
            found_solutions += 1
            pivot -= 1
        ways_to_win *= found_solutions * 2 - (1 if time % 2 == 0 else 0)
    print(ways_to_win)


if __name__ == '__main__':
    main(sys.argv[-1])

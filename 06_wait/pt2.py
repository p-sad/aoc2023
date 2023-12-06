#!/usr/bin/env python3

import sys
import re


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    [time, distance] = [int(''.join(re.findall(r'[0-9]+', line))) for line in lines]
    # I think this is what a binary search might look like in a parallel universe
    pivot = time // 2
    step = pivot // 2
    while step > 0:
        new_pivot = pivot - step
        if new_pivot * (time - new_pivot) > distance:
            pivot = new_pivot
        else:
            step = step // 2
    ways_to_win = time + 1 - 2*pivot
    print(ways_to_win)


if __name__ == '__main__':
    main(sys.argv[-1])

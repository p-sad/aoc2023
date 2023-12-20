#!/usr/bin/env python3

import sys
from typing import Iterable


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    vertical_rows = ([lines[j][i] for j in range(len(lines))] for i in range(len(lines[0])))
    print(sum(get_weight(row) for row in vertical_rows))


def get_weight(row: list[str]):
    total = 0
    next_weight_added = len(row)
    for pos, tile in enumerate(row):
        if tile == 'O':
            total += next_weight_added
            next_weight_added -= 1
        elif tile == '#':
            next_weight_added = len(row) - pos - 1
    return total


if __name__ == '__main__':
    main(sys.argv[-1])

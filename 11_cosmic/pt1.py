#!/usr/bin/env python3.9

import sys


class Vector:
    def __init__(self, y, x) -> None:
        self.y = y
        self.x = x

    def distance_from(self, other: 'Vector'):
        return abs(self.y - other.y) + abs(self.x - other.x)


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    expanded = expand_vertically(expand_horizontally(lines))
    positions: list[Vector] = []
    for y, line in enumerate(expanded):
        for x, char in enumerate(line):
            if char == '#':
                positions.append(Vector(y, x))
    sum_of_distances = 0
    for i, position in enumerate(positions):
        for other in positions[i:]:
            sum_of_distances += position.distance_from(other)
    print(sum_of_distances)


def expand_horizontally(input: list[str]) -> list[str]:
    for i in reversed(range(len(input[0]))):
        if all(line[i] == '.' for line in input):
            input = [line[:i] + '.' + line[i:] for line in input]
    return input


def expand_vertically(input: list[str]) -> list[str]:
    for i in reversed(range(len(input))):
        if input[i] == '.' * len(input[i]):
            input.insert(i, input[i])
    return input


if __name__ == '__main__':
    main(sys.argv[-1])

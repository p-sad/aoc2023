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
    positions: list[Vector] = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char == '#':
                positions.append(Vector(y, x))
    apply_expansions(
        positions,
        find_horizontal_expansions(lines),
        find_vertical_expansions(lines),
    )
    sum_of_distances = 0
    for i, position in enumerate(positions):
        for other in positions[i:]:
            sum_of_distances += position.distance_from(other)
    print(sum_of_distances)


def find_horizontal_expansions(input: list[str]) -> list[int]:
    found = []
    for x in range(len(input[0])):
        if all(line[x] == '.' for line in input):
            found.append(x)
    return found


def find_vertical_expansions(input: list[str]) -> list[int]:
    found = []
    for y in range(len(input)):
        if input[y] == '.' * len(input[y]):
            found.append(y)
    return found


expansion_size = 1_000_000

def apply_expansions(positions: list[Vector], horizontal: list[int], vertical: list[int]):
    for x in reversed(horizontal):
        for position in positions:
            if position.x > x:
                position.x += (expansion_size - 1)
    for y in reversed(vertical):
        for position in positions:
            if position.y > y:
                position.y += (expansion_size - 1)


if __name__ == '__main__':
    main(sys.argv[-1])

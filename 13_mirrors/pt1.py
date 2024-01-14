#!/usr/bin/env python3

import sys
from typing import Iterable


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = (line.strip() for line in file)
        puzzles = get_puzzles(lines)
    sum_of_mirrors = 0
    for puzzle in puzzles:
        mirror = find_vertical_mirror(puzzle)
        if mirror is None:
            mirror = find_horizontal_mirror(puzzle) * 100
        sum_of_mirrors += mirror
    print(sum_of_mirrors)


def find_vertical_mirror(puzzle: list[str]) -> int | None:
    symmetry_axes = [
        axis for axis in range(1, len(puzzle[0]))
        if is_symmetry_axis(axis, puzzle[0])
    ]
    for i in range(1, len(puzzle)):
        symmetry_axes = [
            axis for axis in symmetry_axes
            if is_symmetry_axis(axis, puzzle[i])
        ]
    return symmetry_axes[0] if len(symmetry_axes) == 1 else None


def find_horizontal_mirror(puzzle: list[str]) -> int | None:
    def get_vertical_line(index: int):
        return [line[index] for line in puzzle]
    symmetry_axes = [
        axis for axis in range(1, len(puzzle))
        if is_symmetry_axis(axis, get_vertical_line(0))
    ]
    for i in range(1, len(puzzle[0])):
        symmetry_axes = [
            axis for axis in symmetry_axes
            if is_symmetry_axis(axis, get_vertical_line(i))
        ]
    return symmetry_axes[0] if len(symmetry_axes) == 1 else None


def is_symmetry_axis(index: int, line: str | list[str]) -> bool:
    left, right = index-1, index
    while left >= 0 and right < len(line):
        if line[left] != line[right]:
            return False
        left -= 1
        right += 1
    return True


def get_puzzles(lines: Iterable[str]) -> list[list[str]]:
    current_puzzle = []
    puzzles = []
    for line in lines:
        if len(line):
            current_puzzle.append(line)
            continue
        puzzles.append(current_puzzle)
        current_puzzle = list()
    if len(current_puzzle):
        puzzles.append(current_puzzle)
    return puzzles


if __name__ == '__main__':
    main(sys.argv[-1])

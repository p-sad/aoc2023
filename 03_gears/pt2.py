#!/usr/bin/env python3

import sys
import re
from typing import Iterable
from functools import reduce


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        schematic = [line.rstrip() for line in file]
    found_asterisks = [find_gears(line) for line in schematic]
    found_numbers = [find_numbers(line) for line in schematic]
    found_gears = []
    for i in range(len(schematic)):
        current_line = schematic[i]
        previous_line = schematic[i-1] if i>0 else None
        next_line = schematic[i+1] if i+1 < len(schematic) else None
        found_gears.append([
            pos for pos
            in found_asterisks[i]
            if is_gear(pos, previous_line, current_line, next_line)
        ])
    gear_ratios = []
    for i in range(len(found_gears)):
        lines = [schematic[j] for j in range(len(schematic)) if i-1 <= j <= i+1]
        number_lines = [found_numbers[j] for j in range(len(schematic)) if i-1 <= j <= i+1]
        gear_ratios += [get_ratio_for_gear(gear, lines, number_lines) for gear in found_gears[i]]
    print(sum(gear_ratios))


def find_gears(line: str):
    matches = re.finditer(r'\*', line)
    return [match.start() for match in matches]


def find_numbers(line: str):
    matches = re.finditer(r'[0-9]+', line)
    return [(match.start(), match.end()) for match in matches]


def is_gear(
    gear_index: int,
    previous_line: str | None,
    current_line: str,
    next_line: str | None,
):
    start_index = max(0, gear_index-1)
    end_index = min(gear_index+2, len(current_line)-1)
    found_occurences = 0
    for line in [
        previous_line[start_index:end_index] if previous_line else '...',
        current_line[start_index:end_index],
        next_line[start_index:end_index] if next_line else '...',
    ]:
        if line:
            found_occurences += len(re.findall(r'[0-9]+', line))
    return found_occurences == 2


def get_ratio_for_gear(gear_pos: int, lines: Iterable[str], found_numbers: Iterable[list[tuple[int, int]]]):
    applicable_numbers = []
    for line, num_line in zip(lines, found_numbers):
        for num_position in num_line:
            start, end = num_position
            if gear_pos-1 <= start <= gear_pos+1 or gear_pos-1 <= end-1 <= gear_pos+1 :
                applicable_numbers.append(int(line[start:end]))
    return reduce(lambda a,b: a*b, applicable_numbers)


if __name__ == '__main__':
    main(sys.argv[-1])

#!/usr/bin/env python3

import sys
import re


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        schematic = [line.rstrip() for line in file]
    found_numbers = [find_numbers(line) for line in schematic]
    part_numbers = []
    for i in range(len(found_numbers)):
        current_line = schematic[i]
        previous_line = schematic[i-1] if i>0 else None
        next_line = schematic[i+1] if i+1 < len(schematic) else None
        part_numbers.append([
            current_line[start:end]
            for start, end in found_numbers[i]
            if has_neighboring_symbols(
                (start, end),
                previous_line,
                current_line,
                next_line,
            )
        ])
    print(sum((int(num) for line in part_numbers for num in line)))


def find_numbers(line: str):
    matches = re.finditer(r'[0-9]+', line)
    return [(match.start(), match.end()) for match in matches]


def has_neighboring_symbols(
    match: tuple[int, int],
    previous_line: str | None,
    current_line: str,
    next_line: str | None,
):
    start, end = match
    symbol = re.compile(r'[^0-9\.]')
    start_index = max(0, start-1)
    end_index = min(end+1, len(current_line)-1)
    for line in [previous_line, current_line, next_line]:
        if line:
            if symbol.search(line[start_index:end_index]):
                return True
    return False


if __name__ == '__main__':
    main(sys.argv[-1])

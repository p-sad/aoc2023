#!/usr/bin/env python3

# intentionally(?) bad python code
# "we can have an energy drink if we're tired, or elif it's cold outside we can have some tea",
# "global gravy; can you pass me the gravy?"
#       - statements dreamed up by the utterly deranged

import sys
import re


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    for line in lines:
        parse_line(line)
    global seeds
    global all_maps
    results: list[int] = []
    for value in seeds:
        for ranges_map in all_maps:
            value = map_value(value, ranges_map)
        results.append(value)
    print(min(results))


def parse_line(line: str):
    global seeds
    global current_map
    global next_map
    if line.startswith('seeds'):
        seeds += [int(num) for num in line.split(' ')[1:]]
    elif line.find('map') >= 0:
        current_map = next(next_map)
    elif re.match(r'[0-9]+ [0-9]+ [0-9]+', line):
        [dest, src, range] = [int(n) for n in line.split(' ')]
        current_map.append(Range(src, dest, range))


class Range:
    def __init__(self, source: int, dest: int, range: int) -> None:
        self.source = source
        self.dest = dest
        self.range = range

    def is_in_range(self, input: int):
        return self.source <= input < (self.source + self.range)

    def map_to_dest(self, input: int):
        return input - self.source + self.dest


def map_value(value: int, ranges: list[Range]) -> int:
    for range in ranges:
        if range.is_in_range(value):
            return range.map_to_dest(value)
    return value


seeds: list[int] = []
all_maps: list[list[Range]] = [list() for _ in range(7)]
next_map = (m for m in all_maps)
current_map: list[Range] = None


if __name__ == '__main__':
    main(sys.argv[-1])


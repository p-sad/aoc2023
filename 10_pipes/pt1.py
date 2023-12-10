#!/usr/bin/env python3.9

import sys
from math import ceil


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    s_pos = find_s(lines)
    previous_pos = s_pos
    current_seg, current_pos = find_adjacent_pipe(s_pos, lines)
    length = 0
    while current_seg != 'S':
        current_pos, previous_pos = next_segment(current_pos, current_seg, previous_pos), current_pos
        current_seg = lines[current_pos[0]][current_pos[1]]
        length += 1
    print(ceil(length / 2))


def find_s(lines: list[str]):
    y = next(i for i, line in enumerate(lines) if 'S' in line)
    x = lines[y].index('S')
    return y, x


Position = tuple[int, int]
#        y   x
left  = (0, -1)
right = (0,  1)
up    = (-1, 0)
down  = (1,  0)
pipe_segments = {
    '-': (left, right),
    '|': (up, down),
    'F': (down, right),
    '7': (left, down),
    'L': (up, right),
    'J': (up, left),
}


def connects_to(pos: Position, seg: str, segment_pos: Position):
    pos_y, pos_x = pos
    seg_y, seg_x = segment_pos
    segment = pipe_segments[seg]
    for y, x in segment:
        if seg_y + y == pos_y and seg_x + x == pos_x:
            return True
    return False


def find_adjacent_pipe(s_pos: Position, lines: list[str]):
    sy, sx = s_pos
    for y, x in [up, down, left, right]:
        segment = lines[sy + y][sx + x]
        if segment in pipe_segments:
            segment_pos = (sy+y, sx+x)
            if connects_to(s_pos, segment, segment_pos):
                return segment, segment_pos


def next_segment(current_pos: Position, current_segment: str, previous_pos):
    segment = pipe_segments[current_segment]
    curr_y, curr_x = current_pos
    prev_y, prev_x = previous_pos
    for y, x in segment:
        next_y, next_x = curr_y + y, curr_x + x
        if next_y != prev_y or next_x != prev_x:
            return next_y, next_x


if __name__ == '__main__':
    main(sys.argv[-1])


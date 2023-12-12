#!/usr/bin/env python3.9

from enum import Enum
import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        chart = [line.strip() for line in file]
    s_pos = find_s(chart)
    adjacent_segments = find_adjacent_segments(s_pos, chart)
    segments = [Segment(s_pos, (adjacent_segments[0].position - s_pos, adjacent_segments[1].position - s_pos))]
    previous_segment = segments[0]
    current_segment = adjacent_segments[0]
    overall_turn = 0
    while current_segment.position != s_pos:
        segments.append(current_segment)
        next_pos, turn = current_segment.get_next_position_and_turn(previous_segment)
        if turn != Turn.Straight:
            overall_turn += -1 if turn == Turn.Left else 1
        previous_segment = current_segment
        current_segment = Segment.from_symbol(next_pos, get_symbol_at(next_pos, chart))
    print('Part 1 solution:', len(segments) // 2)
    # turn it into a left-handed track
    if overall_turn > 0:
        segments = list(reversed(segments))
    # and now for the most horrible flood fill algorithm
    flood_filled = set()
    fill_candidates: list[Vector] = []
    segments_positions = [segment.position for segment in segments]
    for i in range(len(segments)):
        fill_candidates += segments[(i+1)%len(segments)].get_flood_fill_positions(segments[i])
    for candidate in fill_candidates:
        if candidate not in segments_positions:
            flood_filled.add(candidate)
    filled_this_iteration = len(flood_filled)
    last_iteration_filled = flood_filled
    current_iteration_filled = set()
    while filled_this_iteration != 0:
        for pos in last_iteration_filled:
            candidates = bloom(pos)
            for candidate in candidates:
                if candidate not in segments_positions and candidate not in flood_filled:
                    current_iteration_filled.add(candidate)
        filled_this_iteration = len(current_iteration_filled)
        flood_filled |= current_iteration_filled
        last_iteration_filled = current_iteration_filled
        current_iteration_filled = set()
    print('Part 2 solution:', len(flood_filled))


def find_s(lines: list[str]):
    y = next(i for i, line in enumerate(lines) if 'S' in line)
    x = lines[y].index('S')
    return Vector(x, y)


def get_symbol_at(pos: 'Vector', chart: list[str]):
    return chart[pos.y][pos.x]


def find_adjacent_segments(s_pos: 'Vector', chart: list[str]):
    positions = (s_pos + direction for direction in [up, down, left, right])
    segments = (
        Segment.from_symbol(position, get_symbol_at(position, chart))
        for position in positions
        if get_symbol_at(position, chart) != '.'
    )
    return [segment for segment in segments if segment.is_connected_to(s_pos)]


class Vector:
    def __init__(self, x, y) -> None:
        self.y = y
        self.x = x

    def __add__(self, other: 'Vector') -> 'Vector':
        return Vector(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Vector') -> 'Vector':
        return Vector(self.x - other.x, self.y - other.y)

    def __eq__(self, other: 'Vector') -> bool:
        return self.y == other.y and self.x == other.x

    def __ne__(self, other: 'Vector') -> bool:
        return not self.__eq__(other)

    def __hash__(self) -> int:
        return hash(self.x | (self.y << 16))


class Turn(Enum):
    Straight = 0
    Left = 1
    Right = 2


class Segment:
    def __init__(self, position: Vector, connections: tuple[Vector, Vector]) -> None:
        self.connections = connections
        self.position = Vector(position.x, position.y)

    @staticmethod
    def from_symbol(position: Vector, symbol: str):
        return Segment(position, pipe_connections[symbol])

    def get_next_position_and_turn(self, previous: 'Segment') -> Vector:
        for i, connection in enumerate(self.connections):
            if previous.position != (self.position + connection):
                prev_connection = self.connections[1-i]
                if connection.x == prev_connection.x or connection.y == prev_connection.y:
                    turn = Turn.Straight
                else:
                    turn = Turn.Left if rotate_left(connection) == prev_connection else Turn.Right
                return self.position + connection, turn

    def is_connected_to(self, position: Vector):
        for connection in self.connections:
            if self.position + connection == position:
                return True
        return False

    def get_flood_fill_positions(self, previous: 'Segment') -> Vector:
        next_pos, _ = self.get_next_position_and_turn(previous)
        connection = next_pos - self.position
        # left and forward-left relative to heading
        return [self.position + rotate_left(connection), self.position + connection + rotate_left(connection)]


def rotate_left(v: Vector):
    return Vector(-v.y, v.x)


def rotate_right(v: Vector):
    return Vector(v.y, -v.x)


def bloom(v: Vector):
    return [
        v + left,
        v + right,
        v + up,
        v + down,
        v + up + left,
        v + up + right,
        v + down + left,
        v + down + right,
    ]


left  = Vector(-1, 0)
right = Vector(1,  0)
up    = Vector(0, -1)
down  = Vector(0,  1)
pipe_connections = {
    '-': (left, right),
    '|': (up, down),
    'F': (down, right),
    '7': (left, down),
    'L': (up, right),
    'J': (up, left),
    'S': (),
}


if __name__ == '__main__':
    main(sys.argv[-1])


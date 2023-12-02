#!/usr/bin/env python3

import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        games = [line_to_game(line.strip()) for line in file]
    result = 0
    for i in range(len(games)):
        if is_game_possible(games[i]):
            result += i+1
    print(result)


def line_to_game(line: str) -> list[list[int]]:
    [game_number, rounds] = line.split(':')
    rounds = rounds.split(';')
    return [round_to_rgb(round) for round in rounds]


def round_to_rgb(round: str):
    rgb = [0, 0, 0]
    values = round.split(',')
    for value in values:
        [number, color] = value.strip().split(' ')
        rgb[color_to_rgb[color]] = int(number)
    return rgb


def is_game_possible(game: list[list[int]]) -> bool:
    for round in game:
        for i in range(3):
            if round[i] > expected_cubes[i]:
                return False
    return True


color_to_rgb = {
    'red': 0,
    'green': 1,
    'blue': 2,
}
expected_cubes = [12, 13, 14]


if __name__ == '__main__':
    main(sys.argv[-1])

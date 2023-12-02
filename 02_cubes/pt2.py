#!/usr/bin/env python3

import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        games = [line_to_game(line.strip()) for line in file]
    min_cubes = (game_min_cubes_power(game) for game in games)
    print(sum(min_cubes))


Game = list[list[int]]


def line_to_game(line: str) -> Game:
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


def game_min_cubes_power(game: Game):
    result = [0, 0, 0]
    for round in game:
        for i in range(3):
            if result[i] < round[i]:
                result[i] = round[i]
    return result[0] * result[1] * result[2]


color_to_rgb = {
    'red': 0,
    'green': 1,
    'blue': 2,
}


if __name__ == '__main__':
    main(sys.argv[-1])

#!/usr/bin/env python3

import sys
import re
from sympy import nextprime


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    nodes = get_nodes(lines[2:])
    part_1(generate_directions(lines[0]), nodes)
    part_2(lines[0], nodes)


def part_1(directions, nodes):
    current_node = nodes['AAA']
    steps = 0
    while current_node.name != 'ZZZ':
        steps += 1
        current_node = current_node.navigate(next(directions))
    print(steps)


def generate_directions(directions: str):
    i = 0
    while True:
        yield directions[i]
        i = (i + 1) % len(directions)


class Node:
    def __init__(self, name: str) -> None:
        self.name = name


    def set_directions(self, left, right):
        self.left = left
        self.right = right


    def navigate(self, direction: str):
        return self.left if direction == 'L' else self.right


def get_nodes(lines: list[str]):
    line_inputs = [re.findall(r'[0-9A-Z]{3}', line) for line in lines]
    nodes = { input[0]: Node(input[0]) for input in line_inputs }
    for key, left, right in line_inputs:
        nodes[key].set_directions(nodes[left], nodes[right])
    return nodes


def part_2(directions_line: str, nodes):
    starting_nodes = [nodes[key] for key in nodes if key.endswith('A')]
    laps_steps = []
    for starting_node in starting_nodes:
        directions = generate_directions(directions_line)
        initial_steps = 0
        current_node = starting_node
        while not current_node.name.endswith('Z'):
            initial_steps += 1
            current_node = current_node.navigate(next(directions))
        laps_steps.append(initial_steps)
    # upon examining the output, the initial steps from ..A to ..Z are always equal to lap steps
    # - seems like a cop out but simplifies the solution
    print(find_least_common_multiple(laps_steps))


def find_least_common_multiple(laps: list[int]):
    divisor = 2
    factors = []
    table = laps.copy()
    while not all(n == 1 for n in table):
        any_divided = False
        for i in range(len(table)):
            if table[i] % divisor == 0:
                any_divided = True
                table[i] = table[i] // divisor
        if any_divided:
            factors.append(divisor)
        else:
            divisor = nextprime(divisor)
    result = 1
    for factor in factors:
        result *= factor
    return result


if __name__ == '__main__':
    main(sys.argv[-1])

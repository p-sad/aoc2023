#!/usr/bin/env python3

import sys
import re


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    directions = generate_directions(lines[0])
    nodes = get_nodes(lines[2:])
    current_node = nodes['AAA']
    steps = 0
    while current_node.name != 'ZZZ':
        steps += 1
        current_node = current_node.left if next(directions) == 'L' else current_node.right
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


def get_nodes(lines: list[str]):
    line_inputs = [re.findall(r'[A-Z]{3}', line) for line in lines]
    nodes = { input[0]: Node(input[0]) for input in line_inputs }
    for key, left, right in line_inputs:
        nodes[key].set_directions(nodes[left], nodes[right])
    return nodes


if __name__ == '__main__':
    main(sys.argv[-1])

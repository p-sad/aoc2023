#!/usr/bin/env python3.9

import re
import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.rstrip() for line in file]
    print(sum(find_correct_combinations(line) for line in lines))


def find_correct_combinations(input: str):
    [row, groups] = input.split()
    group_numbers = [int(num) for num in groups.split(',')]
    hashes_to_add = sum(group_numbers) - row.count('#')
    unknown_spaces = row.count('?')
    combinations = all_combinations(unknown_spaces, hashes_to_add)
    questions_pattern = re.compile(r'\?+')
    substitution_lengths = [m.end() - m.start() for m in questions_pattern.finditer(row)]
    intact_parts = questions_pattern.split(row)
    dots = r'\.+'
    any_dots = r'\.*'
    correct_permutation = re.compile(
        any_dots + dots.join(get_hash_patterns(group_numbers)) + any_dots
    )
    parts_to_join = [''] * (len(substitution_lengths) + len(intact_parts))
    matching_combinations = 0
    for combination in combinations:
        parts_to_join[::2] = intact_parts
        parts_to_join[1::2] = split_substitutions(combination, substitution_lengths)
        substituted = ''.join(parts_to_join)
        if correct_permutation.fullmatch(substituted):
            matching_combinations += 1
    return matching_combinations


def all_combinations(length: int, hash_symbols: int, depth=1) -> list[str]:
    if hash_symbols == 0:
        return [length * '.']
    combinations = []
    new_length = length - 1
    new_hashes = hash_symbols - 1
    while new_length >= new_hashes:
        prefix = '.' * (length - new_length - 1) + '#'
        combinations += [prefix + c for c in all_combinations(new_length, new_hashes, depth=depth+1)]
        new_length -= 1
    return combinations


def split_substitutions(combination: str, lengths: list[int]):
    substitutions = []
    start = 0
    for length in lengths:
        substitutions.append(combination[start : start+length])
        start += length
    return substitutions


def get_hash_patterns(group_numbers: list[int]):
    return (f'#{"{"}{num}{"}"}' for num in group_numbers)


if __name__ == '__main__':
    main(sys.argv[-1])

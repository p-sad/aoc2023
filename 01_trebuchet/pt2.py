#!/usr/bin/env python3

import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        found_tokens = (find_tokens(line.rstrip()) for line in file)
        first_and_last = ((line[0], line[-1]) for line in found_tokens)
        as_numbers = (int(as_digit(tens) + as_digit(ones)) for tens, ones in first_and_last)
        print(sum(as_numbers))


def find_tokens(line: str) -> list[str]:
    found_tokens = []
    for i in range(len(line)):
        remaining_length = len(line) - i
        for token in tokens:
            if len(token) > remaining_length:
                continue
            if line[i : i+len(token)] == token:
                found_tokens.append(token)
    return found_tokens


digits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
words = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']
tokens = sorted(digits + words, key=len)


def as_digit(token: str):
    if token not in words: return token
    return digits[words.index(token)]


if __name__ == '__main__':
    main(sys.argv[-1])

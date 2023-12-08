#!/usr/bin/env python3

import sys


def main(filename: str):
    with open(filename, 'r', encoding='utf-8') as file:
        lines = [line.strip() for line in file]
    rounds = [line.split() for line in lines]
    print(get_part_result(rounds, get_part1_score))
    print(get_part_result(rounds, get_part2_score))


def get_part_result(rounds: tuple[str, str], get_score: callable):
    score_to_bid = [get_score(hand, bid) for hand, bid in rounds]
    score_to_bid.sort(key=lambda s:s[0])
    result = 0
    for i, (_, bid) in enumerate(score_to_bid):
        result += bid * (i+1)
    return result


def get_part1_score(hand: str, bid: str) -> tuple[int, int]:
    strength = get_hand_strength(find_occurences(hand))
    score = (strength << 22) + get_high_card_score(hand, cards)
    return (score, int(bid))


def get_part2_score(hand: str, bid: str) -> tuple[int, int]:
    strength = get_hand_strength(substitute_jokers(find_occurences(hand)))
    score = (strength << 22) + get_high_card_score(hand, cards_with_jokers)
    return (score, int(bid))


def find_occurences(hand: str) -> dict[str, int]:
    card_occurences = {}
    for card in hand:
        if card in card_occurences:
            card_occurences[card] += 1
        else:
            card_occurences[card] = 1
    return card_occurences


def substitute_jokers(card_occurences: dict[str, int]):
    if 'J' in card_occurences and card_occurences['J'] != 5:
        jokers = card_occurences.pop('J')
        key, _ = sorted(card_occurences.items(), key=lambda i:i[1]).pop()
        card_occurences[key] += jokers
    return card_occurences


def get_hand_strength(occurences: dict[str, int]):
    card_counts = [c for c in occurences.values()]
    if card_counts[0] == 5:
        return 7
    if 4 in card_counts:
        return 6
    if 3 in card_counts:
        if 2 in card_counts:
            return 5
        return 4
    if 2 in card_counts:
        if card_counts.count(2) == 2:
            return 3
        return 2
    return 1


def get_high_card_score(hand: str, cards: list[str]):
    score = 0
    for i, card in enumerate(hand):
        score += (len(cards) - cards.index(card)) << ((4-i)*4)
    return score


cards = [*'AKQJT98765432']
cards_with_jokers = [*'AKQT98765432J']


if __name__ == '__main__':
    main(sys.argv[-1])

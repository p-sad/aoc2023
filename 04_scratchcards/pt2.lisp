#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun strings_to_ints (l)
;; (str) => (int)
  (if (endp l)
    nil
    (if (string= "" (car l))
      (strings_to_ints (cdr l))
      (cons
        (parse-integer (car l))
        (strings_to_ints (cdr l))))))

(defun string_to_list_of_ints (str)
;; str => (int)
  (strings_to_ints (uiop:split-string str :separator " ")))

(defun to_card (line)
;; str => (amount:int winning_numbers:(int) your_numbers:(int))
  (cons
    1
    (mapcar
      'string_to_list_of_ints
      (uiop:split-string line :separator "|"))))

(defun parse_line (line)
;; str => card
  (to_card (second (uiop:split-string line :separator ":" ))))

(defun count_points (winning_numbers numbers)
;; ((int)(int)) => int
  (if (endp numbers)
    0
    (+
      (if (member (car numbers) winning_numbers) 1 0)
      (count_points winning_numbers (cdr numbers)))))

(defun get_points (card)
;; card => int
  (count_points (second card) (third card)))

(defun increase_number_of_card_instances (card amount)
;; card => card
  (cons (+ amount (car card)) (cdr card)))

(defun increase_number_of_subsequent_cards (cards &key times amount)
;; (card) => (card)
  (if (= times 0)
    cards
    (cons
      (increase_number_of_card_instances (car cards) amount)
      (increase_number_of_subsequent_cards
        (cdr cards)
        :times (- times 1)
        :amount amount))))

(defun count_each_card (cards)
;; (card) => (int)
  (if (endp cards)
    nil
    (cons
      (car (car cards))
      (count_each_card
        (increase_number_of_subsequent_cards
          (cdr cards)
          :times (get_points (car cards))
          :amount (car (car cards)))))))

(defun sum_card_counts (cards)
  (apply '+ (count_each_card cards)))

(write-line
  (format nil "~a"
    (sum_card_counts
      (mapcar 'parse_line
        (uiop:read-file-lines (car (uiop:command-line-arguments)))))))

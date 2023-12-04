#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun to_numbers (l)
;; (str) => (int)
  (if (endp l)
    nil
    (if (string= "" (car l))
      (to_numbers (cdr l))
      (cons
        (parse-integer (car l))
        (to_numbers (cdr l))))))

(defun to_list_of_numbers (str)
;; str => (int)
  (to_numbers (uiop:split-string str :separator " ")))

(defun to_card (line)
;; str => (amount:int winning_numbers:(int) your_numbers:(int))
  (cons
    1
    (mapcar
      'to_list_of_numbers
      (uiop:split-string line :separator "|"))))

(defun parse_line (line)
;; str => card
  (to_card (second (uiop:split-string line :separator ":" ))))

(defun accumulate_points (winning_numbers numbers)
;; ((int)(int)) => int
  (if (endp numbers)
    0
    (+
      (if (member (car numbers) winning_numbers) 1 0)
      (accumulate_points winning_numbers (cdr numbers)))))

(defun get_points (card)
;; card => int
  (accumulate_points (second card) (third card)))

(defun increment_card (card amount)
;; card => card
  (cons (+ amount (car card)) (cdr card)))

(defun increment_subsequent_cards (cards &key times amount)
;; (card) => (card)
  (if (= times 0)
    cards
    (cons
      (increment_card (car cards) amount)
      (increment_subsequent_cards
        (cdr cards)
        :times (- times 1)
        :amount amount))))

(defun get_number_of_each_card (cards)
;; (card) => (int)
  (if (endp cards)
    nil
    (cons
      (car (car cards))
      (get_number_of_each_card
        (increment_subsequent_cards
          (cdr cards)
          :times (get_points (car cards))
          :amount (car (car cards)))))))

(defun sum_cards (cards)
  (apply '+ (get_number_of_each_card cards)))

(write-line
  (format nil "~a"
    (sum_cards
      (mapcar 'parse_line
        (uiop:read-file-lines (car (uiop:command-line-arguments)))))))

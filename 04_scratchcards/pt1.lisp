#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun to_numbers (l)
  (if (endp l)
    nil
    (if (string= "" (car l))
      (to_numbers (cdr l))
      (cons
        (parse-integer (car l))
        (to_numbers (cdr l))))))

(defun to_list_of_numbers (str)
  (to_numbers (uiop:split-string str :separator " ")))

(defun to_card (line)
  (mapcar
    'to_list_of_numbers
    (uiop:split-string line :separator "|")))

(defun parse_line (line)
  (to_card (second (uiop:split-string line :separator ":" ))))

(defun accumulate_points (winning_numbers numbers)
  (if (endp numbers)
    0
    (+
      (if (member (car numbers) winning_numbers) 1 0)
      (accumulate_points winning_numbers (cdr numbers)))))

(defun get_points (card)
  (ash 1 (- (accumulate_points (car card) (cadr card)) 1)))

(defun sum_points (cards)
  (apply '+
    (mapcar 'get_points cards)))

(write-line
  (format nil "~a"
    (sum_points
      (mapcar 'parse_line
        (uiop:read-file-lines (car (uiop:command-line-arguments)))))))

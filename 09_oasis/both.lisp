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

(defun get_differences (numbers)
;; list(int) => list(int)
  (if (endp (cdr numbers))
    nil
    (cons
      (- (cadr numbers) (car numbers))
      (get_differences (cdr numbers)))))

(defun is_zero (a) (= 0 a)) ;; int => bool

(defun get_lists_of_differences (numbers)
;; list(int) => list(list(int))
  (let ((differences (get_differences numbers)))
    (if (every 'is_zero differences)
      (list differences)
      (cons
        differences
        (get_lists_of_differences differences)))))

(defun get_sequence_of_differences (numbers)
  (cons numbers (get_lists_of_differences numbers)))

(defun carlast (l) (car (last l))) ;; list(obj) => obj

(defun find_next_item (sequences)
;; list(list(int)) => int
  (if (endp (cdr sequences))
    (carlast (car sequences))
    (+
      (carlast (car sequences))
      (find_next_item (cdr sequences)))))

(defun find_previous_item (sequences)
;; list(list(int)) => int
  (if (endp (cdr sequences))
    (car (car sequences))
    (-
      (car (car sequences))
      (find_previous_item (cdr sequences)))))

(defun print_result (direction)
  (write-line
    (format nil "~a"
      (apply '+
        (mapcar direction
          (mapcar 'get_sequence_of_differences
            (mapcar 'to_list_of_numbers
              (uiop:read-file-lines (car (uiop:command-line-arguments))))))))))

(print_result #'find_next_item)
(print_result #'find_previous_item)

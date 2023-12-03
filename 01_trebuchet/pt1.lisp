#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun split-line (line) (loop for char across line collect char))

(defun get-digits () (split-line "123456789"))

(defun find-digit (line &key from-end)
  (find-if (lambda (it) (member it (get-digits))) (split-line line) :from-end from-end))

(defun first-and-last-digit (line)
  (list
    (find-digit line :from-end nil)
    (find-digit line :from-end t)))

(defun sum-all (lines)
  (apply '+
    (mapcar
      (lambda (line)
        (parse-integer (concatenate 'string (first-and-last-digit line))))
      lines)))

(write-line
  (format nil "~a"
    (sum-all
      (uiop:read-file-lines (car (uiop:command-line-arguments))))))

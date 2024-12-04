;;; 04.el --- advent 2024 day 4  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/4

(require 'aoc2024)

(defun split (str)
  (string-split str "" t))

(defun chars-left (x y)
  (list (list x y)
        (list x (- y 1))
        (list x (- y 2))
        (list x (- y 3))))

(defun chars-up-left (x y)
  (list (list x y)
        (list (+ x 1) (- y 1))
        (list (+ x 2) (- y 2))
        (list (+ x 3) (- y 3))))

(defun chars-down-left (x y)
  (list (list x y)
        (list (- x 1) (- y 1))
        (list (- x 2) (- y 2))
        (list (- x 3) (- y 3))))

(defun chars-right (x y)
  (list (list x y)
        (list x (+ y 1))
        (list x (+ y 2))
        (list x (+ y 3))))

(defun chars-up-right (x y)
  (list (list x y)
        (list (+ x 1) (+ y 1))
        (list (+ x 2) (+ y 2))
        (list (+ x 3) (+ y 3))))

(defun chars-down-right (x y)
  (list (list x y)
        (list (- x 1) (+ y 1))
        (list (- x 2) (+ y 2))
        (list (- x 3) (+ y 3))))

(defun chars-up (x y)
  (list (list x y)
        (list (+ x 1) y)
        (list (+ x 2) y)
        (list (+ x 3) y)))

(defun chars-down (x y)
  (list (list x y)
        (list (- x 1) y)
        (list (- x 2) y)
        (list (- x 3) y)))

(defun xmas-p (lst positions)
  (equal "XMAS"
         (cl-loop for (x y) in positions
                  if (< x 0) return nil
                  if (< y 0) return nil
                  for val = (nth y (nth x lst))
                  if (eq val nil) return nil
                  concat val)))

(defun truep (val)
  (eq val t))

(defun check-pos (lst x y)
  (if (not (equal "X" (nth y (nth x lst)))) 0
    (cl-count-if 'truep (list (xmas-p lst (chars-left x y))
                              (xmas-p lst (chars-up-left x y))
                              (xmas-p lst (chars-up x y))
                              (xmas-p lst (chars-up-right x y))
                              (xmas-p lst (chars-right x y))
                              (xmas-p lst (chars-down-right x y))
                              (xmas-p lst (chars-down x y))
                              (xmas-p lst (chars-down-left x y))))))

(defun chars-tl-br (x y)
  (list (list (- x 1) (- y 1))
        (list x y)
        (list (+ x 1) (+ y 1))))

(defun chars-br-tl (x y)
  (list (list (+ x 1) (+ y 1))
        (list x y)
        (list (- x 1) (- y 1))))

(defun chars-bl-tr (x y)
  (list (list (+ x 1) (- y 1))
        (list x y)
        (list (- x 1) (+ y 1))))

(defun chars-tr-bl (x y)
  (list (list (- x 1) (+ y 1))
        (list x y)
        (list (+ x 1) (- y 1))))

(defun mas-p (lst positions)
  (equal "MAS"
         (cl-loop for (x y) in positions
                  if (< x 0) return nil
                  if (< y 0) return nil
                  for val = (nth y (nth x lst))
                  if (eq val nil) return nil
                  concat val)))

(defun check-pos-v2 (lst x y)
  (if (not (equal "A" (nth y (nth x lst)))) nil
    (= 2 (cl-count-if 'truep (list (mas-p lst (chars-tl-br x y))
                                   (mas-p lst (chars-br-tl x y))
                                   (mas-p lst (chars-bl-tr x y))
                                   (mas-p lst (chars-tr-bl x y)))))))

(defun count-occurrances (lst)
  (cl-loop for x from 0 to (- (length lst) 1)
           sum (cl-loop for y from 0 to (- (length (nth 0 lst)) 1)
                        sum (check-pos lst x y))))

(defun count-occurrances-v2 (lst)
  (cl-loop for x from 0 to (- (length lst) 1)
           sum (cl-loop for y from 0 to (- (length (nth 0 lst)) 1)
                        count (check-pos-v2 lst x y))))

(defun 2024-04-part1 (input)
  (->> (mapcar 'split input)
       (count-occurrances)))

(defun 2024-04-part2 (input)
  (->> (mapcar 'split input)
       (count-occurrances-v2)))

(defconst testfile (expand-file-name "input/04.test.txt"))
(defconst inputfile (expand-file-name "input/04.input.txt"))

(defcheck 2024-04-part1 testfile 18)
(defcheck 2024-04-part1 inputfile 2543)
(defcheck 2024-04-part2 testfile 9)
(defcheck 2024-04-part2 inputfile 1930)

(solve "2024-04")

;;; 06.el --- advent 2024 day 6  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/6

(require 'aoc2024)

(defun find-guard (map)
  (cl-loop for x from 0 to (- (length map) 1)
           with found = nil
           do (cl-loop for y from 0 to (- (length (nth x map)) 1)
                       if (equal "^" (nth y (nth x map)))
                       do (setq found (cons (list x y) "^"))
                       and return nil)
           if found return found))

(defun guard-out-of-bounds (guard map)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((>= x (length map)) t)
          ((>= y (length (nth x map))) t)
          ((< x 0) t)
          ((< y 0) t))))

(defun guard-forward (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (cons (list (- x 1) y) dir))
          ((equal dir ">")
           (cons (list x (+ y 1)) dir))
          ((equal dir "v")
           (cons (list (+ x 1) y) dir))
          ((equal dir "<")
           (cons (list x (- y 1)) dir)))))

(defun guard-turn (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (cons (list x (+ y 1)) ">"))
          ((equal dir ">")
           (cons (list (+ x 1) y) "v"))
          ((equal dir "v")
           (cons (list x (- y 1)) "<"))
          ((equal dir "<")
           (cons (list (- x 1) y) "^")))))

(defun guard-peek (guard map)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (nth y (nth (- x 1) map)))
          ((equal dir ">")
           (nth (+ y 1) (nth x map)))
          ((equal dir "v")
           (nth y (nth (+ x 1) map)))
          ((equal dir "<")
           (nth (- y 1) (nth x map))))))

(defun guard-next (guard map)
  (if (equal "#" (guard-peek guard map))
      (guard-turn guard)
    (guard-forward guard)))

(defun track-guard-path (map)
  (cl-loop while t
           with guard = (find-guard map)
           with path = nil
           if (guard-out-of-bounds guard map) return path
           else do (push (car guard) path)
           and do (setq guard (guard-next guard map))))

(defun 2024-06-part1 (input)
  (-<>> (mapcar 'split input)
       (track-guard-path)
       (cl-remove-duplicates <> :test 'equal)
       (length)))

(defconst testfile (expand-file-name "input/06.test.txt"))
(defconst inputfile (expand-file-name "input/06.input.txt"))

(defcheck 2024-06-part1 testfile 41)
(defcheck 2024-06-part1 inputfile 5461)
;; (defcheck 2024-06-part2 testfile 123)
;; (defcheck 2024-06-part2 inputfile 4884)

(solve "2024-06")

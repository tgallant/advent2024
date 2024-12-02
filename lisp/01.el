;;; 01.el --- advent 2024 day 1  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/1

(require 'aoc2024)

(defun split (str)
  (->> (split-string str " " t)
       (mapcar 'string-to-number)))

(defun handle-collect (acc cur)
  (cl-destructuring-bind (a b) acc
    (list (append a (list (nth 0 cur)))
          (append b (list (nth 1 cur))))))

(defun collect-lists (lst)
  (cl-reduce 'handle-collect lst :initial-value '(nil nil)))

(defun sort-lists (lst)
  (cl-destructuring-bind (a b) lst
    (list (sort a #'<)
          (sort b #'<))))

(defun distance (a b)
  (if (< a b)
      (- b a)
    (- a b)))

(defun calculate-distance (lst)
  (cl-destructuring-bind (a b) lst
    (cl-mapcar 'distance b a)))

(defun 2024-01-part1 (input)
  (->> (mapcar 'split input)
       (collect-lists)
       (sort-lists)
       (calculate-distance)
       (apply '+)))

(defun increment-table (tbl key)
  (let ((val (gethash key tbl)))
    (if (eq val nil)
        (puthash key 1 tbl)
      (puthash key (+ 1 val) tbl)))
  tbl)

(defun handle-collect-v2 (acc cur)
  (cl-destructuring-bind (lst tbl) acc
    (list (append lst (list (nth 0 cur)))
          (increment-table tbl (nth 1 cur)))))

(defun collect-data (lst)
  (cl-reduce 'handle-collect-v2 lst :initial-value (list nil (make-hash-table))))

(defun gethash-or-zero (key tbl)
  (let ((val (gethash key tbl)))
    (if (eq val nil) 0
      val)))

(defun calculate-similarity (data)
  (cl-destructuring-bind (lst tbl) data
    (defun similarity (key)
      (* key (gethash-or-zero key tbl)))
    (mapcar 'similarity lst)))

(defun 2024-01-part2 (input)
  (->> (mapcar 'split input)
       (collect-data)
       (calculate-similarity)
       (apply '+)))

(defconst testfile (expand-file-name "input/01.test.txt"))
(defconst inputfile (expand-file-name "input/01.input.txt"))

(defcheck 2024-01-part1 testfile 11)
(defcheck 2024-01-part1 inputfile 2904518)
(defcheck 2024-01-part2 testfile 31)
(defcheck 2024-01-part2 inputfile 18650129)

(solve "2024-01")

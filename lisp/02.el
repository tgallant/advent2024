;;; 02.el --- advent 2024 day 2  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/2

(require 'aoc2024)

(defun split (str)
  (->> (split-string str " " t)
       (mapcar 'string-to-number)))

(defun distance (a b)
  (if (< a b)
      (- b a)
    (- a b)))

(defun determine-dir (a b)
  (if (< a b) 'inc
    'dec))

(defun handle-safety (acc cur)
  (cl-destructuring-bind (prev dir safe-p) acc
    (cond ((not safe-p)
           (list cur dir nil))
          ((eq nil prev)
           (list cur nil t))
          ((eq (distance prev cur) 0)
           (list cur dir nil))
          ((> (distance prev cur) 3)
           (list cur dir nil))
          ((eq dir nil)
           (list cur (determine-dir prev cur) t))
          ((not (eq dir (determine-dir prev cur)))
           (list cur dir nil))
          (t (list cur (determine-dir prev cur) t)))))

(defun determine-safety (lst)
  (cl-reduce 'handle-safety lst :initial-value '(nil nil t)))

(defun is-safe (x)
  (nth 2 x))

(defun 2024-02-part1 (input)
  (->> (mapcar 'split input)
       (mapcar 'determine-safety)
       (cl-count-if 'is-safe)))

(defun remove-nth (nth list)
  (cl-loop for i in list
           for idx from 0
           unless (= idx nth)
           collect i))

(defun problem-dampen (lst)
  (cl-loop for i from 0 to (- (length lst) 1)
           for l = (remove-nth i lst)
           for r = (determine-safety (remove-nth i lst))
           if (is-safe r)
             return r
           finally return (list nil nil nil)))

(defun determine-safety-v2 (lst)
  (let ((v (cl-reduce 'handle-safety lst :initial-value '(nil nil t))))
    (if (is-safe v) v
      (problem-dampen lst))))

(defun 2024-02-part2 (input)
  (->> (mapcar 'split input)
       (mapcar 'determine-safety-v2)
       (cl-count-if 'is-safe)))

(defconst testfile (expand-file-name "input/02.test.txt"))
(defconst inputfile (expand-file-name "input/02.input.txt"))

(defcheck 2024-02-part1 testfile 2)
(defcheck 2024-02-part1 inputfile 572)
(defcheck 2024-02-part2 testfile 4)
(defcheck 2024-02-part2 inputfile 612)

(solve "2024-02")

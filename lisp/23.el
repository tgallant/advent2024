;;; 23.el --- advent 2024 day 23  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/23

(require 'aoc2024)

(defun split-pairs (str)
  (string-split str "-" t))

(defun build-map (lst)
  (cl-loop for (a b) in lst
           with map = nil
           for b-nodes = (cadr (assoc b map))
           if (eq (assoc a map) nil)
           do (push (list a (list b)) map)
           else do (push b (cadr (assoc a map)))
           if (eq (assoc b map) nil)
           do (push (list b (list a)) map)
           else do (push a (cadr (assoc b map)))
           finally return map))

(defun find-triple-connected (nodes)
  (cl-loop for (a conns) in nodes
           with map = nodes
           with out = nil
           do (cl-loop for b in conns
                       for next = (cadr (assoc b map))
                       if (not (eq next nil))
                       do (cl-loop for c in next
                                   for final = (cadr (assoc c map))
                                   for sorted = (sort (list a b c) 'string-lessp)
                                   if (member a final)
                                   if (not (member sorted out))
                                   do (push sorted out)
                                   and do (setq map (assoc-delete-all a map))
                                   ))
           finally return out))

(defun has-t (lst)
  (cl-loop for i in lst
           if (equal "t" (car (string-split i "" t)))
           return t))

(defun 2024-23-part1 (input)
  (->> (mapcar 'split-pairs input)
       (build-map)
       (find-triple-connected)
       (cl-remove-if-not 'has-t)
       (length)
       ))

(defconst testfile (expand-file-name "input/23.test.txt"))
(defconst inputfile (expand-file-name "input/23.input.txt"))

(defcheck 2024-23-part1 testfile 7)
(defcheck 2024-23-part1 inputfile 1248)

;; (defcheck* 2024-03-part2 testfile-pt2 48)
;; (defcheck* 2024-03-part2 inputfile 106921067)

(solve "2024-23")

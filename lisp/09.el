;;; 09.el --- advent 2024 day 9  -*- lexical-binding:t -*-
;;; https://adventofcode.com/2024/day/9

(require 'aoc2024)

(defun render-layout (lst)
  (cl-loop for el in lst
           with out = nil
           with id = 0
           with filep = t
           do (cl-loop repeat el
                       if filep do (push id out)
                       else do (push "." out))
           do (setq filep (not filep))
           if filep do (setq id (1+ id))
           finally return out))

(defun render-layout-v2 (lst)
  (cl-loop for el in lst
           with out = nil
           with files = nil
           with spaces = nil
           with id = 0
           with filep = t
           do (cl-loop repeat el
                       with cur = nil
                       if filep do (push id out)
                       else do (push "." out))
           do (setq filep (not filep))
           if filep do (setq id (1+ id))
           finally return (list (nreverse out) files (nreverse spaces))))

(defun next-free (layout prev max)
  (cl-loop for x from prev to max
           if (equal (nth x layout) ".") return x))

(defun defrag (layout)
  (cl-loop while t
           with idx = (- (length layout) 1)
           with val = (nth idx layout)
           with free = (next-free layout 0 idx)
           if (eq free nil) return layout
           if (not (eq val nil))
           do (setf (nth free layout) val)
           and do (setf (nth idx layout) ".")
           and do (setq idx (- idx 1))
           and do (setq val (nth idx layout))
           and do (setq free (next-free layout free idx))))

(defun defrag-v2 (data)
  (cl-destructuring-bind (layout files spaces) data
    (cl-loop while t
             with idx = (- (length layout) 1)
             with val = (nth idx layout)
             with free = (next-free layout 0 idx)
             if (eq free nil) return layout
             if (not (eq val nil))
             do (setf (nth free layout) val)
             and do (setf (nth idx layout) ".")
             and do (setq idx (- idx 1))
             and do (setq val (nth idx layout))
             and do (setq free (next-free layout free idx)))))

(defun calculate-checksum (layout)
  (cl-loop for x from 0 to (- (length layout) 1)
           for val = (nth x layout)
           collect (* x val)))

(defun freep (val)
  (equal val "."))

(defun 2024-09-part1 (input)
  (->> (split input)
       (mapcar 'string-to-number)
       (render-layout)
       (nreverse)
       (defrag)
       (cl-remove-if 'freep)
       (calculate-checksum)
       (apply '+)))

(defun 2024-09-part2 (input)
  (->> (split input)
       (mapcar 'string-to-number)
       (render-layout-v2)
       (defrag-v2)
       (cl-remove-if 'freep)
       (calculate-checksum)
       (apply '+)
       ))

(defconst testfile (expand-file-name "input/09.test.txt"))
(defconst inputfile (expand-file-name "input/09.input.txt"))

(defcheck* 2024-09-part1 testfile 1928)
(defcheck* 2024-09-part1 inputfile 6356833654075)
;; (defcheck* 2024-09-part2 testfile 11387)
;; (defcheck* 2024-09-part2 inputfile 11387)

;; (solve "2024-09")

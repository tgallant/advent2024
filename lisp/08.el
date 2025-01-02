;;; 08.el --- advent 2024 day 8  -*- lexical-binding:t -*-
;;; https://adventofcode.com/2024/day/8

(require 'aoc2024)

(defun split (str)
  (string-split str "" t))

(defun collect-antennas (grid)
  (cl-loop for x from 0 to (- (length grid) 1)
           with tbl = (make-hash-table :test 'equal)
           do (cl-loop for y from 0 to (- (length (nth x grid)) 1)
                       for point = (list x y)
                       for val = (nth y (nth x grid))
                       for acc = (gethash val tbl)
                       if (not (equal val "."))
                       if (eq acc nil) do (puthash val (list point) tbl)
                       else do (puthash val (append acc (list point)) tbl)
                       )
           finally return tbl))

(defun calculate-antinodes (tbl)
  (cl-loop for k being the hash-keys of tbl
           for v = (gethash k tbl)
           do (cl-loop for point in v
                       do (print k)
                       do (print point))
           ))

(defun 2024-08-part1 (input)
  (->> (mapcar 'split input)
       (collect-antennas)
       (calculate-antinodes)))

;; (defun 2024-07-part2 (input)
;;   (->> (parse-input input)
;;        (mapcar (lambda (d)
;;                  (let ((r) (check-validity d))
;;                    (if r r
;;                      (check-validity d ops-v2)))))
;;        (cl-remove-if 'nilp)
;;        (apply '+)))

(defconst testfile (expand-file-name "input/08.test.txt"))
(defconst inputfile (expand-file-name "input/08.input.txt"))

;; (defcheck 2024-07-part1 testfile 3749)
;; (defcheck 2024-07-part1 inputfile 5837374519342)
;; (defcheck 2024-07-part2 testfile 11387)
;; (defcheck 2024-07-part2 inputfile 11387)

;; (solve "2024-07")

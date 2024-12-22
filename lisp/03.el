;;; 03.el --- advent 2024 day 3  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/3

(require 'aoc2024)
(require 'parsec)

(defun s-valid-mul ()
  (parsec-re "mul([[:digit:]]+,[[:digit:]]+)"))

(defun s-junk ()
  (list 'junk
        (parsec-many-till-as-string
         (parsec-any-ch)
         (parsec-lookahead
          (parsec-or
           (s-valid-mul)
           (parsec-eof))))))

(defun s-mul ()
  (list 'mul
        (parsec-collect
         (string-to-number
          (parsec-many-till-as-string (parsec-digit)
                                      (parsec-try (parsec-str ","))))
         (string-to-number
          (parsec-many-till-as-string (parsec-digit)
                                      (parsec-try (parsec-str ")")))))))

(defun s-seg ()
  (parsec-and (parsec-lookahead (s-valid-mul))
              (parsec-str "mul(")
              (s-mul)))

(defun s-parse-exprs ()
  (parsec-many-till (parsec-or (s-seg)
                               (s-junk))
                    (parsec-try (parsec-eof))))

(defun s-program ()
  (parsec-start
   (s-parse-exprs)))

(defun parse-valid (str)
  (parsec-with-input str
    (s-program)))

(defun mul (lst)
  (* (nth 0 lst)
     (nth 1 lst)))

(defun mulp (lst)
  (eq 'mul (car lst)))

(defun 2024-03-part1 (input)
  (->> (parse-valid input)
       (cl-remove-if-not 'mulp)
       (mapcar 'cadr)
       (mapcar 'mul)
       (apply '+)))

(defconst testfile (expand-file-name "input/03.test.txt"))
(defconst inputfile (expand-file-name "input/03.input.txt"))

(defcheck* 2024-03-part1 testfile 161)
(defcheck* 2024-03-part1 inputfile 174561379)

;; (defcheck 2024-02-part2 testfile 4)
;; (defcheck 2024-02-part2 inputfile 612)

(solve "2024-03")

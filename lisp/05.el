;;; 05.el --- advent 2024 day 5  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/5

(require 'aoc2024)
(require 'parsec)

(defun s-rule ()
  (parsec-collect
   (string-to-number
    (parsec-many-till-as-string
     (parsec-any-ch)
     (parsec-try (parsec-str "|"))))
   (string-to-number
    (parsec-many-till-as-string
     (parsec-any-ch)
     (parsec-try (parsec-eol))))))

(defun s-number ()
  (string-to-number
   (parsec-many-as-string (parsec-digit))))

(defun s-update ()
  (parsec-sepby (s-number) (parsec-str ",")))

(defun s-program ()
  (parsec-collect
   (parsec-many-till (s-rule) (parsec-try (parsec-newline)))
   (parsec-endby (s-update) (parsec-eol))))

(defun parse-input (input)
  (parsec-with-input input
    (s-program)))

(defun validp (a b rules)
  (cond ((member (list b a) rules) nil)
        (t)))

(defun collect-valid (data)
  (cl-destructuring-bind (rules updates) data
    (cl-loop for update in updates
             with valid = nil
             for invalid = nil
             for len = (- (length update) 1)
             for max = (- (length update) 2)
             do (cl-loop for a from 0 to max
                         for next = (+ a 1)
                         do (cl-loop for b from next to len
                                     if (not (validp (nth a update) (nth b update) rules))
                                     do (setq invalid t)
                                     and return nil)
                         if invalid return nil)
             if (not invalid) do (push update valid)
             finally return valid)))

(defun collect-and-fix-invalid (data)
  (cl-destructuring-bind (rules updates) data
    (cl-loop for update in updates
             with valid = nil
             for invalid = nil
             for len = (- (length update) 1)
             for max = (- (length update) 2)
             do (cl-loop for a from 0 to max
                         for next = (+ a 1)
                         do (cl-loop for b from next to len
                                     if (not (validp (nth a update) (nth b update) rules))
                                     do (setq invalid t)
                                     and do (print (list (nth a update) (nth b update)))
                                     and return nil)
                         if invalid return nil)
             if invalid do (push update valid)
             finally return valid)))

(defun middle (lst)
  (nth (ceiling (/ (length lst) 2)) lst))

(defun 2024-05-part1 (input)
  (->> (parse-input input)
       (collect-valid)
       (mapcar 'middle)
       (apply '+)))

(defun 2024-05-part2 (input)
  (->> (parse-input input)
       (collect-and-fix-invalid)
       ;; (mapcar 'middle)
       ;; (apply '+)
       ))

(defconst testfile (expand-file-name "input/05.test.txt"))
(defconst inputfile (expand-file-name "input/05.input.txt"))

(defcheck* 2024-05-part1 testfile 143)
(defcheck* 2024-05-part1 inputfile 6041)
;; (defcheck 2024-02-part2 testfile 4)
;; (defcheck 2024-02-part2 inputfile 612)

(solve "2024-05")

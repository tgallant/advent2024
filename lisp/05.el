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

(defun find-invalid-pair (update rules)
  (cl-loop for a from 0 to (- (length update) 2)
           with invalid = nil
           do (cl-loop for b from (1+ a) to (- (length update) 1)
                       if (not (validp (nth a update) (nth b update) rules))
                       do (setq invalid (list a b)))
           if invalid return invalid))

(defun collect-valid (data)
  (cl-destructuring-bind (rules updates) data
    (cl-loop for update in updates
             with pairs = nil
             if (not (find-invalid-pair update rules))
             do (push update pairs)
             finally return pairs)))

(defun collect-invalid (data)
  (cl-destructuring-bind (rules updates) data
    (cl-loop for update in updates
             with pairs = nil
             if (find-invalid-pair update rules)
             do (push update pairs)
             finally return (list pairs rules))))

(defun swap (update pair)
  (cl-destructuring-bind (a b) pair
    (let ((a-val (nth a update))
          (b-val (nth b update)))
      (setf (nth a update) b-val)
      (setf (nth b update) a-val)
      update)))

(defun fix-invalid (update rules)
  (cl-loop while t
           with next = update
           with pair = nil
           do (setq pair (find-invalid-pair next rules))
           if (eq nil pair) return next
           else do (setq next (swap update pair))))

(defun middle (lst)
  (nth (ceiling (/ (length lst) 2)) lst))

(defun 2024-05-part1 (input)
  (->> (parse-input input)
       (collect-valid)
       (mapcar 'middle)
       (apply '+)))

(defun 2024-05-part2 (input)
  (-<>> (parse-input input)
        (collect-invalid)
        (cl-destructuring-bind (pairs rules) <>
          (mapcar (lambda (x) (fix-invalid x rules)) pairs))
        (mapcar 'middle)
        (apply '+)))

(defconst testfile (expand-file-name "input/05.test.txt"))
(defconst inputfile (expand-file-name "input/05.input.txt"))

(defcheck* 2024-05-part1 testfile 143)
(defcheck* 2024-05-part1 inputfile 6041)
(defcheck* 2024-05-part2 testfile 123)
(defcheck* 2024-05-part2 inputfile 4884)

(solve "2024-05")

;;; 07.el --- advent 2024 day 7  -*- lexical-binding:t -*-
;;; https://adventofcode.com/2024/day/7

(require 'aoc2024)
(require 'parsec)

(defun s-number ()
  (string-to-number
   (parsec-many-as-string (parsec-digit))))

(defun s-update ()
  (parsec-sepby (s-number) (parsec-str ",")))

(defun s-test-value ()
  (string-to-number
   (parsec-many-till-as-string
    (parsec-digit)
    (parsec-try (parsec-str ": ")))))

(defun s-test-input ()
  (parsec-sepby (s-number) (parsec-str " ")))

(defun s-equation ()
  (parsec-collect
   (s-test-value)
   (s-test-input)))

(defun s-calibrations ()
  (parsec-endby (s-equation) (parsec-eol)))

(defun parse-input (input)
  (parsec-with-input input
    (s-calibrations)))

(defun truep (val)
  (eq val t))

(defun nilp (val)
  (eq val nil))

(defun || (a b)
  (string-to-number
   (string-join (list (number-to-string a) (number-to-string b)))))

(defconst ops '(+ *))
(defconst ops-v2 '(+ * ||))

(defun step (acc cur)
  (append acc (mapcar (lambda (op) (append cur (list op))) ops)))

(defun step-v2 (acc cur)
  (append acc (mapcar (lambda (op) (append cur (list op))) ops-v2)))

(defun make-next (lst)
  (if (eq nil lst) (list '(+) '(*))
    (cl-reduce 'step lst :initial-value nil)))

(defun make-next-v2 (lst)
  (if (eq nil lst) (list '(+) '(*) '(||))
    (cl-reduce 'step-v2 lst :initial-value nil)))

(defun make-ops (count)
  (cl-loop repeat count with next = nil
           do (setq next (make-next next))
           finally return next))

(defun make-ops-v2 (count)
  (cl-loop repeat count with next = nil
           do (setq next (make-next-v2 next))
           finally return next))

(defun operate (acc cur)
  (let ((val (car acc))
        (rst (cdr acc)))
    (cons (funcall cur val (car rst)) (cdr rst))))

(defun prepare-eq (lst oprs)
  (car (cl-reduce 'operate oprs :initial-value lst)))

(defun make-eqs (lst)
  (mapcar (lambda (oprs) (prepare-eq lst oprs))
          (make-ops (- (length lst) 1))))

(defun check-validity (data)
  (cl-destructuring-bind (v lst) data
    (cl-loop for oprs in (make-ops (- (length lst) 1))
             if (= v (prepare-eq lst oprs)) return v)))

(defun check-validity-v2 (data)
  (cl-destructuring-bind (v lst) data
    (cl-loop for oprs in (make-ops-v2 (- (length lst) 1))
             if (= v (prepare-eq lst oprs)) return v)))

(defun 2024-07-part1 (input)
  (->> (parse-input input)
       (mapcar 'check-validity)
       (cl-remove-if 'nilp)
       (apply '+)))

(defun 2024-07-part2 (input)
  (->> (parse-input input)
       (mapcar 'check-validity-v2)
       (cl-remove-if 'nilp)
       (apply '+)))

(defconst testfile (expand-file-name "input/07.test.txt"))
(defconst inputfile (expand-file-name "input/07.input.txt"))

(defcheck* 2024-07-part1 testfile 3749)
(defcheck* 2024-07-part1 inputfile 5837374519342)
(defcheck* 2024-07-part2 testfile 11387)
(defcheck* 2024-07-part2 inputfile 11387)

;; (solve "2024-07")

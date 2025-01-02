;;; 07.el --- advent 2024 day 7  -*- lexical-binding:t -*-
;;; https://adventofcode.com/2024/day/7

(require 'aoc2024)
(require 'generator)
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

(defun || (a b)
  (string-to-number
   (string-join (list (number-to-string a) (number-to-string b)))))

(defconst ops '(+ *))
(defconst ops-v2 '(+ * ||))

(iter-defun my-iter (x)
  (iter-yield (1+ (iter-yield (1+ x))))
  ;; Return normally
  -1)

(defun fill (lst len op)
  (cl-loop while t
           with new = lst
           if (>= (length new) len) return new
           do (setq new (append new (list op)))))

(defun all-filler (lst)
  (cl-loop for el in lst
           if (not (eq el '+)) return nil
           finally return t))

;; iterator to return each combination one by one as needed
;; fill remaining items to match expected length

;;; include values
;;; keep a rolling tally of the results
;;; exclude a branch if the tally is greater than the expected
(iter-defun make-ops-iter (n oprs)
  (cl-loop for el in (mapcar 'list oprs)
           do (iter-yield (fill el n '+)))
  (cl-loop repeat (- n 1)
           with state = (mapcar 'list oprs)
           if (= n 1) return nil
           do (cl-loop for el in state
                       with next = nil
                       do (cl-loop for op in oprs
                                   for cur = (append el (list op))
                                   for filled = (fill cur n '+)
                                   do (setq next (append next (list cur)))
                                   if (not (all-filler filled))
                                   do (iter-yield filled))
                       do (setq state next))
           ))

(defun make-next (lst oprs)
  (defun step (acc cur)
    (append acc (mapcar (lambda (op) (append cur (list op))) oprs)))
  (if (eq nil lst) (mapcar 'list oprs)
    (cl-reduce 'step lst :initial-value nil)))

(iter-defun make-next-iter (lst oprs)
  (defun step (acc cur)
    (mapcar (lambda (op) (iter-yield (append cur (list op)))) oprs))
  (if (eq nil lst) (iter-yield (mapcar 'list oprs))
    (cl-reduce 'step lst :initial-value nil)))

(defun make-ops (oprs count)
  (cl-loop for i from 1 to count
           with next = nil
           with tbl = (make-hash-table)
           do (setq next (make-next next oprs))
           do (puthash i next tbl)
           finally return tbl))

(defun operate (acc cur)
  (let ((val (car acc))
        (rst (cdr acc)))
    (cons (funcall cur val (car rst)) (cdr rst))))

(defun prepare-eq (lst oprs)
  (car (cl-reduce 'operate oprs :initial-value lst)))

(defun make-eqs (lst)
  (mapcar (lambda (oprs) (prepare-eq lst oprs))
          (make-ops (- (length lst) 1))))

(defun check-validity (data initial-ops)
  (cl-destructuring-bind (v lst) data
    (cl-loop for oprs iter-by (make-ops-iter (- (length lst) 1) initial-ops)
             for result = (prepare-eq lst oprs)
             do (message "loop")
             if (= v result) return v)))

(defun check-with-ops (data ops-tbl)
  (mapcar (lambda (d) (check-validity d ops-tbl)) data))

(defun check-all (oprs data)
  (->> (mapcar (lambda (d) (length (nth 1 d))) data)
       (apply 'max)
       (make-ops oprs)
       (check-with-ops data)))

(defun 2024-07-part1 (input)
  (->> (parse-input input)
       (mapcar (lambda (d) (check-validity d ops)))
       (cl-remove-if 'nilp)
       (apply '+)))

(defun 2024-07-part2 (input)
  (->> (parse-input input)
       (mapcar (lambda (d)
                 (let ((r) (check-validity d))
                   (if r r
                     (check-validity d ops-v2)))))
       (cl-remove-if 'nilp)
       (apply '+)))

(defconst testfile (expand-file-name "input/07.test.txt"))
(defconst inputfile (expand-file-name "input/07.input.txt"))

(defcheck* 2024-07-part1 testfile 3749)
(defcheck* 2024-07-part1 inputfile 5837374519342)
(defcheck* 2024-07-part2 testfile 11387)
(defcheck* 2024-07-part2 inputfile 11387)

;; (solve "2024-07")

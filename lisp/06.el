;;; 06.el --- advent 2024 day 6  -*- lexical-binding:t; coding:utf-8 -*-
;;; https://adventofcode.com/2024/day/6

(require 'aoc2024)

(defun find-guard (map)
  (cl-loop for x from 0 to (- (length map) 1)
           with found = nil
           do (cl-loop for y from 0 to (- (length (nth x map)) 1)
                       if (equal "^" (nth y (nth x map)))
                       do (setq found (cons (list x y) "^"))
                       and return nil)
           if found return found))

(defun find-obstacles(map)
  (cl-loop for x from 0 to (- (length map) 1)
           with found = nil
           do (cl-loop for y from 0 to (- (length (nth x map)) 1)
                       if (equal "#" (nth y (nth x map)))
                       do (push (list x y) found))
           finally return found))

(defun guard-out-of-bounds (guard map)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((>= x (length map)) t)
          ((>= y (length (nth x map))) t)
          ((< x 0) t)
          ((< y 0) t))))

(defun guard-forward (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (cons (list (- x 1) y) dir))
          ((equal dir ">")
           (cons (list x (+ y 1)) dir))
          ((equal dir "v")
           (cons (list (+ x 1) y) dir))
          ((equal dir "<")
           (cons (list x (- y 1)) dir)))))

(defun guard-turn (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (cons (list x (+ y 1)) ">"))
          ((equal dir ">")
           (cons (list (+ x 1) y) "v"))
          ((equal dir "v")
           (cons (list x (- y 1)) "<"))
          ((equal dir "<")
           (cons (list (- x 1) y) "^")))))

(defun guard-peek (guard map)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (nth y (nth (- x 1) map)))
          ((equal dir ">")
           (nth (+ y 1) (nth x map)))
          ((equal dir "v")
           (nth y (nth (+ x 1) map)))
          ((equal dir "<")
           (nth (- y 1) (nth x map))))))

(defun guard-peek-pos (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^")
           (list (- x 1) y))
          ((equal dir ">")
           (list x (+ y 1)))
          ((equal dir "v")
           (list (+ x 1) y))
          ((equal dir "<")
           (list x (- y 1))))))

(defun guard-next (guard map)
  (if (equal "#" (guard-peek guard map))
      (guard-turn guard)
    (guard-forward guard)))

(defun track-guard-path (map)
  (cl-loop while t
           with guard = (find-guard map)
           with path = nil
           if (guard-out-of-bounds guard map) return path
           else do (push (car guard) path)
           and do (setq guard (guard-next guard map))))

(defun next-dir (dir)
  (if (eq dir 'x) 'y 'x))

(defun guard-next-dir (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^") ">")
          ((equal dir ">") "v")
          ((equal dir "v") "<")
          ((equal dir "<") "^"))))

(defun guard-next-dir* (guard)
  (cl-destructuring-bind ((x y) . dir) guard
    (cond ((equal dir "^") ">")
          ((equal dir ">") "<")
          ((equal dir "v") "^")
          ((equal dir "<") ">"))))

(defun completes-loop-p (guard map corners)
  (message "checking %s" guard)
  (cl-loop while t
           with next = (cons (car guard) (guard-next-dir guard))
           with peek = (guard-peek guard map)
           with cnrs = corners
           if (guard-out-of-bounds next map) return nil
           if (equal peek "#")
           if (member next cnrs) return t
           else do (setq cnrs (append cnrs (list next)))
           do (setq next (guard-next next map))
           do (setq peek (guard-peek next map))
           ))

(defun guard-loop-p (guard map corners obstacles)
  (cl-destructuring-bind ((x y) . dir) guard
    (defun loop-point-p (point)
      (cond ((equal "^" dir)
             (and (eq x (nth 0 point)) (< y (nth 1 point))))
            ((equal ">" dir)
             (and (eq y (nth 1 point)) (< x (nth 0 point))))
            ((equal "v" dir)
             (and (eq x (nth 0 point)) (> y (nth 1 point))))
            ((equal "<" dir)
             (and (eq y (nth 1 point)) (> x (nth 0 point))))))
    (cl-loop for point in obstacles
             if (loop-point-p point)
             return (completes-loop-p guard map corners))))

(defun track-guard-path-v2 (map)
  (cl-loop while t
           with initial = nil
           with guard = (find-guard map)
           with obstacles = (find-obstacles map)
           with corners = nil
           with peek = nil
           with loops = nil
           if (eq initial nil) do (setq initial guard)
           if (eq peek nil) do (setq peek (guard-peek guard map))
           if (guard-out-of-bounds guard map) return loops
           if (equal peek "#")
           do (push guard corners)
           if (not (equal peek "#"))
           if (not (equal (guard-peek-pos guard) (car initial)))
           if (guard-loop-p guard map corners obstacles) do (push guard loops)
           do (setq guard (guard-next guard map))
           do (setq peek (guard-peek guard map))))

;; (6 4)
;; (6 6)
;; (7 6)
;; (8 2)
;; (8 4)
;; (9 7)

(defun 2024-06-part1 (input)
  (-<>> (mapcar 'split input)
        (track-guard-path)
        (cl-remove-duplicates <> :test 'equal)
        (length)))

(defun 2024-06-part2 (input)
  (-<>> (mapcar 'split input)
        (track-guard-path-v2)
        ;; (cl-remove-duplicates <> :test 'equal)
        ;; (print)
        (length)
        ))

(defconst testfile (expand-file-name "input/06.test.txt"))
(defconst inputfile (expand-file-name "input/06.input.txt"))

(defcheck 2024-06-part1 testfile 41)
(defcheck 2024-06-part1 inputfile 5461)
(defcheck 2024-06-part2 testfile 6)
;; (defcheck 2024-06-part2 inputfile 4884)

;; (solve "2024-06")

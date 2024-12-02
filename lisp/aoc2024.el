;;; -*- lexical-binding: t -*-
;;; A module for making it easier to solve Advent of Code problems.

(require 'ert)
(require 'subr-x)

(provide 'aoc2024)

;;; Arrow functions are copied from the following blog post which reimplemented
;;; the cl-arrows functionality in elisp.
;;;
;;; https://tkurtbond.github.io/posts/2020/07/03/arrow-macros-in-emacs-lisp/
;;; https://github.com/nightfly19/cl-arrows/blob/master/arrows.lisp

(defun simple-inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
      (list next acc))))

(defmacro -> (initial-form &rest forms)
  "Inserts INITIAL-FORM as first argument into the first of FORMS, the result
into the next, etc., before evaluation.  FORMS are treated as list designators."
  (cl-reduce (simple-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  "Like ->, but the forms are inserted as last argument instead of first."
  (cl-reduce (simple-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun diamond-inserter (insert-fun)
  (simple-inserter (lambda (acc next)
                     (cl-case (cl-count-if #'<>p next)
                       (0 (funcall insert-fun acc next))
                       (1 (cl-substitute-if acc #'<>p next))
                       (t (let ((r (gensym "R")))
                            `(let ((,r ,acc))
                               ,(cl-substitute-if r #'<>p next))))))))

(defmacro -<> (initial-form &rest forms)
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level
element, each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (cl-reduce (diamond-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro -<>> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (cl-reduce (diamond-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun <>p (form)
  "Predicate identifying the placeholders for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(defun insert-first (arg surround)
  "Inserts ARG into the list form SURROUND as its first argument, after the
operator."
  (list* (car surround)
         arg
         (cdr surround)))

(defun insert-last (arg surround)
  "Inserts ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

;;; Advent of Code Helpers

(defun read-lines (path omit-null)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" omit-null)))

(defmacro defsolution (name &rest forms)
  `(defun ,name (input)
     ,(append `(-<>> (read-lines input t)) forms)))

(defmacro defsolution-with-null (name &rest forms)
  `(defun ,name (input)
     ,(append `(-<>> (read-lines input nil)) forms)))

(defmacro defsolve (name &rest forms)
  (defun make-test (form)
    (let* ((fn (car form))
           (val (cadr form))
           (fn-name (symbol-name (car fn)))
           (fn-input (cadr (split-string (cadr fn) "\\." t)))
           (test-name (intern (string-join `(,name ,fn-name ,fn-input) "-"))))
      (eval `(ert-deftest ,test-name ()
         (should (equal ,fn ,val))) t)))
  (mapc 'make-test forms))

(defun make-test-name (fn in)
  (intern (string-join (list (symbol-name fn) "-" (symbol-name in) "-test"))))

(defmacro defcheck (fn in expected)
  (let ((test-name (make-test-name fn in)))
    `(ert-deftest ,test-name ()
       (should (equal (funcall (quote ,fn) (read-lines ,in t)) ,expected)))))

(defun solve (name)
  (ert name))

(defun t2-hello ()
  (+ 3 3))

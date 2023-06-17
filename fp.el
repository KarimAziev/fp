;;; fp.el --- Collection of combinators for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/fp
;; Keywords: lisp, extensions
;; Version: 2.2.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Collection of combinators for Emacs Lisp

;;; Code:



(defun fp--expand (init-fn)
  "If INIT-FN is a non-quoted symbol, add a sharp quote.
Otherwise, return it as is."
  (setq init-fn (macroexpand init-fn))
  (if (symbolp init-fn)
      `(#',init-fn)
    `(,init-fn)))

(defmacro fp-pipe (&rest functions)
  "Return a left-to-right composition from FUNCTIONS.
The first argument may have any arity; the remaining arguments must be unary."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       ,@(let ((init-fn (pop functions)))
           (list
            (seq-reduce
             (lambda (acc fn)
               `(funcall ,@(fp--expand fn) ,acc))
             functions
             `(apply ,@(fp--expand init-fn) ,args)))))))

(defmacro fp-compose (&rest functions)
  "Return a right-to-left composition from FUNCTIONS.
The last function may have any arity; the remaining arguments must be unary."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(fp-pipe ,@(reverse functions)))

(defmacro fp-or (&rest functions)
  "Expand to a unary function that calls FUNCTIONS until first non-nil result.
Return that first non-nil result without calling the remaining functions.
If all functions returned nil, the result will be also nil."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  (let ((it (make-symbol "it")))
    `(lambda (,it)
       (or
        ,@(mapcar (lambda (v)
                    `(funcall ,@(fp--expand v) ,it))
                  functions)))))

(defmacro fp-and (&rest functions)
  "Return a unary function that calls FUNCTIONS until one of them yields nil.
If all functions return non-nil, return the last such value."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  (let ((it (make-symbol "it")))
    `(lambda (,it)
       (and
        ,@(mapcar (lambda (v)
                    `(funcall ,@(fp--expand v) ,it))
                  functions)))))

(defmacro fp-partial (fn &rest args)
  "Return a partial application of a function FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function that does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (let ((pre-args (make-symbol "pre-args")))
    `(lambda (&rest ,pre-args)
       ,(car (list
              `(apply ,@(fp--expand fn)
                      (append (list ,@args) ,pre-args)))))))

(defmacro fp-rpartial (fn &rest args)
  "Return a partial application of a function FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (let ((pre-args (make-symbol "pre-args")))
    `(lambda (&rest ,pre-args)
       ,(car (list
              `(apply ,@(fp--expand fn)
                      (append ,pre-args (list ,@args))))))))

(defmacro fp-converge (combine-fn &rest functions)
  "Return a function to apply COMBINE-FN with the results of branching FUNCTIONS.
If the first element of FUNCTIONS is a vector, it will be used instead.

Example:

\(funcall (fp-converge concat [upcase downcase]) \"John\").
\(funcall (fp-converge concat upcase downcase) \"John\")

Result: \"JOHNjohn\"."
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       (apply
        ,@(fp--expand combine-fn)
        (list
         ,@(mapcar (lambda (v)
                     `(apply ,@(fp--expand v) ,args))
                   (if (vectorp (car functions))
                       (append (car functions) nil)
                     functions)))))))

(defmacro fp-use-with (combine-fn &rest functions)
  "Return a function with the arity of length FUNCTIONS.
Call every branching function with an argument at the same index,
and finally, COMBINE-FN will be applied to the supplied values.

Example:

\(funcall (fp-use-with concat [upcase downcase]) \"hello \" \"world\")


If first element of FUNCTIONS is vector, it will be used instead:

\(funcall (fp-use-with + [(fp-partial 1+) identity]) 2 2)
=> Result: 5

\(funcall (fp-use-with + (fp-partial 1+) identity) 2 2)
=> Result: 5

=> Result: \"HELLO world\"."
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       (apply
        ,@(fp--expand combine-fn)
        (list
         ,@(seq-map-indexed (lambda (v idx)
                              `(funcall ,@(fp--expand v)
                                        (nth ,idx ,args)))
                            (if (vectorp (car functions))
                                (append (car functions) nil)
                              functions)))))))

(defmacro fp-when (pred fn)
  "Return a function that calls FN if the result of calling PRED is non-nil.
Both PRED and FN are called with one argument.
If the result of PRED is nil, return the argument as is."
  (declare
   (indent defun))
  (let ((arg (make-symbol "arg")))
    `(lambda (,arg)
       (if
           (funcall ,@(fp--expand pred) ,arg)
           (funcall ,@(fp--expand fn) ,arg)
         ,arg))))

(defmacro fp-unless (pred fn)
  "Return a unary function that invokes FN if the result of calling PRED is nil.
Accept one argument and pass it both to PRED and FN.
If the result of PRED is non-nil, return the argument as is."
  (let ((arg (make-symbol "arg")))
    `(lambda (,arg)
       (if (funcall ,@(fp--expand pred) ,arg)
           ,arg
         (funcall ,@(fp--expand fn) ,arg)))))

(defmacro fp-const (value)
  "Return a function that always returns VALUE.
This function accepts any number of arguments but ignores them."
  (declare (pure t)
           (side-effect-free error-free))
  (let ((arg (make-symbol "_")))
    `(lambda (&rest ,arg) ,value)))

(defmacro fp-ignore-args (fn)
  "Return a function that invokes FN without args.
This function accepts any number of arguments but ignores them."
  (declare
   (indent defun))
  (let ((arg (make-symbol "_")))
    `(lambda (&rest ,arg)
       (funcall ,@(fp--expand fn)))))

(defmacro fp-cond (&rest pairs)
  "Return a function that expands a list of PAIRS to cond clauses.
Every pair should be either:
- a vector of [predicate transformer],
- a list of (predicate transformer).

The predicate can also be t.

All of the arguments to function are applied to each of the predicates in turn
until one returns a \"truthy\" value, at which point fn returns the result of
applying its arguments to the corresponding transformer."
  (declare (pure t)
           (indent defun)
           (side-effect-free error-free))
  (setq pairs (mapcar (lambda (it)
                        (if (listp it)
                            (apply #'vector it)
                          it))
                      pairs))
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       (cond ,@(mapcar (lambda (v)
                         (list (if (eq (aref v 0) t) t
                                 `(apply ,@(fp--expand (aref v 0)) ,args))
                               `(apply ,@(fp--expand (aref v 1)) ,args)))
                       pairs)))))

(defmacro fp-not (fn)
  "Return a function that negates the result of function FN."
  `(fp-compose not ,fn))

(defun fp-t (&rest _)
  "Do nothing and return t.

This function accepts any number of arguments, but ignores them."
  t)

(defun fp-nil (&rest _)
  "Do nothing and return nil.
This function accepts any number of arguments but ignores them."
  nil)

(defmacro fp-ignore-errors-partial (fn &rest args)
  "Apply FN with ARGS ignoring errors."
  (declare
   (indent defun))
  `(lambda (&rest right-args)
     (condition-case nil
         (progn
           (apply ,@(fp--expand fn)
                  (append (list ,@args) right-args)))
       (error nil))))


(defmacro fp-ignore-errors-rpartial (fn &rest args)
  "Return a function that takes in arguments FN and ARGS.
This macro returns a lambda function that takes &REST PRE-ARGS.
Within the lambda function, apply FN to PRE-ARGS and ARGS. Should FN throw an
error, return nil instead."
  (declare
   (indent defun))
  `(lambda (&rest pre-args)
     (condition-case nil
         (progn
           (apply ,@(fp--expand fn)
                  (append pre-args (list ,@args))))
       (error nil))))

(defun fp-rpartial-ignore-errors (fn &rest args)
  "Return a function that is a partial application of FN to ARGS.
ARGS is a list of the first N arguments to pass to FN.

The result is a new function which does the same as FN,
except that:
- the first N arguments are fixed at the values with which this function
was called
- if an error occurs, return nil."
  (lambda (&rest args2)
    (condition-case nil
        (progn
          (apply fn
                 (append args args2)))
      (error nil))))

(defun fp-partial-ignore-errors (fn &rest args)
  "Return a function that is a partial application of FN to ARGS.
ARGS is a list of the first N arguments to pass to FN.

The result is a new function which does the same as FN,
except that:
- the first N arguments are fixed at the values with which this function
was called
- if an error occurs, return nil."
  (lambda (&rest args2)
    (condition-case nil
        (progn
          (apply fn
                 (append args args2)))
      (error nil))))

(when (version<= "28.1" emacs-version)
  (eval-when-compile
    (require 'shortdoc nil t)
    (when (fboundp 'define-short-documentation-group)
      (define-short-documentation-group fp
        "Combinators"
        (fp-pipe
         :eval (fp-pipe split-string upcase)
         :eval (funcall (fp-pipe upcase split-string) "some string"))
        (fp-compose
         :eval (fp-compose split-string upcase)
         :eval (funcall (fp-compose split-string upcase) "some string"))
        (fp-partial
         :eval (funcall (fp-rpartial > 3) 2))
        (fp-rpartial
         :eval (funcall
                (fp-rpartial plist-get :name) '("John" :age 30)))
        (fp-and
         :eval (funcall (fp-and numberp 1+) 30))
        (fp-or
         :eval (fp-or floatp integerp)
         :eval (funcall (fp-or floatp integerp) 3)
         :eval (seq-filter
                (fp-or numberp stringp)
                '("a" "b" (0 1 2 3 4) "c" 34 (:name "John"
                                                    :age 30))))
        (fp-converge
         :eval "(funcall
                (fp-converge concat [upcase downcase])
                \"John\")"
         :eval "(funcall
                (fp-converge concat upcase downcase)
                \"John\")")
        (fp-when
          :eval
          "(funcall (fp-when
                    (fp-compose (fp-partial < 4) length)
                    (fp-rpartial substring 0 4))
                  \"long string\")")
        (fp-unless
         :eval (funcall (fp-unless zerop (fp-partial / 2)) 0))
        (fp-const
         :eval (funcall (fp-const 2) 4))
        (fp-ignore-args
          :eval "(funcall (fp-ignore-args
                           (lambda () (message \"No arguments\")))
                         4)")
        (fp-use-with
         :eval "(funcall
                (fp-use-with concat [upcase downcase])
                \"hello \" \"world\")"
         :eval
         "(funcall
           (fp-use-with + [(fp-partial 1+) identity])
          2 2)")
        (fp-cond
          :eval
          "(funcall (fp-cond
                     [stringp identity]
                     [symbolp symbol-name]
                     [integerp number-to-string]
                     [floatp number-to-string]
                     [t (fp-partial format \"%s\")])
                   2)"
          :eval "(funcall
                  (fp-cond
                    (stringp identity)
                    (symbolp symbol-name)
                    (integerp number-to-string)
                    (floatp number-to-string)
                    (t (fp-partial format \"%s\")))
                  2)")
        (fp-not
         :eval
         (funcall (fp-not stringp) 4))
        (fp-nil
         :eval (fp-nil t)
         :eval (fp-nil 23)
         :eval (fp-nil))
        (fp-t
         :eval (fp-t nil)
         :eval (fp-t)
         :eval (fp-t 23))))))

(provide 'fp)
;;; fp.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:

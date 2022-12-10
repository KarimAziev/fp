;;; fp.el --- Collection of combinators for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/fp
;; Keywords: lisp
;; Version: 2.0.0
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

;; URL: https://github.com:KarimAziev/fp.git

;;; Commentary:

;; Collection of combinators for Emacs Lisp

;;; Code:

(defun fp--expand (init-fn)
  "Expand apply.
INIT-FN is ."
  (setq init-fn (macroexpand init-fn))
  (if (symbolp init-fn)
      `(#',init-fn)
    `(,init-fn)))

(defmacro fp-pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             `(funcall ,@(fp--expand fn) ,acc))
           functions
           `(apply ,@(fp--expand init-fn) args))))))

(defmacro fp-compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(fp-pipe ,@(reverse functions)))

(defmacro fp-or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (or
      ,@(mapcar (lambda (v)
                  `(funcall ,@(fp--expand v) it))
                functions))))

(defmacro fp-and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (and
      ,@(mapcar (lambda (v)
                  `(funcall ,@(fp--expand v) it))
                functions))))

(defmacro fp-partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list
            `(apply ,@(fp--expand fn)
                    (append (list ,@args) pre-args))))))

(defmacro fp-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list
            `(apply ,@(fp--expand fn)
                    (append pre-args (list ,@args)))))))

(defmacro fp-converge (combine-fn &rest functions)
  "Return a function that apply COMBINE-FN with results of branching FUNCTIONS.
If first element of FUNCTIONS is vector, it will be used instead.

Example:

\(funcall (fp-converge concat [upcase downcase]) \"John\").
\(funcall (fp-converge concat upcase downcase) \"John\")

Result: \"JOHNjohn\"."
  `(lambda (&rest args)
     (apply
      ,@(fp--expand combine-fn)
      (list
       ,@(mapcar (lambda (v)
                   `(apply ,@(fp--expand v) args))
                 (if (vectorp (car functions))
                     (append (car functions) nil)
                   functions))))))

(defmacro fp-use-with (combine-fn &rest functions)
  "Return a function with the arity of length FUNCTIONS.
Every branching function will be called with argument at the same index,
and finally COMBINE-FN will be applied on the supplied values.

Example:

\(funcall (fp-use-with concat [upcase downcase]) \"hello \" \"world\")


If first element of FUNCTIONS is vector, it will be used instead:

\(funcall (fp-use-with + [(fp-partial 1+) identity]) 2 2)
=> Result: 5

\(funcall (fp-use-with + (fp-partial 1+) identity) 2 2)
=> Result: 5

=> Result: \"HELLO world\"
."
  `(lambda (&rest args)
     (apply
      ,@(fp--expand combine-fn)
      (list
       ,@(seq-map-indexed (lambda (v idx)
                            `(funcall ,@(fp--expand v)
                                      (nth ,idx args)))
                          (if (vectorp (car functions))
                              (append (car functions) nil)
                            functions))))))

(defmacro fp-when (pred fn)
  "Return an unary function that invoke FN if result of calling PRED is non-nil.
Both PRED and FN called with one argument.
If result of PRED is nil, return the argument as is."
  `(lambda (arg)
     (if
         (funcall ,@(fp--expand pred) arg)
         (funcall ,@(fp--expand fn) arg)
       arg)))

(defmacro fp-unless (pred fn)
  "Return an unary function that invoke FN if result of calling PRED is nil.
Both PRED and FN called with one argument.
If result of PRED is non nil return the argument as is."
  `(lambda (arg)
     (if (funcall ,@(fp--expand pred) arg)
         arg
       (funcall ,@(fp--expand fn) arg))))

(defmacro fp-const (value)
  "Return a function that always return VALUE.

This function accepts any number of arguments, but ignores them."
  (declare (pure t)
           (side-effect-free error-free))
  `(lambda (&rest _) ,value))

(defmacro fp-ignore-args (fn)
  "Return a function that invoke FN without args.

This function accepts any number of arguments, but ignores them."
  (declare
   (indent defun))
  `(lambda (&rest _)
     (funcall ,@(fp--expand fn))))

(defmacro fp-not (fn)
  "Return a function which firstly invoke FN and then not."
  `(fp-compose not ,fn))

(when (version<= "28.1" emacs-version)
  (eval-and-compile
    (require 'shortdoc nil t)
    (when (fboundp 'define-short-documentation-group)
      (define-short-documentation-group fp
        "Macros"
        (fp-pipe
         :eval (funcall (fp-pipe upcase split-string) "some string"))
        (fp-compose
         :eval (funcall (fp-compose split-string upcase) "some string"))
        (fp-partial
         :eval (funcall (fp-rpartial > 3) 2))
        (fp-rpartial
         :eval (funcall (fp-rpartial plist-get :name) '(:name "John" :age 30)))
        (fp-and
         :eval (funcall (fp-and numberp 1+) 30))
        (fp-or
         :eval (seq-filter
                (fp-or numberp stringp)
                '("a" "b" (0 1 2 3 4) "c" 34 (:name "John" :age 30))))
        (fp-converge
         :eval (funcall (fp-converge concat [upcase downcase]) "John")
         :eval (funcall (fp-converge concat upcase downcase) "John"))
        (fp-when
         :eval
         (funcall (fp-when
                   (fp-compose (fp-partial < 4) length)
                   (fp-rpartial substring 0 4))
                  "long string"))
        (fp-unless
         :eval (funcall (fp-unless zerop
                                   (fp-partial / 2))
                        0))
        (fp-const
         :eval (funcall (fp-const 2) 4))
        (fp-ignore-args
          :eval (funcall (fp-ignore-args
                           (lambda (&optional a) (numberp a)))
                         4))
        (fp-use-with
         :eval (funcall (fp-use-with concat [upcase downcase]) "hello " "world")
         :eval (funcall (fp-use-with + [(fp-partial 1+) identity]) 2 2)
         :eval (funcall (fp-use-with + (fp-partial 1+) identity) 2 2))))))

(provide 'fp)
;;; fp.el ends here
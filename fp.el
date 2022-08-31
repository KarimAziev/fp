;;; fp.el --- Collection of combinators for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/fp
;; Keywords: lisp
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

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


(when (version<= "28.1" emacs-version)
  (require 'fp-shortdoc))

(defmacro fp-pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro fp-compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(fp-pipe ,@(reverse functions)))

(defmacro fp-or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (or
            ,@(mapcar (lambda (v) (if (symbolp v)
                                 `(,v it)
                               `(funcall ,v it)))
                      functions))))

(defmacro fp-and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (and
            ,@(mapcar (lambda (v) (if (symbolp v)
                                 `(,v it)
                               `(funcall ,v it)))
                      functions))))

(defmacro fp-partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

(defmacro fp-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro fp-converge (combine-fn &rest functions)
  "Return a function that apply COMBINE-FN with results of branching FUNCTIONS.
If first element of FUNCTIONS is vector, it will be used instead.

Example:

\(funcall (fp-converge concat [upcase downcase]) \"John\").
\(funcall (fp-converge concat upcase downcase) \"John\")

Result: \"JOHNjohn\"."
  `(lambda (&rest args) (apply
                    ,@(if (symbolp combine-fn)
                          `(#',combine-fn)
                        (list combine-fn))
                    (list
                     ,@(mapcar (lambda (v)
                                 (setq v (macroexpand v))
                                 (if (symbolp v)
                                     `(apply #',v args)
                                   `(apply ,v args)))
                               (if (vectorp (car functions))
                                   (append (car functions) nil)
                                 functions))))))

(defmacro fp-when (pred fn)
  "Return an unary function that invoke FN if result of calling PRED is non-nil.
Both PRED and FN called with one argument.
If result of PRED is nil, return the argument as is."
  `(lambda (arg) (if ,(if (symbolp pred)
                     `(,pred arg)
                   `(funcall ,pred arg))
                ,(if (symbolp fn)
                     `(,fn arg)
                   `(funcall ,fn arg))
              arg)))

(defmacro fp-unless (pred fn)
  "Return an unary function that invoke FN if result of calling PRED is nil.
Both PRED and FN called with one argument.
If result of PRED is non nil return the argument as is."
  `(lambda (arg) (if ,(if (symbolp pred)
                     `(,pred arg)
                   `(funcall ,pred arg))
                arg
              ,(if (symbolp fn)
                   `(,fn arg)
                 `(funcall ,fn arg)))))

(defmacro fp-const (value)
  "Return a function that always return VALUE.

This function accepts any number of arguments, but ignores them."
  (declare (pure t) (side-effect-free error-free))
  `(lambda (&rest _) ,value))

(defmacro fp-ignore-args (fn)
  "Return a function that invoke FN without args.

This function accepts any number of arguments, but ignores them."
  (declare
   (indent defun))
  `(lambda (&rest _) ,(if (symbolp fn)
                     `(,fn)
                   `(funcall ,fn))))

(provide 'fp)
;;; fp.el ends here
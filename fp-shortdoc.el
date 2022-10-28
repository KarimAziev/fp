;; -*- no-byte-compile: t; -*-
;;; fp-shortdoc.el --- Shortdoc implementation for fp.el -*- lexical-binding: t; -*-

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/fp
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1"))
;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

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

;; Shortdoc implementation for fp.el

;;; Code:

(when (version<= "28.1" emacs-version)
  (require 'shortdoc)
  (define-short-documentation-group fp
    "Macros"
    (fp-pipe
     :eval (funcall (fp-pipe upcase split-string) "some string"))
    (fp-compose
     :eval (funcall (fp-compose split-string upcase) "some string"))
    (fp-partial
     :eval (funcall (fp-rpartial > 3) 2))
    (fp-rpartial
     :eval (funcall (fp-rpartial plist-get :name) '(:name "John"
                                                          :age 30)))
    (fp-and
     :eval (funcall (fp-and numberp 1+) 30))
    (fp-or
     :eval (seq-filter
            (fp-or numberp stringp)
            '("a" "b" (0 1 2 3 4) "c" 34 (:name "John"
                                                :age 30))))
    (fp-converge
     :eval (funcall (fp-converge concat [upcase downcase]) "John")
     :eval (funcall (fp-converge concat upcase downcase) "John"))
    (fp-when
     :eval
     (funcall (fp-when (fp-compose (fp-partial < 4) length)
                       (fp-rpartial substring 0 4))
              "long string"))
    (fp-unless
     :eval (funcall (fp-unless zerop
                               (fp-partial / 2))
                    0))
    (fp-const
     :eval (funcall (fp-const 2) 4))
    (fp-ignore-args
      :eval (funcall (fp-ignore-args (lambda (&optional a)
                                       (numberp a)))
                     4))
    (fp-use-with
     :eval (funcall (fp-use-with concat [upcase downcase]) "hello " "world")
     :eval (funcall (fp-use-with + [(fp-partial 1+) identity]) 2 2)
     :eval (funcall (fp-use-with + (fp-partial 1+) identity) 2 2))))

(eval-when-compile
  (when (version< emacs-version "28.1")
    (warn "Emacs should not be compiling this file")))

(provide 'fp-shortdoc)
;;; fp-shortdoc.el ends here
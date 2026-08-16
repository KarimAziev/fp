;;; fp-test.el --- Tests for fp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karim Aziiev <karim.aziiev@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Unit tests for the public combinators in fp.el.

;;; Code:

(require 'ert)
(require 'fp)

(ert-deftest fp-test-pipe-composes-left-to-right ()
  (should
   (equal (funcall (fp-pipe list
                            (lambda (items) (apply #'+ items))
                            number-to-string)
                   1 2 3)
          "6"))
  (should (= (funcall (fp-pipe #'1+ #'1+) 1) 3)))

(ert-deftest fp-test-compose-composes-right-to-left ()
  (should
   (equal (funcall (fp-compose upcase
                               (lambda (number) (format "value: %s" number))
                               +)
                   2 3 4)
          "VALUE: 9")))

(ert-deftest fp-test-converge-applies-every-branch-to-the-same-arguments ()
  (should
   (equal (funcall (fp-converge list
                                (lambda (x y) (+ x y))
                                (lambda (x y) (* x y)))
                   2 3)
          '(5 6)))
  (should
   (equal (funcall (fp-converge list [car cadr]) '(first second))
          '(first second))))

(ert-deftest fp-test-use-with-transforms-positional-arguments ()
  (should (= (funcall (fp-use-with + 1+ 1-) 2 4) 6))
  (should
   (equal (funcall (fp-use-with list [upcase downcase]) "Hello" "WORLD")
          '("HELLO" "world"))))

(ert-deftest fp-test-partial-prepends-fixed-arguments ()
  (should
   (equal (funcall (fp-partial list 'first 'second) 'third 'fourth)
          '(first second third fourth)))
  (should (= (funcall (fp-partial (lambda (x y) (+ x y)) 10) 5) 15)))

(ert-deftest fp-test-rpartial-appends-fixed-arguments ()
  (should
   (equal (funcall (fp-rpartial list 'third 'fourth) 'first 'second)
          '(first second third fourth)))
  (should (equal (funcall (fp-rpartial substring 1 3) "abcd") "bc")))

(ert-deftest fp-test-or-short-circuits-and-returns-first-truthy-value ()
  (let ((calls nil))
    (should
     (equal (funcall
             (fp-or (lambda (_value)
                      (push 'first calls)
                      nil)
                    (lambda (_value)
                      (push 'second calls)
                      'matched)
                    (lambda (_value)
                      (push 'third calls)
                      'too-late))
             :input)
            'matched))
    (should (equal (nreverse calls) '(first second))))
  (should-not (funcall (fp-or) :input)))

(ert-deftest fp-test-and-short-circuits-and-returns-last-truthy-value ()
  (let ((calls nil))
    (should-not
     (funcall
      (fp-and (lambda (_value)
                (push 'first calls)
                'truthy)
              (lambda (_value)
                (push 'second calls)
                nil)
              (lambda (_value)
                (push 'third calls)
                'too-late))
      :input))
    (should (equal (nreverse calls) '(first second))))
  (should (equal (funcall (fp-and identity (fp-const 'last)) :input) 'last))
  (should (eq (funcall (fp-and) :input) t)))

(ert-deftest fp-test-when-transforms-only-matching-values ()
  (let* ((original (list 1 2))
         (transform (fp-when listp reverse)))
    (should (equal (funcall transform original) '(2 1)))
    (should (= (funcall transform 3) 3))))

(ert-deftest fp-test-unless-transforms-only-nonmatching-values ()
  (let* ((original (list 1 2))
         (transform (fp-unless listp 1+)))
    (should (eq (funcall transform original) original))
    (should (= (funcall transform 3) 4))))

(ert-deftest fp-test-const-ignores-all-arguments ()
  (let ((constant (fp-const '(fixed value))))
    (should (equal (funcall constant) '(fixed value)))
    (should (equal (funcall constant 1 2 3) '(fixed value)))))

(ert-deftest fp-test-ignore-args-calls-function-without-arguments ()
  (let* ((call-count 0)
         (without-args
          (fp-ignore-args
            (lambda ()
              (setq call-count (1+ call-count))
              :called))))
    (should (eq (funcall without-args 'ignored 'values) :called))
    (should (= call-count 1))))

(ert-deftest fp-test-cond-accepts-vector-clauses ()
  (let ((convert
         (fp-cond [stringp upcase]
                  [integerp number-to-string]
                  [t (lambda (value) (format "other: %s" value))])))
    (should (equal (funcall convert "hello") "HELLO"))
    (should (equal (funcall convert 42) "42"))
    (should (equal (funcall convert 'symbol) "other: symbol"))))

(ert-deftest fp-test-cond-accepts-list-clauses-and-multiple-arguments ()
  (let ((classify
         (fp-cond
           ((lambda (x y) (= x y)) (lambda (&rest _args) 'equal))
           ((lambda (x y) (< x y)) (lambda (&rest _args) 'ascending)))))
    (should (eq (funcall classify 2 2) 'equal))
    (should (eq (funcall classify 2 3) 'ascending))
    (should-not (funcall classify 3 2))))

(ert-deftest fp-test-not-negates-a-predicate ()
  (let ((not-string (fp-not stringp)))
    (should (funcall not-string 10))
    (should-not (funcall not-string "text"))))

(ert-deftest fp-test-boolean-constants-ignore-arguments ()
  (should (eq (fp-t) t))
  (should (eq (fp-t nil 1 "value") t))
  (should-not (fp-nil))
  (should-not (fp-nil t 1 "value")))

(ert-deftest fp-test-ignore-errors-partial-prepends-and-suppresses-errors ()
  (let ((divide-ten (fp-ignore-errors-partial / 10)))
    (should (= (funcall divide-ten 2) 5))
    (should-not (funcall divide-ten 0))))

(ert-deftest fp-test-ignore-errors-rpartial-appends-and-suppresses-errors ()
  (let ((divide-by-two (fp-ignore-errors-rpartial / 2))
        (divide-by-zero (fp-ignore-errors-rpartial / 0)))
    (should (= (funcall divide-by-two 10) 5))
    (should-not (funcall divide-by-zero 10))))

(ert-deftest fp-test-partial-ignore-errors-function-variant ()
  (let ((divide-ten (fp-partial-ignore-errors #'/ 10)))
    (should (= (funcall divide-ten 2) 5))
    (should-not (funcall divide-ten 0))))

(ert-deftest fp-test-rpartial-ignore-errors-function-variant ()
  (let ((divide-by-two (fp-rpartial-ignore-errors #'/ 2))
        (divide-by-zero (fp-rpartial-ignore-errors #'/ 0)))
    (should (= (funcall divide-by-two 10) 5))
    (should-not (funcall divide-by-zero 10))))

(provide 'fp-test)
;;; fp-test.el ends here

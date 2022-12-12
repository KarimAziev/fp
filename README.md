# fp

A library of point-free style combinators for Emacs Lisp.

# Table of Contents

>   - [fp](#fp)
>       - [Requirements](#requirements)
>       - [Installation](#installation)
>           - [Manually](#manually)
>           - [With use-package and
>             straight](#with-use-package-and-straight)
>       - [Usage](#usage)
>           - [Macros](#macros)
>               - [`fp-pipe` (\&rest
>                 functions)](#fp-pipe-rest-functions)
>               - [`fp-compose` (\&rest
>                 functions)](#fp-compose-rest-functions)
>               - [`fp-partial` (fn \&rest
>                 args)](#fp-partial-fn-rest-args)
>               - [`fp-rpartial` (fn \&rest
>                 args)](#fp-rpartial-fn-rest-args)
>               - [`fp-and` (\&rest functions)](#fp-and-rest-functions)
>               - [`fp-or` (\&rest functions)](#fp-or-rest-functions)
>               - [`fp-converge` (combine-fn \&rest
>                 functions)](#fp-converge-combine-fn-rest-functions)
>               - [`fp-use-with` (combine-fn \&rest
>                 functions)](#fp-use-with-combine-fn-rest-functions)
>               - [`fp-when` (pred fn)](#fp-when-pred-fn)
>               - [`fp-unless` (pred fn)](#fp-unless-pred-fn)
>               - [`fp-const` (value)](#fp-const-value)
>               - [`fp-ignore-args` (fn)](#fp-ignore-args-fn)
>               - [`fp-not` (fn)](#fp-not-fn)
>               - [`fp-cond` (\&rest pairs)](#fp-cond--rest-pairs)
>           - [Functions](#functions)
>               - [`fp-nil` (\&rest ignored)](#fp-nil--rest-ignored)
>               - [`fp-t` (\&rest ignored)](#fp-t--rest-ignored)

## Requirements

  - Emacs \>= 26.1

## Installation

### Manually

Download repository and it to your load path in your init file:

``` elisp
(add-to-list 'load-path "/path/to/fp")
(require 'fp)
```

### With use-package and straight

``` elisp
(use-package fp :straight (:repo "KarimAziev/fp" :type git :host github))
```

## Usage

### Macros

#### `fp-pipe` (\&rest functions)

Return a left-to-right composition from `functions`. The first argument
may have any arity; the remaining arguments must be unary.

``` elisp
(funcall (fp-pipe upcase split-string) "some string")
;; ⇒ ("SOME" "STRING")
```

#### `fp-compose` (\&rest functions)

Return a right-to-left composition from `functions`. The last function
may have any arity; the remaining arguments must be unary.

``` elisp
(funcall (fp-compose split-string upcase) "some string")
;; ⇒ ("SOME" "STRING")
```

#### `fp-partial` (fn \&rest args)

Return a partial application of a function `fn` to left-hand `args`.

Arguments `args` is a list of the last `n` arguments to pass to `fn`.
The result is a new function that does the same as `fn`, except that the
last `n` arguments are fixed at the values with which this function was
called.

``` elisp
(funcall (fp-partial > 3) 2) ;; ⇒ t
```

#### `fp-rpartial` (fn \&rest args)

Return a partial application of a function `fn` to right-hand arguments
`args`.

Arguments `args` is a list of the last `n` arguments to pass to `fn`.

The result is a new function which does the same as `fn`, except that
the last `n` arguments are fixed at the values with which this function
was called.

``` elisp
(funcall (fp-rpartial plist-get :name) '(:name "John" :age 30))
;; ⇒ "John"
```

#### `fp-and` (\&rest functions)

Return a unary function that calls `functions` until one of them yields
nil. If all functions return non-nil, return the last such value.

``` elisp
(let ((x 30))
  (funcall (fp-and numberp 1+) x))
;; ⇒ 31
```

#### `fp-or` (\&rest functions)

Expand to a unary function that calls `functions` until first non-nil
result. Return that first non-nil result without calling the remaining
functions. If all functions returned nil, the result will be also nil.

``` elisp
(seq-filter
 (fp-or numberp stringp)
 '("a" "b" (0 1 2 3 4) "c" 34 (:name "John" :age 30)))

;; ⇒ ("a" "b" "c" 34)
```

#### `fp-converge` (combine-fn \&rest functions)

Return a function to apply `combine-fn` with the results of branching
`functions`. If the first element of `functions` is a vector, it will be
used instead.

``` elisp
(funcall (fp-converge concat [upcase downcase]) "John")
;; ⇒ "JOHNjohn"
```

If first element of `functions` is a vector, it will be used instead.

``` elisp
(funcall (fp-converge concat upcase downcase) "John")
;; ⇒ "JOHNjohn"
```

#### `fp-use-with` (combine-fn \&rest functions)

Return a function with the arity of length `functions`. Call every
branching function with an argument at the same index, and finally,
`combine-fn` will be applied to the supplied values.

``` elisp
(funcall (fp-use-with concat [upcase downcase]) "hello " "world")
  ;;  ⇒ "HELLO world"
```

If first element of `functions` is a vector, it will be used instead.

``` elisp
(funcall (fp-use-with concat upcase downcase) "hello " "world")
;;   ⇒ "HELLO world"
```

#### `fp-when` (pred fn)

Return a function that calls `fn` if the result of calling `pred` is
non-nil. Both `pred` and `fn` are called with one argument. If the
result of `pred` is nil, return the argument as is.

``` elisp
(defun truncate-maybe (str len)
  "Truncate STR if longer LEN, otherwise return STR."
  (funcall (fp-when
            (fp-compose (fp-partial < len) length)
            (fp-rpartial substring 0 len))
           str))

(list (truncate-maybe "long string" 4)
      (truncate-maybe "lo" 4))

;; ⇒ ("long" "lo")
```

#### `fp-unless` (pred fn)

Return a function that calls `fn` if the result of calling `pred` is
non-nil.

Both `pred` and `fn` are called with one argument.

If the result of `pred` is nil, return the argument as is.Return a unary
function that invokes `fn` if the result of calling `pred` is nil.
Accept one argument and pass it both to `pred` and `fn`. If the result
of `pred` is non-nil, return the argument as is.

``` elisp
(defun divide-maybe (a b)
  "Divide A and B unless B is 0."
  (funcall (fp-unless zerop
                       (fp-partial / a))
           b))

(list (divide-maybe 10 0)
      (divide-maybe 10 2))

;; ⇒ '(0 5)
```

#### `fp-const` (value)

Return a function that always returns `value`. This function accepts any
number of arguments but ignores them.

``` elisp
(funcall (fp-const 2) 4) ;; ⇒ 2
```

#### `fp-ignore-args` (fn)

Return a function that invokes `fn` without args. This function accepts
any number of arguments but ignores them.

``` elisp
(defun my-fn ()
  "Show message hello world."
  (message "Hello world"))

(funcall (fp-ignore-args my-fn) 4) ;;   ⇒ "Hello world"
```

#### `fp-not` (fn)

Return a function that negates the result of function `fn`.

``` elisp
(funcall (fp-not stringp) 4) ;;   ⇒ t
```

#### `fp-cond` (\&rest pairs)

Return a function that expands a list of `pairs` to cond clauses. Every
pair should be either:

  - a vector of \[predicate transformer\]

<!-- end list -->

``` elisp
(funcall (fp-cond
           [stringp identity]
           [symbolp symbol-name]
           [integerp number-to-string]
           [floatp number-to-string]
           [t (fp-partial format "%s")])
         2)

;;   ⇒ 4
```

  - a list of (predicate transformer).

<!-- end list -->

``` elisp
(funcall (fp-cond
           (stringp identity)
           (symbolp symbol-name)
           (integerp number-to-string)
           (floatp number-to-string)
           (t (fp-partial format "%s")))
         2)
;;   ⇒ 4
```

The predicate can also be `t`.

All of the arguments to function are applied to each of the predicates
in turn until one returns a "truthy" value, at which point fn returns
the result of applying its arguments to the corresponding transformer.

### Functions

#### `fp-nil` (\&rest ignored)

Do nothing and return `t`. This function accepts any number of arguments
but ignores them.

``` example
(fp-nil t)
    ⇒ nil
(fp-nil 23)
    ⇒ nil
(fp-nil)
   ⇒ nil
```

#### `fp-t` (\&rest ignored)

Do nothing and return `t`. This function accepts any number of
arguments, but ignores them.

``` example
(fp-t nil)
   ⇒ t
 (fp-t)
   ⇒ t
 (fp-t 23)
   ⇒ t
```

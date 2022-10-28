# fp

- [Requirements](#requirements)
- [Installation](#installation)
  - [Manually](#manually)
  - [With use-package and straight](#with-use-package-and-straight)
- [Usage](#usage)
  - [fp-pipe (\&rest functions)](#fp-pipe-rest-functions)
  - [fp-compose (\&rest functions)](#fp-compose-rest-functions)
  - [fp-partial (fn \&rest args)](#fp-partial-fn-rest-args)
  - [fp-rpartial (fn \&rest args)](#fp-rpartial-fn-rest-args)
  - [fp-and (\&rest functions)](#fp-and-rest-functions)
  - [fp-or (\&rest functions)](#fp-or-rest-functions)
  - [fp-converge (combine-fn \&rest
    functions)](#fp-converge-combine-fn-rest-functions)
  - [fp-use-with (combine-fn \&rest
    functions)](#fp-use-with-combine-fn-rest-functions)
  - [fp-when (pred fn)](#fp-when-pred-fn)
  - [fp-unless (pred fn)](#fp-unless-pred-fn)
  - [fp-const (value)](#fp-const-value)
  - [fp-ignore-args (fn)](#fp-ignore-args-fn)

## Requirements

- Emacs \>= 26.1

## Installation

### Manually

Download repository and it to your load path in your init file:

```elisp

(add-to-list 'load-path "/path/to/fp)

(require 'fp)

```

### With use-package and straight

```elisp

(use-package fp
    :straight (:repo "KarimAziev/fp" :type git :host github))

```

## Usage

### fp-pipe (\&rest functions)

Return left-to-right composition from `functions`.

**Example:**

```elisp
(funcall (fp-pipe upcase split-string) "some string")
```

**Result:**

```elisp
("SOME" "STRING")
```

### fp-compose (\&rest functions)

Return right-to-left composition from `functions`.

**Example:**

```elisp
(funcall (fp-compose split-string upcase) "some string")
```

**Result:**

```elisp
("SOME" "STRING")
```

### fp-partial (fn \&rest args)

Return a partial application of `fn` to left-hand `args`.

`args` is a list of the last N arguments to pass to `fn`. The result is
a new function which does the same as `fn`, except that the last N
arguments are fixed at the values with which this function was called.

**Example:**

```elisp
(funcall (fp-partial > 3) 2)
```

**Result:**

```elisp
t
```

### fp-rpartial (fn \&rest args)

Return a partial application of `fn` to right-hand `args`.

`args` is a list of the last N arguments to pass to `fn`. The result is
a new function which does the same as `fn`, except that the last N
arguments are fixed at the values with which this function was called.

**Example:**

```elisp
(funcall (fp-rpartial > 3) 2)
```

**Result:**

```elisp
nil
```

**Example:**

```elisp
(funcall (fp-rpartial plist-get :name) '(:name "John" :age 30))
```

**Result:**

```elisp
"John"
```

### fp-and (\&rest functions)

Return an unary function which call invoke `functions` until one of them
yields nil.

**Example:**

```elisp
(funcall (fp-and numberp 1+) 30)
```

**Result:**

```elisp
31
```

### fp-or (\&rest functions)

Return a function that `functions` until one of them yields non-nil.

**Example:**

```elisp
(seq-filter
 (fp-or numberp stringp)
 '("a" "b" (0 1 2 3 4) "c" 34 (:name "John" :age 30)))
```

**Result:**

```elisp
("a" "b" "c" 34)
```

### fp-converge (combine-fn \&rest functions)

Return a new function that accepts a converging function COMBINE-FN and
a list of branching `functions`.

When invoked, this new function is applied to some arguments, and each
branching function is applied to those same arguments. The results of
each branching function are passed as arguments to the converging
function to produce the return value.

For example here both `upcase` and `downcase` applied with argument
John, and `concat` applied with results.

**Example:**

```elisp
(funcall (fp-converge concat [upcase downcase]) "John")
⇒ "JOHNjohn"
```

If first element of `functions` is a vector, it will be used instead.

**Example:**

```elisp
(funcall (fp-converge concat upcase downcase) "John")
⇒ "JOHNjohn"
```

### (fp-use-with combine-fn \&rest functions)

Return a function with the arity of length `functions`.

This function will apply `combine-fn` with results of every function
called with **one** argument at the same index .

**Example:**

```elisp

(funcall (fp-use-with concat [upcase downcase]) "hello " "world")
  ;;  ⇒ "HELLO world"
```

If first element of `functions` is a vector, it will be used instead.

```elisp

(funcall (fp-use-with concat upcase downcase) "hello " "world")
;;   ⇒ "HELLO world"
```

### fp-when (pred fn)

Return an unary function that invoke `fn` if result of calling PRED is
non-nil.

If result of PRED is nil, return the argument as is.

Both PRED and `fn` called with one argument.

```elisp
(defun truncate-maybe (str len)
  "Truncate STR if longer LEN, otherwise return STR."
  (funcall (fp-when
            (fp-compose (fp-partial < len) length)
            (fp-rpartial substring 0 len))
           str))

(list (truncate-maybe "long string" 4)
      (truncate-maybe "lo" 4))
```

**Result:**

```elisp
("long" "lo")
```

### fp-unless (pred fn)

Return an unary function that invoke `fn` if result of calling PRED is
non-nil.

If result of PRED is nil, return the argument as is.

Both PRED and `fn` called with one argument.

```elisp
(defun divide-maybe (a b)
  "Divide A and B unless B is 0."
  (funcall (fp-unless zerop
                       (fp-partial / a))
           b))

(list (divide-maybe 10 0)
      (divide-maybe 10 2))
```

**Result**:

```elisp
(0 5)
```

### fp-const (value)

Return a function that always return `value.`

This function accepts any number of arguments, but ignores them.

```elisp
(funcall (fp-const 2) 4)
```

**Result**:

```elisp
2
```

### fp-ignore-args (fn)

Return a function that invoke `fn` without args.

This function accepts any number of arguments, but ignores them.

```elisp
(defun my-fn ()
  "Show message hello world."
  (message "Hello world"))

(funcall (fp-ignore-args my-fn) 4)
```

**Result**:

```elisp
"Hello world"
```

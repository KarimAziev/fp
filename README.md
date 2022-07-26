# fp

  - [Macros](#macros)
      - [fp–pipe (\&rest functions)](#fp--pipe-rest-functions)
      - [fp–compose (\&rest functions)](#fp--compose-rest-functions)
      - [fp–partial (fn \&rest args)](#fp--partial-fn-rest-args)
      - [fp–rpartial (fn \&rest args)](#fp--rpartial-fn-rest-args)
      - [fp–and (\&rest functions)](#fp--and-rest-functions)
      - [fp–or (\&rest functions)](#fp--or-rest-functions)
      - [fp–converge (combine-fn \&rest
        functions)](#fp--converge-combine-fn-rest-functions)
  - [Functions](#functions)
      - [fp-pipe (\&rest functions)](#fp-pipe-rest-functions)
      - [fp-compose (\&rest functions)](#fp-compose-rest-functions)
      - [fp-partial (fn \&rest args)](#fp-partial-fn-rest-args)
      - [fp-rpartial (fn \&rest args)](#fp-rpartial-fn-rest-args)

## Macros

### fp–pipe (\&rest functions)

Return left-to-right composition from FUNCTIONS.

**Example:**

``` commonlisp
(funcall (fp--pipe upcase split-string) "some string")
```

**Result:**

``` commonlisp
("SOME" "STRING")
```

### fp–compose (\&rest functions)

Return right-to-left composition from FUNCTIONS.

**Example:**

``` commonlisp
(funcall (fp--compose split-string upcase) "some string")
```

**Result:**

``` commonlisp
("SOME" "STRING")
```

### fp–partial (fn \&rest args)

Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a
new function which does the same as FN, except that the last N arguments
are fixed at the values with which this function was called.

**Example:**

``` elisp
(funcall (fp--partial > 3) 2)
```

**Result:**

``` elisp
t
```

### fp–rpartial (fn \&rest args)

Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a
new function which does the same as FN, except that the last N arguments
are fixed at the values with which this function was called.

**Example:**

``` elisp
(funcall (fp--rpartial > 3) 2)
```

**Result:**

``` elisp
nil
```

**Example:**

``` elisp
(funcall (fp--rpartial plist-get :name) '(:name "John" :age 30))
```

**Result:**

``` elisp
"John"
```

### fp–and (\&rest functions)

Return an unary function which call invoke FUNCTIONS until one of them
yields nil.

**Example:**

``` elisp
(funcall (fp--and numberp 1+) 30)
```

**Result:**

``` elisp
31
```

### fp–or (\&rest functions)

Return a function that FUNCTIONS until one of them yields non-nil.

**Example:**

``` elisp
(seq-filter
 (fp--or numberp stringp)
 '("a" "b" (0 1 2 3 4) "c" 34 (:name "John" :age 30)))
```

**Result:**

``` elisp
("a" "b" "c" 34)
```

### fp–converge (combine-fn \&rest functions)

Return a new function that accepts a converging function COMBINE-FN and
a list of branching FUNCTIONS.

When invoked, this new function is applied to some arguments, and each
branching function is applied to those same arguments. The results of
each branching function are passed as arguments to the converging
function to produce the return value.

If first element of FUNCTIONS is a vector, it will be used instead.

For example here both `upcase` and `downcase` applied with argument
John, and `concat` applied with results.

**Example:**

``` commonlisp
(funcall (fp--converge concat [upcase downcase]) "John")
```

**Result:**

``` commonlisp
"JOHNjohn"
```

**Example:**

``` commonlisp
(funcall (fp--converge concat upcase downcase) "John")
```

**Result:**

``` commonlisp
"JOHNjohn"
```

## Functions

### fp-pipe (\&rest functions)

Return left-to-right composition from FUNCTIONS.

**Example:**

``` commonlisp
(funcall (fp-pipe #'upcase #'split-string) "some string")
```

**Result:**

``` commonlisp
("SOME" "STRING")
```

### fp-compose (\&rest functions)

Return right-to-left composition from FUNCTIONS.

**Example:**

``` commonlisp
(funcall (fp-compose #'split-string #'upcase) "some string")

```

**Result:**

``` commonlisp
("SOME" "STRING")
```

### fp-partial (fn \&rest args)

Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a
new function which does the same as FN, except that the last N arguments
are fixed at the values with which this function was called.

**Example:**

``` elisp
(funcall (fp-partial #'> 3) 2)
```

**Result:**

``` elisp
t
```

### fp-rpartial (fn \&rest args)

Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a
new function which does the same as FN, except that the last N arguments
are fixed at the values with which this function was called.

**Example:**

``` elisp
(funcall (fp-rpartial #'> 3) 2)
```

**Result:**

``` elisp
nil
```

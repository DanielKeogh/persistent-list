# persistent-list

A fast Persistent/Immutable List implementation.

## Usage

Given that `persistent-list` collides with several important function definitions in the `:common-lisp` namespace it is recommended that thsi library is used with a local nickname. For example, like this:

```lisp
(defpackage my-package
    (:use #:cl)
    (:local-nicknames (#pl:persistent-list)))
```

**Constructor: list**

```lisp
(pl:list 0 1 2 3)
;; (0 1 2 3)

(pl:list)
;; nil
```

**Prepend: cons**

```lisp
(pl:cons 1 nil)
;; (1)
(pl:cons 1 (pl:list 2 3))
;; (1 2 3)
```

**First: first**

```lisp
(pl:first (pl:list 1 2 3))
;; 1
(pl:first nil)
;; nil
```

**Rest:**

```lisp
(pl:rest (pl:list 1 2 3))
;; (2 3)
(pl:rest nil)
;; nil
```

```lisp
(pl:pop (pl:list 1 2 3))
;; (2 3)
(pl:pop nil)
;; error
```

**Nth:**

```lisp
(pl:nth (pl:list "a" "b" "c") 1)
;; "b"
(pl:nth nil 1)
;; nil
```

**Length**

```lisp
(pl:length (pl:list 1 2 3))
;; 3
(pl:length nil)
;; nil
```

**Equality**

```lisp
(pl:list-equal (pl:list 1 2 3) (pl:list 1 2 3))
;; t
(pl:list-equal nil nil)
;; nil
```

**Map**

```lisp
(pl:map '1+ (pl:list 1 2 3))
;; (2 3 4)
```

**Reduce**

```lisp
(pl:reduce '+ (pl:list 1 2 3))
;; 6
(pl:reduce '+ (pl:list 1 2 3) 10)
;; 16
```

**Dolist**

```lisp
(pl:dolist (x (pl:list 1 2 3))
   (print x))
;; "1"
;; "2"
;; "3"
```

**Type checking**

```lisp
(pl:persistent-lisp-p (pl:list 1 2 3))
;; t
(pl:persistent-lisp-p nil)
;; t
```
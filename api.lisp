;;;; api.lisp

(in-package :persistent-list)

(defun list (&rest items)
  "Return a persistent list containing the following items."
  (build-list items))

(defun cons (first rest)
  "Prefix the list with the first argument. rest must be a persistent-list or nil."
  (lst-cons first rest))

(defun list-equal (lst1 lst2 &key (test 'eql))
  "Return true when the two lists are of equal length, and all of their elements are equal using the test parameter."
  (or (not (and lst1 lst2))
      (and (= (pl-count lst1) (pl-count lst2))
	   (loop for l1 = lst1 then (pl-rest l1)
		 for l2 = lst2 then (pl-rest l2)
		 while l1
		 always (funcall test (pl-first l1) (pl-first l2))))))

(defun first (lst)
  "Access the first item in the persistent-list."
  (when lst (pl-first lst)))

(defun rest (lst)
  "Return the list without the first item."
  (when lst (pl-rest lst)))

(defun pop (lst)
  "Return the list without the first item. If the list is empty an error will occur."
  (pl-rest lst))

(defun nth (lst n)
  "Get the nth item in the list. 
If lst is nil returns nil, no matter what interger n is.
Otherwise if n is out of bounds, and error will occur."
  (when lst
    (when (or (< n 0)
	      (>= n (pl-count lst)))
      (error "Index out of bounds"))
    (pl-first (lst-cdr-n lst n))))

(defun length (lst)
  "Return the length of the list. If list is nil return 0."
  (if lst
      (pl-count lst)
      0))

(defun persistent-list-p (lst)
  "Ensure the type of lst is a persistent-list or nil."
  (or (null lst)
      (typep lst 'persistent-list)))

(defun reduce (fn list &optional (initial-value nil initial-value-p))
  "Apply a function to accumulate all members of a persistent-list into a cached value."
  (if list
    (loop
      for val = (if initial-value-p initial-value (pl-first list))
	then (funcall fn val (pl-first lst))
      for lst = (if initial-value-p list (pl-rest list))
	then (pl-rest lst)
      while lst
      finally (return val))
    initial-value))

(defun map (fn list)
  "Apply a function to all items in a persistent-list to make a new persistent-list of the same length."
  (pl-map fn list))

(defmacro dolist ((var list &optional result) &body body)
  "A macro for traversing all elements of the list."
  (let ((lst (gensym)))
    `(when ,list 
       (loop repeat (persistent-list::pl-count ,list)
	     for ,lst = ,list then (persistent-list::pl-rest ,lst)
	     for ,var = (persistent-list::pl-first ,lst)
	     do (progn ,@body)
	     finally (return ,result)))))

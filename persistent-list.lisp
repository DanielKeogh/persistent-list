;;;; persistent-list.lisp

(in-package #:persistent-list)

(defstruct (persistent-list (:conc-name pl-)
			    (:constructor make-pl (first rest count))
			    (:predicate nil))
  (first (required-argument :first))
  (rest (required-argument :rest) :type (or null persistent-list))
  (count (required-argument :count) :type fixnum))

(defmacro with-list ((first rest count) lst &body body)
  `(with-accessors ((,first pl-first)
		    (,rest pl-count)
		    (,count pl-count))
       ,lst
     ,@body))

(defun lst-cons (first rest)
  (if rest
      (make-pl first rest (1+ (pl-count rest)))
      (make-pl first rest 1)))

(defun build-list (items)
  (loop for item in (nreverse items)
	for lst = (make-pl item nil 1)
	  then (lst-cons item lst)
	finally (return lst)))

(defun lst-cdr-n (lst n)
  (loop for v = lst then (pl-rest v)
	repeat n
	finally (return v)))

(defun list-make-iterator (lst &optional (start 0) (end nil))
  (let ((i start)
	(remaining (lst-cdr-n lst start))
	(r-end (or end (pl-count lst))))
    (lambda ()
      (when (< i r-end)
	(let ((element (pl-first remaining)))
	  (setf remaining (pl-rest remaining))
	  (incf i)
	  (values t element))))))

(defun pl-map (fn list)
  (when list
    (loop for i from (pl-count list) downto 1
	  for lst = list then (pl-rest lst)
	  for v = (funcall fn (pl-first lst))
	  for start = (make-pl v nil i) then start
	  for r = start
	    then (let ((new (make-pl v nil i)))
		   (setf (pl-rest r) new)
		   new)
	  finally (return start))))

(defmethod print-object ((lst persistent-list) stream)
  (write-char #\( stream)
  (loop with itr = (list-make-iterator lst)
	with print-length = (and (not *print-readably*) *print-length*)
	for (remaining val) = (multiple-value-list (funcall itr)) then (cl:list next-remaining next-val)
	for count from 0
	while remaining
	for (next-remaining next-val) = (multiple-value-list (funcall itr))
	when (and (numberp print-length) (>= count print-length))
	  do
	     (print "..." stream)
	     (return)
	end
	do (prin1 val stream)
	   (when next-remaining (write-char #\  stream)))
  (write-char #\) stream))

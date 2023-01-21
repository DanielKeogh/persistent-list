;;;; tests/main.lisp

(in-package #:persistent-list-tests)

(def-suite all-tests
  :description "Main test suite for persisten-list")

(in-suite all-tests)

(test list
  (is (pl:persistent-list-p (pl:list)))
  (is (pl:persistent-list-p (pl:list 1 2 3)))
  (is (pl:persistent-list-p (pl:list nil nil nil)))
  (is (pl:persistent-list-p (apply #'pl:list (loop for i from 1 to 50 collect i)))))

(test nth
  (let ((l (pl:list 0 1 2 3 4 5 6 7 8 9)))
    (is (= 0 (pl:nth l 0)))
    (is (= 3 (pl:nth l 3)))
    (is (= 9 (pl:nth l 9)))))

(test sequence-equal
  (is (pl:list-equal (pl:list) (pl:list)))
  (is (pl:list-equal (pl:list 1 2 3) (pl:list 1 2 3)))
  (is (pl:list-equal (pl:list "cad" "bad" "sad") (pl:list "cad" "bad" "SAD") :test 'equalp)))

(test cons
  (is (pl:list-equal (pl:cons nil nil) (pl:list nil)))
  (is (pl:list-equal (pl:cons 1 (pl:list 2 3)) (pl:list 1 2 3))))

(test first
  (is (= 1 (pl:first (pl:list 1 2 3))))
  (is (= 1 (pl:first (pl:cons 1 nil)))))

(test rest
  (is (null (pl:rest nil)))
  (is (null (pl:rest (pl:list 1))))
  (is (pl:list-equal (pl:list 2 3) (pl:rest (pl:list 1 2 3))))
  (is (pl:list-equal (pl:list 3) (pl:rest (pl:rest (pl:list 1 2 3)))))
  (is (= 2 (pl:length (pl:rest (pl:list 1 2 3))))))

(test pop
  (is (pl:list-equal nil (pl:pop (pl:list 1))))
  (is (pl:list-equal (pl:list 2) (pl:pop (pl:list 1 2))))
  (is (pl:list-equal (pl:list 1 2 3) (pl:pop (pl:pop (pl:list 1 2)))))
  (signals error (pl:pop nil)))

(test length
  (is (= 0 (pl:length nil)))
  (is (= 0 (pl:length (pl:list))))
  (is (= 1 (pl:length (pl:list 1))))
  (is (= 2 (pl:length (pl:list 1 2))))
  (is (= 3 (pl:length (pl:list 1 2 3))))
  (is (= 50 (pl:length (apply #'pl:list (loop for i from 1 to 50 collect i))))))

(test map
  (is (null (pl:map '1+ nil)))
  (is (pl:list-equal (pl:list 2 3 4) (pl:map '1+ (pl:list 1 2 3))))
  (is (= 3 (pl:length (pl:map '1+ (pl:list 1 2 3)))))
  (is (= 2 (pl:length (pl:rest (pl:map '1+ (pl:list 1 2 3)))))))

(test dolist
  (is (= 6 (let ((aggr 0))
	     (pl:dolist (x (pl:list 1 2 3))
	       (incf aggr x))
	     aggr)))
  (is (= 6 (let ((aggr 0))
	     (pl:dolist (x (pl:list 1 2 3) aggr)
	       (incf aggr x)))))
  (is (null (pl:dolist (x nil)))))

(defun factorial (n) 
  (loop for i from 1 to n
	for j = i then (* i j)
	finally (return j)))

(test reduce
  (is (= (factorial 5) (pl:reduce (lambda (x y) (* x y)) (pl:list 1 2 3 4 5))))
  (is (= (* 10 (factorial 5)) (pl:reduce (lambda (x y) (* x y)) (pl:list 1 2 3 4 5) 10)))
  (is (null (pl:reduce nil nil))))

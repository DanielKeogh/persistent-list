;;;; tests/package.lisp

(defpackage #:persistent-list-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:pl #:persistent-list))
  (:export #:run!
	   #:all-tests))

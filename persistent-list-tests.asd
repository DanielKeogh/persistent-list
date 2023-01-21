;;;; persistent-list-tests.asdf

(asdf:defsystem #:persistent-list-tests
  :description "Tests for persistent-list"
  :author "Daniel Keogh"
  :license "Eclipse 2.0"
  :depends-on  (:persistent-list :fiveam)
  :components ((:module "tests"
		:serial t
		:components ((:file "package")
			     (:file "main")))))

;;;; persistent-list.asd

(asdf:defsystem #:persistent-list
  :description "Persistent/Immutable List datastructure based upon Clojure"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "persistent-list")
	       (:file "api")))

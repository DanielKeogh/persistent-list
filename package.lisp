;;;; package.lisp

(defpackage #:persistent-list
  (:documentation  "Persistent/Immutable List datastructure based upon Clojure")
  (:use #:cl)
  (:shadow
   #:length
   #:list
   #:first
   #:rest
   #:push
   #:pop
   #:nth
   #:cons
   #:dolist
   #:map
   #:reduce
   )
  (:export
   ;; constructors
   #:list
   #:cons

   ;; traversal
   #:first
   #:rest
   #:nth
   #:pop

   ;; metadata
   #:length

   ;; equality
   #:list-equal

   ;; looping
   #:map
   #:reduce
   #:dolist

   ;; types
   #:persistent-list-p
   #:persistent-list
   ))

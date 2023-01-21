;;;; utils.lisp

(in-package :persistent-list)

(defun required-argument (name)
  (error "Required arugment ~@[~S~] missing." name))

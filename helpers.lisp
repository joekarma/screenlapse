;;; helpers.lisp

(in-package :screenlapse)

(defun sym (string-or-something)
  (intern
   (string-upcase
    (format nil "~a" string-or-something))))

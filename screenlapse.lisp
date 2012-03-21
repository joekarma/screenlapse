;;;; screenlapse.lisp

(in-package #:screenlapse)

(defvar *active-thread* ()) ; '(bt-thread thread-death-function)
(defvar *default-screenlapse-directory* (merge-pathnames #p"Pictures/screenlapses/"
							 (user-homedir-pathname)))

(defun record-for-n-minutes (n)
  "Records a screenlapse for N minutes."
  (bt:make-thread
   (lambda ()
     (let ((thread-sexp (start-recording)))
       (sleep (* n 60))
       (stop-recording thread-sexp)))))

(defun recording-p (&optional (thread-sexp *active-thread*))
  "Returns T if screenlapse is currently recording the screen."
  (and *active-thread* (bt:thread-alive-p (first thread-sexp))))

(defun make-screenlapse-directory (&key (root-directory *default-screenlapse-directory*))
  "Generates a new directory for screencaptures. Directory name
   (which is returned) is whatever the universal time at time
   of call."
  (let ((dir (merge-pathnames
	      (pathname (format nil "~a/" (get-universal-time)))
	      root-directory)))
    (ensure-directories-exist dir)
    dir))

(defun start-recording (&key (root-directory *default-screenlapse-directory*))
  "Record a new screencapture every 10 seconds to ROOT-DIRECTORY or
   *DEFAULT-SCREENLAPSE-DIRECTORY*"
  (let* ((dir (make-screenlapse-directory :root-directory root-directory))
	 (thread (bt:make-thread (lambda () (record-indefinitely dir))))
	 (thread-killer (lambda () (bt:destroy-thread thread))))
    (stop-recording)			; just in case
    (setf *active-thread* (list thread thread-killer))))

(defun stop-recording (&optional (thread-sexp *active-thread*))
  "If a screenlapse is being recorded, stop."
  (when (recording-p thread-sexp)
    (funcall (second thread-sexp)) ; call the thread killing function
    ))

(defun record-indefinitely (dir)
  "Record screencaptures in `dir` indefinitely."
  (loop
     for i from 0 do
       (save-screenshot-to-directory dir i)
       (sleep 10)))

(defun save-screenshot-to-directory (directory screenshot-number)
  (etypecase directory
    (pathname directory)
    (string (pathname directory)))
  (assert (null (pathname-name directory)))
  (assert (probe-file directory))
  (let* ((file-name (write-to-string screenshot-number))
	 (path-to-new-image (merge-pathnames (make-pathname :name file-name
							    :type "png")
					     directory)))
    (external-program:run "screencapture"
			  `("-x"
			    ,path-to-new-image))))



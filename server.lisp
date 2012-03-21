;;;; server.lisp

(in-package #:screenlapse)

;;; Routing

(defvar *routes* nil)

(defun add-route (pair)
  (setf *routes* (append *routes* pair)))

(defmacro defroutes (&body routes)
  `(setf *routes*
	 (list 
	  ,@(loop for (uri function) on routes by #'cddr
	       appending `(,(intern (format nil "~a" uri) 'keyword) ',function)))))

(defun uri-to-key (uri)
  (intern (string-upcase uri) 'keyword))

(defmethod hunchentoot:handle-request ((acc acceptor) request-object)
  (let* ((uri (hunchentoot:request-uri request-object)))
    (awhen (getf *routes* (uri-to-key uri))
      (funcall it request-object))))







;;; Acceptor setup (so as not to interfere with other websites)

(defclass acceptor (hunchentoot:acceptor) ())

(let* ((project-directory (asdf:system-source-directory :screenlapse))
       (access-log-directory (merge-pathnames #p"logs/access.log"
					      project-directory))
       (message-log-directory (merge-pathnames #p"logs/message.log"
					       project-directory))
       (document-root (merge-pathnames #p"htdocs/"
				       project-directory)))
  (defvar *acceptor* (make-instance 'acceptor :port 7276
				    :access-log-destination access-log-directory
				    :document-root document-root
				    :message-log-destination message-log-directory)))






;;; Basic commands

(defun start-control-panel ()
  (hunchentoot:start *acceptor*))

(defun stop-control-panel ()
  (hunchentoot:stop *acceptor*))

(defun restart-control-panel ()
  (stop-control-panel)
  (start-control-panel))





;;; Web pages

(defun index (req)
  (declare (ignorable req))
  (who:with-html-output-to-string (s)
     (:html
      (:head
       (:title "Screenlapse Control Panel"))
      (:body
       (:h1 "Screenlapse")
       (:div (:p "Record timelapse videos of your computer screen. Each generated video corresponds to a 25-minute timeblock. Because it is a timelapse, the video itself will be over in a matter of seconds. The 25-minute cap is to encourage adoption of the \"pomodoro method\".")
	     (:div (:a :href "/start" "Start recording"))
	     (:div (:a :href "/stop" "Stop recording")))))))

(defun start-recording-page (req)
  (record-for-n-minutes 25)
  "Recording has started!")

(defun stop-recording-page (req)
  (stop-recording)
  "Recording has stopped!")




;;;; package.lisp

(defpackage #:screenlapse
  (:use #:cl #:anaphora)
  (:export #:*acceptor*
	   #:start-control-panel ; control panel is accessed via web. server is started on port 7276
	   #:stop-control-panel
	   #:start-recording
	   #:stop-recording))

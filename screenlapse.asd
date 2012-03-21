;;;; screenlapse.asd

(asdf:defsystem #:screenlapse
  :serial t
  :components ((:file "package")
	       (:file "helpers")
	       (:file "server")
               (:file "screenlapse")
	       (:file "routes"))
  :depends-on (:external-program
	       :hunchentoot
	       :bordeaux-threads :cl-who
	       :css-lite
	       :anaphora))

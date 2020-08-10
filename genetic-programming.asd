;;;; genetic-programming.asd

(asdf:defsystem #:genetic-programming
  :author "Gustavo Pacheco <gap1512@gmail.com>"
  :version "0.0.1"
  ;:depends-on (#:trivial-arguments)
  :serial t
  :components ((:file "package")
               (:file "genetic-programming")
	       (:file "symbolic-regression")))

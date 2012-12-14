;;;; wo-graph-functions.asd

(asdf:defsystem #:wo-graph-functions
  :serial t
  :depends-on (#:wo-graph #:wo-util #:fset)
  :components ((:file "package")
	       (:file "vertex-functions")
	       (:file "paths")
	       (:file "simplify")
               (:file "wo-graph-functions")
	       (:file "graphviz")))


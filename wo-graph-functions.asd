;;;; wo-graph-functions.asd

(asdf:defsystem #:wo-graph-functions
  :serial t
  :depends-on (#:wo-graph #:wo-util)
  :components ((:file "package")
               (:file "wo-graph-functions")))


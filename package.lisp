;;;; package.lisp

(defpackage #:wo-graph-functions
  (:use #:cl #:wo-graph #:wo-util)
  (:export
   #:neighborhood
   #:simplify
   #:topological-sort
   #:make-single-sided-reducer
   #:make-subgraph-reducer
   #:boundary-from
   #:minimal-boundary-from
   #:reachable-from-not-reachable-from
   #:write-to-dot
   #:shortest-path
   #:classify-by-reacheability
   #:shortest-edge-path))


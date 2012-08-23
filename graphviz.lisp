(in-package #:wo-graph-functions)



(defun write-to-dot (stream graph 
		     &optional &key 
				 (graph-attributes nil)
				 (node-attributes (constantly nil))
				 (edge-attributes (constantly nil))
				 (node-to-id #'identity))
  "Writes the `graph' as a dot digraph graph to `stream'.
The formatting of the nodes and edges is optionally modified
by the functions `node-attributes' and `edge-attributes'.

The optional argument `graph-attributes' is a property list and
is written at the beginning of the graph.

If function `node-attributes' is a function taking two arguments,
a vertex and the graph.   It should return a property list with
such as (:shape :box :label \"text\") and these will be incorperated
into the dot file as ... [shape=box,label=\"text\"].

The same holds for `edge-attributes', however this is a function of 2 arguments
the edge and graph.  But the result should again be a property
list which will be formatted the same as for the `node-attributes'."
  (with-standard-io-syntax
    (let ((*print-right-margin* 10000)
	  (*print-case* :downcase))
      (format stream "digraph {~%~@[~{~A=~A;~^~%~}~]~%" graph-attributes)
      (loop :for node :in (wo-graph:all-vertices graph)
	 :for attributes = (funcall node-attributes node graph)
	 :do
	 (format stream "\"~A\"~@[ [~{~A=~A~^,~}]~];~%" (funcall node-to-id node) attributes))
      (loop :for node :in (wo-graph:all-vertices graph)
	 :do
	 (loop :for edge :in (wo-graph:outgoing-edges node graph)
	    :for target = (car (wo-graph:target-vertex edge graph))
	    :do
	    (when target
	      (format stream "\"~A\" -> \"~A\"~@[ [~{~A=~A~^,~}]~];~%"
		      (funcall node-to-id node) (funcall node-to-id target)
		      (funcall edge-attributes edge graph)))))
      (format stream "}"))))

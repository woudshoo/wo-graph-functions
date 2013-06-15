(in-package #:wo-graph-functions)


(defun simplify (graph &optional &key
		 (selector (constantly nil))
		 (reducers))
  "Simplifies a graph by repeatedly calling the reducers in REDUCERS on a copy of GRAPH.

The REDUCERS argument should be a list of functions each taking two arguments,
the SELECTOR and the GRAPH.  Such a function should return true if
the graph is modified, and nil if the reducer did not modify the graph.

The SELECTOR is also a function taking two arguments, a vertex and the graph.

The idea of the algorithm is that the reducers are called until none of the reducers
modifies the graph.   The selector argument is passed to the reducers to indicate
which vertices are not to be removed by the reducer.

The return value is the reduced graph."
  (loop :with result = (copy graph)
     :finally (return result)
     :while (some (lambda (r) (funcall r result selector)) reducers)))




;;; Common reducers

(defun make-single-sided-reducer (next previous next-count connect-edge)
  "Removes all vertices for which NEXT returns a list of length
NEXT-COUNT elements.  It will add edges with CONNECT-EDGE
between each source and target of the vertex to be removed.

Could be used as

 (make-singe-sided-reducer sources-of-vertex targets-of-vertex 1
           (lambda (s t g) (add-edge s t nil g)))

Or for the other way around

 (make-singe-sided-reducer targets-of-vertex sources-of-vertex 1
           (lambda (t s g) (add-edge s t nil g)))
 "
  (lambda (graph interesting-p)
    (loop
       :with changed = nil
       :for vertex :in (topological-sort graph previous next)
       :for targets = (funcall next vertex graph)
       :finally (return changed)
       :do
       (when (and (length-is next-count targets)
		      (not (funcall interesting-p vertex graph)))
	 (when connect-edge
	   (loop
	      :for source :in (funcall previous vertex graph)
	      :do
	      (loop :for target :in targets :do
		   (funcall connect-edge source target graph))))
	 (remove-vertex vertex graph)
	 (setf changed t)))))

(defun make-double-sided-reducer (next previous next-count previous-count connect-edge)
  (lambda (graph interesting-p)
    (loop 
       :with changed = nil
       :for vertex :in (all-vertices graph)
       :for targets = (funcall next vertex graph)
       :for sources = (funcall previous vertex graph)
       :finally (return changed)
       :do
       (when (and (not (funcall interesting-p vertex graph))
		  (length-is next-count targets)
		  (length-is previous-count sources))
	 (when connect-edge
	   (loop :for source :in sources :do
	      (loop :for target :in targets :do
		 (funcall connect-edge source target graph))))
	 (remove-vertex vertex graph)
	 (setf changed t)))))

(defun make-subgraph-reducer ()
  "Removes all non interesting vertices."
  (lambda (graph interesting-p)
    (loop
       :with changed = nil
       :for vertex :in (all-vertices graph)
       :finally (return changed)
       :do
       (unless (funcall interesting-p vertex graph)
	 (remove-vertex vertex graph)
	 (setf changed t)))))



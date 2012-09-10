(in-package #:wo-graph-functions)


(defun shortest-path (vertex-a vertex-b next graph)
  "Returns a path from `vertex-a' to `vertex-b'.
A path in this context is a list '(v1 v2 ... vn)
with v1 = vertex-a and vn = vertex-b.
Also vi is an element of (funcall 'next v{i-1} graph)."
  (let ((marker (get-vertex-marker graph))
	(todo (make-queue)))

    (flet ((add-to-todo-and-result (v-set sub-path)
	     (loop :for v :in v-set :do
		(unless (get-mark v marker)
		  (setf (get-mark v marker) (cons v sub-path))
		  (queue-push v todo)))))

      (add-to-todo-and-result (list vertex-a) nil)

      (loop :until (or (queue-empty-p todo) (get-mark vertex-b marker))
	 :for v = (queue-pop todo)
	 :for p = (get-mark v marker)
	 :do
	 (add-to-todo-and-result (funcall next v graph) p))

      (nreverse (get-mark vertex-b marker)))))

(defun shortest-edge-path (vertex-a vertex-b outgoing-edges outgoing-vertices graph 
			   &key include-vertices)
  "Returns a path from VERTEX-A to VERTEX-B.

OUTGOING-EDGES is a function of 2 arguments, a vertex and a graph and
should return a list of edges.  These edges are considered outgoing of
the vertex.

OUTGOING-EDGES is a function of 2 arguments, an edge and a graph, and
the result is the target vertices of that edge.  Note that this in
gerenal is a list of 1 element, but for hypergraphs it could contain
more elements.

INCLUDE-VERTICES defaults to nil, if t, the resulting path will include the vertices.

The path returned is a list of edges, '(e-1 e-2 ... e-n) with the
following constraints:

- e-1 is an element of (outgoing-edges vertex-a graph)
- vertex-b is an element of (outgoing-vertices e-n)
- for all 1 < i <= n:  e-i is an element of (outgoing-edges (outgoing-vertices e-{i-1}))

[With some liberal reading of the pseudo lisp above]

With INCLUDE-VERTICES t the path will look like 
   '(vertex-a e-1 v-1 e-2 ... v-{n-1} e-n vertex-b)"

  (let ((marker (get-vertex-marker graph))
	(todo (make-queue)))

    (labels ((extend-path (e v path)
	       "Adds edge (and vertex if requested) to path."
	       (let ((new-path (cons e path)))
		 (if include-vertices
		     (cons v new-path)
		     new-path)))
	     
	     (add-to-todo-and-result (edge-set sub-path)
	       (loop :for e :in edge-set :do
		  (loop :for v :in (funcall outgoing-vertices e graph) :do
		     (unless (get-mark v marker)
		       (setf (get-mark v marker) (extend-path e v sub-path))
		       (queue-push v todo))))))
      
      ;; Note that the setting the initial mark makes only a differences
      ;; if we have:
      ;;   a - vertex-a = vertex-b
      ;;   b - vertex-a has a loop edge.
      ;;
      ;;             Initial mark    |   not initial mark
      ;;   case a - empty-path       |  try to find path looping back
      ;;   case b - loop edge ignored|  (Harmless I think. find loop)
      (setf (get-mark vertex-a marker) (list vertex-a))
      (queue-push vertex-a todo)

      (loop :until (or (queue-empty-p todo) (get-mark vertex-b marker))
	 :for v = (queue-pop todo)
	 :for p = (get-mark v marker)
	 :do
	 (add-to-todo-and-result (funcall outgoing-edges v graph) p))

      (let ((result-path (nreverse (get-mark vertex-b marker))))
	(if include-vertices
	    result-path
	    (rest result-path))))))

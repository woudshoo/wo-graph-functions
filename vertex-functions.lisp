(in-package #:wo-graph-functions)


(defun neighborhood (vertex graph &optional &key
		     (max-distance nil)
		     (selector #'neighbors-of-vertex))
  "Returns a list of vertices which can be considered the neighborhood
of the `vertex' argument.

The `selector' argument is a function of two arguments,
a vertex and a graph and provides the direct neighbors of the vertex.

The list of vertices which is returned is all vertices which
can be reached from `vertex' in less than `max-distances' steps using
the `selector' function.

If `max-distance' is nil or 0 there is no limit on the number of steps."
  (let ((marker (get-vertex-marker graph))
	(todo (queue-push vertex (make-queue)))
	(result (list)))
    (setf (get-mark vertex marker) (or max-distance -1))
    (loop
       :until (queue-empty-p todo)
       :for current = (queue-pop todo)
       :for distance = (- (get-mark current marker) 1)
       :do
       (push current result)
       (unless (eql distance 0)
	 (loop :for source :in (funcall selector current graph)
	    :for source-mark = (get-mark source marker)
	    :do
	    (when (or (not source-mark) (< source-mark distance))
	      (setf (get-mark source marker) distance)
	      (queue-push source todo)))))
    result))

(defun boundary-from (vertices next selector-p graph)
  "Returns all vertices from the graph such that

1. There is a path from `vertices' by calling successively `next'.
2. The vertex satisfies `selector-p' (calling is (selector-p vertex `graph'))
3. The path does not contain any other vertex which satisfies `selector-p'.

Note: The argument `vertices' is either a single vertex or a
      collection of vertices.

Note: The argument `next' is a function of two arguments a vertex and
      `graph' and returns the reacheable set of other vertices given
      the vertex argument.

Some explanation: It basically keeps expanding the set of vertices
given as first argument until it finds a vertex which satisfies the
selector.  So the selector is indicate the boundary."
  (let ((marker (get-vertex-marker graph))
	(todo (make-queue))
	(result (list)))

    (flet ((add-to-todo-and-result (v-set)
	     (loop :for v :in v-set :do
		(unless (get-mark v marker)
		  (setf (get-mark v marker) t)
		  (if (funcall selector-p v graph)
		      (push v result)
		      (queue-push v todo))))))

      (add-to-todo-and-result (listify vertices))
      (loop :until (queue-empty-p todo)
	 :for v = (queue-pop todo)
	 :do
	 (add-to-todo-and-result (funcall next v graph))))

    result))

(defun minimal-boundary-from (vertices next selector-p graph)
  "Returns all vertices from the `graph' such that

1. There is a path from `vertices' by calling successively `next'.

2. The vertex satisfies `selector-p'
   (called as (selector-p vertex graph))

3. There is no path to the vertex which will have 2
   vertices (including the end points) that satisfy `selector-p'.

See for comparision the function boundary-from, the results of this
function will be a subset of boundary-from.

Note: The argument `vertices' is either a single vertex or a
      collection of vertices

Note: The argument `next' is a function of two arguments, a vertex and
     `graph' and returns the reacheable set of other vertices given the
      vertex argument.
"
  (let ((marker (get-vertex-marker graph))
	(todo (make-queue))
	(result (list)))

   (flet ((add-to-todo-and-result (v-set from-dist)
	     (loop :for v :in v-set
		:for new-dist = (if (= 0 from-dist)
				    (if  (funcall selector-p v graph) 1 0)
				    2)
		:for old-dist = (get-mark v marker -1)
		:do
		(when (> new-dist old-dist)
		  (setf (get-mark v marker) new-dist)
		  (queue-push v todo)
		  (when (= 1 new-dist) (push v result))))))

      (add-to-todo-and-result (listify vertices) 0)

      (do-queue (v todo)
	(add-to-todo-and-result (funcall next v graph)
				(get-mark v marker)))

      (remove-if-not (lambda (v) (= 1 (get-mark v marker))) result))))

(defun reachable-from-not-reachable-from (vertex-a next-a vertex-b next-b graph)
  "Returns a list of vertices in `graph' which are reacheable from
`vertex-a' by subsequently calling `next-a', but which are not
reachable from `vertex-b' by calling `next-b'."
  (let ((marker (get-vertex-marker graph))
	(todo (queue-push vertex-b (make-queue)))
	(result (list)))
    ;; first mark all reacheable from vertex-b as exclude.
    (setf (get-mark vertex-b marker) 'exclude)
    (loop
       :until (queue-empty-p todo)
       :for current = (queue-pop todo)
       :do
       (loop :for next :in (funcall next-b current graph)
	  :do
	  (unless (get-mark next marker)
	    (setf (get-mark next marker) 'exclude)
	    (queue-push next todo))))
    ;; now
    (flet ((mark-push-and-add-to-result (v)
	     (unless (eq 'exclude (get-mark v marker)) (push v result))
	     (setf (get-mark v marker) 'done)
	     (queue-push v todo)))
      (mark-push-and-add-to-result vertex-a)
      (loop
	 :until (queue-empty-p todo)
	 :for current = (queue-pop todo)
	 :do
	 (loop :for next :in (funcall next-a current graph)
	    :do
	    (unless (eq 'done (get-mark next marker))
	      (mark-push-and-add-to-result next)))))
    result))

(defun topological-sort (graph next previous)
  "Returns the nodes of `graph' vertices sorted in topological order,
The functions `next' and `previous' are functions of two arguments, a
vertex and a graph.

Given a vertex they should return successors respecitvely predecessors in the directed graph.

So typically you would call it as

  (topological-sort graph
                  #'targets-of-vertex
                  #'sources-of-vertex)

By swapping the two functions around, the topological sort is reversed."

  (let ((marker (get-vertex-marker graph))
	(todo (make-queue))
	(result (list)))

    (flet ((seen-all-previous (v)
	     (every (lambda (v) (get-mark v marker)) (funcall previous v graph)))

	   (visit (v)
	     (setf (get-mark v marker) 'seen)
	     (push v result)
	     (queue-push v todo)))

      (mapc (lambda (v) (unless (funcall previous v graph) (visit v)))
	    (all-vertices graph))

      (loop :until (queue-empty-p todo)
	 :for v = (queue-pop todo)
	 :do
	 (loop :for w :in (funcall next v graph)
	    :do (when (and (not (get-mark w marker))
			   (seen-all-previous w))
		  (visit w))))
      (nreverse result))))


;;; NOTE
;;;
;;; THIS FUNCTION MIGHT NEED TO GO.  IT IS TOO SIMPLE.
;;; PROBABLY WE NEED TO EXTEND neighborhood To optionally
;;; take a funcion argument. ALSO NOTE THAT neighborhood
;;; is probably buggy, I think it can return the same vertex
;;; multiple times. HOWEVER, neighborhood needs to use a priority-queue.
(defun visit-vertices (fn vertex next graph)
  "Visit the vertices (including vertex) which are reacheable from
`vertex' by calling `next' repeatedly.  For each vertex found it will
call the function `fn' with two arguments, the vertex and the `graph'.
Care is taken that the function `fn' is called exactly once for a
reacheable vertex."
  (mapcar (lambda (v) (funcall fn v graph))
	  (neighborhood vertex graph :selector next)))
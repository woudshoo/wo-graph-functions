(in-package #:wo-graph-functions)

;;; Testing out new grounds.
;;;
;;;
;;; Classify by reacheability
;;;
;;; the idea is that given the graph G=(V,E) and
;;; a subset S \subset V of selected vertices we can define
;;; an equivelance relation R_S on V by looking at which selected
;;; vertices are reacheable.
;;;
;;; First let p <= q means that there is a path from p to q.
;;;   (if p == q we assume p <= q).
;;; (note that for a DAG this is a partial order
;;;
;;; Now we say that:
;;;
;;;    p R_S q  iff for all s in S:
;;;                s <= p  <==> s <= q  and
;;;                p <= s  <==> q <= s
;;;
;;;
;;;   e.g:
;;;             /--> c --> d
;;;    a* --> b
;;;             \---> e* --> f
;;;
;;;
;;;   We have the following classes:
;;;                                          /-- {c,d}
;;;     {a},  {b}, {c,d}, {e}, {f}    a --> b
;;;                                          \-- {e} --> {f}
;;;
;;;   If a was not marked the classes are:
;;;
;;;    {a, b}, {e}, {f}, {c,d}
;;;


(defun classify-by-reacheability (selected graph)
  "This function needs lots of work.
1 - it is not very effient,
2 - it should have next previous arguments,
3 - it should handle the 'test-fn' specified in the graph correctly.
4 - the return type should probably not be a hashtable."
  (let ((from-marker (get-vertex-marker graph))
	(to-marker (get-vertex-marker graph))
	(result (make-hash-table :test #'equalp)))

    (loop :for special-vertex :in selected :do
       (visit-vertices (lambda (v g)
			 (declare (ignore g))
			 (push special-vertex (get-mark v from-marker (list))))
		       special-vertex #'sources-of-vertex graph)
       (visit-vertices (lambda (v g)
			 (declare (ignore g))
			 (push special-vertex (get-mark v to-marker (list))))
		       special-vertex #'targets-of-vertex graph))


    (loop :for v :in (all-vertices graph) :do
       (push v (gethash (cons (get-mark v from-marker) (get-mark v to-marker)) result (list))))

    result))

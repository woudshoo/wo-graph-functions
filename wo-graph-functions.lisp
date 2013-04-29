(in-package #:wo-graph-functions)

;;; Testing out new grounds.
;;;
;;;
;;; Classify by reacheability


(defun classify-by-reacheability (graph next previous &key selector-p)
  "
The idea is that given the graph G=(V,E) and
a subset S \subset V of selected vertices we can define
an equivelance relation R_S on V by looking at which selected
vertices are reacheable.

First let p <= q means that there is a path from p to q. (if p == q we assume p <= q).

Note that for a DAG this is a partial order

Now we say that:

  ;
    p R_S q  iff for all s in S:
                s <= p  <==> s <= q  and
                p <= s  <==> q <= s


e.g:


  ;
           /--> c --> d
   a* --> b
           \\---> e* --> f

We have the following classes:

  ;
                                        /-- {c,d}
   {a},  {b}, {c,d}, {e}, {f}    a --> b
                                        \\-- {e} --> {f}

If a was not marked the classes are:

  ;
   {a, b}, {e}, {f}, {c,d}


This function needs lots of work.
- it should have next previous arguments,
- it should handle the 'test-fn' specified in the graph correctly.
- the return type should probably not be a hashtable.
"

  (let ((sorted-vertices (topological-sort graph next previous))
	(to-marker (get-vertex-marker graph))
	(from-marker (get-vertex-marker graph))
	(result (make-hash-table :test #'equalp)))

    (labels ((update-mark (marker vertex set-of-values)
	       (when set-of-values
		 (setf (get-mark vertex marker) 
		       (fset:union (get-mark vertex marker (fset:set))
				   set-of-values))))
	     
	     (mark-one-way (marker ordered-vertices next-vertices-fn)
	       (loop :for vertex :in ordered-vertices :do
		  (when (funcall selector-p vertex graph)
		    (update-mark marker vertex (fset:set vertex)))
		  (loop :for next-vertex :in (funcall next-vertices-fn vertex graph) :do
		     (update-mark marker next-vertex (get-mark vertex marker))))))
      
      (mark-one-way to-marker sorted-vertices next)
      (mark-one-way from-marker (reverse sorted-vertices) previous)
      ;; do the marking

      (loop :for v :in sorted-vertices :do
	 (push v (gethash 
		  (cons (get-mark v from-marker) (get-mark v to-marker))
		  result 
		  (list))))
      
      result)))

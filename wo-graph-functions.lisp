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

 
Now this is obvioulsy an equivalence relation.  However
this also induces a partial order on the equivalence classes:

[p]_S <= [q]_S if there exists a p' R_S p and q' R_S q such that p' <= q'.

Proof:
1 - it is obvious that [p]_S <= [p]_S
2 - 
    
    


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
- it should handle the 'test-fn' specified in the graph correctly.

The return type is a fset mapping:

  (from-names . to-names)  -> vertices

With from-names and to-names both an fset and vertices a list.
"

  (let ((sorted-vertices (topological-sort graph next previous))
	(to-marker (get-vertex-marker graph))
	(from-marker (get-vertex-marker graph))
	(result (fset:empty-map)))

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
	 (setf result
	       (wo-util:add-value-to-map result (cons (get-mark v from-marker)
						      (get-mark v to-marker)) v)))
      
      result)))

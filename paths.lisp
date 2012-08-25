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

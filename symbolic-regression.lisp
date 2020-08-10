(in-package :genetic-programming)

(defclass program ()
  ((params :initarg :params
	   :accessor params)
   (expression :initarg :expression
	       :accessor expression)
   (fitness :initform 0 :accessor fitness)))

(defun generate-random-program (function-set terminal-set max-depth method
				min max step objective
				&optional grow-rate)
  (let ((p (make-instance 'program
			  :params (filter-symbols terminal-set)
			  :expression (generate-random-expression function-set
								  terminal-set
								  max-depth
								  method
								  grow-rate))))
    (update-fitness p min max step objective)
    p))


(defun filter-symbols (list)
  (remove-if-not #'symbolp list))

(defun generate-initial-population (size function-set terminal-set
				    depth method 
				    min max step objective
				    &optional grow-rate)
  (do ((i size (1- i))
       result)
      ((zerop i) result)
    (push (generate-random-program function-set terminal-set
				   (random-bet depth)
				   (if (eq method :ramped)
				       (if (evenp i) :grow :full)
				       method)
				   min max step objective grow-rate) 
	  result)))

(defmethod print-object ((obj program) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~{~a~^ ~})~%~2t~a" (params obj) (expression obj))))

(defmethod runnable-program ((p program))
  (eval `(lambda ,(slot-value p 'params)
	   ,@(mapcar #'(lambda (var)
			`(declare (ignorable ,var)))
		    (slot-value p 'params))
	   ,(slot-value p 'expression))))

(defmethod update-fitness ((p program) min max step objective)
  (let ((fn (runnable-program p)))
    (with-slots (fitness) p
      (setf fitness
	    (loop for i from min to max by step
	       summing (abs (- (funcall fn i)
			       (funcall objective i)))
	       into deviation
	       finally (return deviation))))))

(defun new-population (prog-list k r-rate c-rate m-rate
		       function-set terminal-set depth method
		       min max step objective
		       &optional grow-rate)
  (labels ((fn () (tournament prog-list k #'fitness #'<)))
    (do* ((lt (length prog-list))
	  (i 0 (1+ i))
	  (rep (* lt r-rate) (1- rep))
	  (cro (* lt c-rate) (1- cro))
	  (mut (* lt m-rate) (1- mut))
	  result)
	 ((= i lt) result)
      (when (> rep 0) (push (fn) result))
      (when (> cro 0) (push (program-crossover (fn) (fn) min max step objective)
			    result))
      (when (> mut 0) (push
		       (program-mutation (fn)
					 function-set terminal-set
					 (random-bet depth)
					 method
					 min max step objective grow-rate)
		       result)))))

(defun program-crossover (program-1 program-2 min max step objective)
  (let ((p (make-instance 'program :params (remove-duplicates
					    (append (params program-2)
						    (params program-1)))
			  :expression
			  (flat-replacnth
			   (flat-nth (random (flat-length 
					      (expression program-2)))
				     (expression program-2))
			   (random (flat-length (expression program-1)))
			   (expression program-1)))))
    (update-fitness p min max step objective)
    p))

(defun program-mutation (parent function-set terminal-set
			 max-depth method
			 min max step objective &optional grow-rate)
  (program-crossover parent (generate-random-program function-set
						     terminal-set
						     max-depth method
						     min max step objective
						     grow-rate)
		     min max step objective))

(defun terminationp (prog-list target &optional (order #'<=))
  (find-if #'(lambda (item)
	       (funcall order item target))
	   prog-list :key #'fitness))

(defun symbolic-regression (size tournament-k r-rate c-rate m-rate
			    function-set terminal-set depth method
			    objective target-error min max step
			    &optional grow-rate)
  (do ((population (generate-initial-population size function-set
						terminal-set depth
						method min max step objective
						grow-rate)
		   (new-population population tournament-k
				   r-rate c-rate m-rate
				   function-set terminal-set
				   depth method 
				   min max step objective grow-rate)))
      ((terminationp population target-error #'<=)
       (terminationp population target-error #'<=))))

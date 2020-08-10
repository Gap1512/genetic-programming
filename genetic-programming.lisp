(in-package :genetic-programming)

(defun arity (fn)
  (let ((arglist (sb-introspect:function-lambda-list fn)))
    (if (intersection arglist lambda-list-keywords)
	(error "~S lambda list ~S contains keywords" fn arglist)
	(length arglist))))

(defun +/2 (a b) (+ a b))
(defun -/2 (a b) (- a b))
(defun */2 (a b) (* a b))
(defun % (number &rest more-numbers)
  (cond ((some #'zerop more-numbers) 1)
	(more-numbers (apply #'/ (cons number more-numbers)))
	(t (/ number))))
(defun %/2 (a b) (% a b))

(defun generate-random-expression (function-set terminal-set max-depth method 
				   &optional grow-rate)
  (let* ((fn-set-l (length function-set))
	 (t-set-l (length terminal-set))
	 (grow-p (or grow-rate (/ t-set-l (+ t-set-l fn-set-l)))))
    (labels ((rec (actual-depth result-expression)
	       (if (or (zerop actual-depth) (and (eql method :grow)
						 (< (/ (random 1000) 1000)
						    grow-p)))
		   (let ((sym (nth (random t-set-l) terminal-set)))
		     (if (listp sym)
			 (random-bet sym)
			 sym))
		   (let ((rnd-function (nth (random fn-set-l) function-set)))
		     (append (cons rnd-function
				   (do ((i 0 (1+ i))
					expr)
				       ((= i (arity rnd-function)) 
					(nreverse expr))
				     (push (rec (1- actual-depth) nil) expr)))
			     result-expression)))))
      (rec max-depth nil))))

(defun random-bet (range)
  (destructuring-bind (min max precision) range
    (when (> max min)
      (let ((scale (round (% 1 precision))))
	(+ (/ (random (* scale (- max min))) scale) min)))))

(defun tournament (list k &optional (fitness #'eval) (order #'>))
  (car (select-best 1 (select-random k list) order fitness)))

(defun select-random (n list)
  (let ((l (length list)))
    (loop for i from 1 to n collecting (nth (random l) list))))

(defun select-best (n list predicate key)
  (when (not (or (zerop n) (null list)))
    (subseq (sort (copy-seq list) predicate :key key) 0 n)))

(defun flat-length (tree)
  (labels ((rec (tree acc)
	     (if (atom tree)
		 (1+ acc)
		 (+ acc (rec (car tree) 0)
		    (if (cdr tree) (rec (cdr tree) 0) 0)))))
    (rec tree 0)))

(defun flat-nth (n tree)
  (labels ((rec (n tree)
	     (cond
	       ((null tree) (values n nil))
	       ((<= n 0) (values n (car tree)))
	       ((atom (car tree)) (rec (1- n) (cdr tree)))
	       (t (multiple-value-bind (a l)
		      (rec n (car tree))
		    (if l (values a l)
			(rec a (cdr tree))))))))
    (nth-value 1 (rec n (list tree)))))

(defsetf flat-nth (n tree) (new-tree)
  `(flat-replacnth ,new-tree ,n ,tree))

(defun flat-replacnth (new-tree n tree)
  (labels ((rec (tree acc)
	     (cond
	       ((null tree) (values nil acc))
	       ((= acc n) (values (cons new-tree (cdr tree)) (1+ acc)))
	       ((atom tree) (values tree (1+ acc)))
	       (t (multiple-value-bind (sub x)
		      (rec (car tree) acc)
		    (multiple-value-bind (sub-cdr x-cdr)
			(rec (cdr tree) x)
		      (values (cons sub sub-cdr) x-cdr)))))))
    (car (rec (list tree) 0))))

(defun subtree-crossover (parent-1 parent-2)
  (setf (flat-nth (random (flat-length parent-1)) parent-1)
	(flat-nth (random (flat-length parent-2)) parent-2))
  parent-1)

(defun subtree-mutation (parent function-set terminal-set
			 max-depth method &optional grow-rate)
  (subtree-crossover parent (generate-random-expression function-set
							terminal-set
							max-depth method
							grow-rate)))

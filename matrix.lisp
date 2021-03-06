;;; ================================================================================================
;;; 1. Checking a given matrix is well-formed
;;; ================================================================================================

;;; Driver function to ensure a matrix is well-formed.
;;; All 3 conditions must equate to true for a matrix to be considered well-formed:
;;; 	1. Matrix is not the empty list
;;; 	2. All rows of matrix have same length
;;; 	3. All matrix elements are valid expressions in E
(defun b-check (matrix)
	(if (and (not (null matrix)) (b-check-expressions matrix) (b-check-shape matrix))
		T
		nil))

;;; Ensures the shape of an input matrix is correct (all rows have the same length)
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-check-shape (matrix) 
;;	3 cases to check shape of matrix is correct:
	(cond
;;		1. Base: 
;;		- if matrix contains 1 list (matrix length = 1), length is inherently equal for all rows
		((= (length matrix) 1)
			T)
;;		2. Recursive: 
;;		- if both the first row and second row in the matrix have equal length, the matrix is thus
;;		far well-formed. So recurse down the remaining rows of the matrix
;;		- because equality is transitive, it's sufficient to check if each consecutive pair of rows 
;;		has equal length
;;			e.g. if (length row1) = (length row2), and (length row2) = (length row3)
;;				 then (length row1) = (length row3)
;;		- once we reach the base case of checking the final row (matrix length = 1), all rows
;;		have been found to have equal length (including the final row, as its equality was checked
;;		with its preceding row)
		((= (length (nth 0 matrix)) (length (nth 1 matrix)))
			(b-check-shape (cdr matrix)))
;;		3. Default: 
;;		- if the matrix contains more than 1 list, and a consecutive pair of rows are found to be 
;;		of different length, then the matrix is ill-formed
		(T
			nil)))

;;; Ensures the elements of an input matrix are all valid expressions in E.
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;;; As such, the process is broken up into 2 recursive stages:
;;; 	1. Recurse over each individual row of the matrix (b-check-expressions)
;;;		2. Recurse over each element in the row (b-check-expressions-row and row-function)
(defun b-check-expressions (matrix)
;;	Stage 1:
;;	Receive entire matrix as argument, so 3 cases to consider:
	(cond
;;	 	1. Base:
;;		- if we reach the null list (end of the matrix), then every row (and thus, every element) in 
;;		the matrix has been traversed without finding an invalid expression. As such, we return true.
		((null matrix)
			T)
;;	 	2. Recursive:
;;		- first, ensure the first row in the matrix is not the empty list (ensures that the base
;;		case in b-check-expressions-row doesn't inadvertently return true for an invalid row)
;;		- also ensure that the head of the matrix is actually a list (i.e. a row)
;;		- then, check the elements in the first row of the matrix by calling the helper function. 
;; 		If this call returns true (no invalid expressions are found in the rule), then we recurse 
;;		down with the remaining rows
		((and (not (null (car matrix))) (listp (car matrix)) (b-check-expressions-row (car matrix)))
			(b-check-expressions (cdr matrix)))
;;		3. Default:
;;		- if any row of the matrix is null, or if any of the recursive calls to each row of the 
;;		matrix discover an invalid expression, the default case is realised and the matrix is 
;;		classified as ill-formed
		(T 
			nil)))

;;; Helper function called from b-check-expressions.
;;;	Ensures individual matrix elements in a parsed row are all expressions in E.
;;; @row: matrix row of length m with form (a11 ... a1m) (i.e. a list)
(defun b-check-expressions-row (row)
;;	Stage 2: 
;;	Receive a row of a matrix as argument, so 4 cases to consider:
	(cond
;;	 	1. Base:
;;		- if we reach the null list (end of the row), then every element in the input row has been 
;;		traversed without finding an invalid expression. As such, we return true.
		((null row)
			T)
;;	 	2. Recursive (function call):
;;		- if the first element of the row is a list itself, the only valid expression contained in 
;;		this list would be a function call to b-and/b-or/b-not (so call the helper function)
;;		- if we return from this helper function with true (i.e. finding no invalid expressions
;;		in the function), we recurse down the remainder of the row elements
		((listp (car row))
			(if (b-check-expressions-row-function (car row))
				(b-check-expressions-row (cdr row))
				nil))
;;		3. Recursive (nullary symbol):
;;		- if the first element of the row is the number 0, 1, or a symbol, we recurse down the 
;;		remainder of the row elements (as theses are valid expressions in E)
		((or (symbolp (car row)) 
			 (or (= 0 (car row)) (= 1 (car row))))
			(b-check-expressions-row (cdr row)))
;;		4. Default:
;;		- if the first element of the row is none of: a list, 0, 1, a symbol, then the expression
;;		for that matrix element is not in E, and we return nil up the call stack
		(T
			nil)))

;;; Helper function called from b-check-expressions-row.
;;; Ensures a matrix element containing an expected function call is a valid expression in E.
;;; Performs work of checking whether a function element in a matrix is a valid symbolic expression.
;;; @fn-call: a list from within one of the rows in the matrix (i.e. an expected function)
(defun b-check-expressions-row-function (fn-call)
;;	Stage 2 cont.:
;;		2. Recursive (functional call):
;;		- if a list is found as a matrix element in a row, it gets parsed to this helper function
;;		- check that the fn-call is a valid function, matching any of the following forms:
;;			- (b-not _)   (length of 2)
;;			- (b-and _ _) (length of 3)
;;			- (b-or _ _)  (length of 3)
	(if (or 
			(and (= (length fn-call) 2)
				 (eql 'b-not (car fn-call)))
			(and (= (length fn-call) 3)
				 (or (eql 'b-and (car fn-call))
				     (eql 'b-or (car fn-call)))))
;;		- if we find a list with a matching form, we recurse on the remainder of the element with
;;		the original b-check-expressions-row function (allowing nested function calls, and any 
;;		invalid function arguments to be discovered)
		(b-check-expressions-row (cdr fn-call))
;;		- otherwise, the element is not a valid function call, and we return nil up the call stack
		nil))


;;; ================================================================================================
;;; 2. Retrieving the shape of a matrix
;;; ================================================================================================

;;; Finds and returns the shape of the matrix as a list.
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-shape (matrix)
;	Ensure the matrix is well-formed
	(if (b-check matrix)
;;		If the matrix is well-formed, the shape of the list can simply be derived from the length 
;;		of the outer list (the number of inner lists/rows), and the length of the first row in the 
;;		matrix (i.e. the number of 'columns': a well-formed matrix will have rows of equal length)
		(list (length matrix) (length (car matrix)))
;		Otherwise, if the matrix is found to be ill-formed, return nil
		nil))

;;; ================================================================================================
;;; 3. Accessing an element of a matrix
;;; ================================================================================================

;;; Finds and returns the element of the n-th row and m-th column in the matrix.
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;;; @n: n-th row (1-indexed)
;;;	@m: m-th column (1-indexed)
;;;	eg. (b-elem ((a b) (c d)) 1 2) = b
(defun b-elem (matrix n m)
;	Ensure the matrix is well-formed, and the indexes are positive (to use nth)
	(if (and (b-check matrix) (> n 0) (> m 0))
;;		If the matrix is well-formed, the element can be found by using nth. Because nth is 
;;		0-indexed, we initially subtract 1 from the given n and m values (as they are 1-indexed).
;;		Example:
;;		(b-elem ((a b) (c d)) 1 2)
;;		- (nth (- n 1) matrix): returns nth row of the matrix, e.g. (a b)
;;		- (nth (- m 1) <row>):  returns mth element in that row of the matrix (the 'column') e.g. b
;;		Will also return nil if either n or m are invalid row/column numbers (i.e. out of bounds)
		(nth (- m 1) (nth (- n 1) matrix))
;		Otherwise, if matrix is ill-formed, return nil
		nil))

;;; ================================================================================================
;;; 4. Applying boolean operations to matrices
;;; ================================================================================================

;;; Driver function to construct a matrix performing b-and element-wise on a pair of matrices.
;;; @matrix-1&2: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-and (matrix-1 matrix-2)
;;  Ensure that both matrices are well-formed and have equal shapes (same number of rows and columns)
;;  before calling the worker function. Otherwise, return nil.
	(if (and (b-check matrix-1) (b-check matrix-2) (equal (b-shape matrix-1) (b-shape matrix-2)))
		(b-build-fn-matrix 'b-and matrix-1 matrix-2)
		nil))

;;; Driver function to construct a matrix performing b-or element-wise on a pair of matrices.
;;; @matrix-1&2: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-or (matrix-1 matrix-2)
;;  Ensure that both matrices are well-formed and have equal shapes (same number of rows and columns)
;;  before calling the worker function. Otherwise, return nil.
	(if (and (b-check matrix-1) (b-check matrix-2) (equal (b-shape matrix-1) (b-shape matrix-2)))
		(b-build-fn-matrix 'b-or matrix-1 matrix-2)
		nil))

;;; Driver function to construct a matrix performing b-not element-wise on an input matrix.
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-not (matrix)
;	Ensure the matrix is well-formed before calling the worker function. Otherwise, return nil.
	(if (b-check matrix)
		(b-build-fn-matrix 'b-not matrix)
		nil))

;;; Helper function called from b-and/b-not/b-or.
;;; Recursively 'performs' the provided function element-wise on the input matrices
;;; @fn-name: the function to be applied to each element of the matrices
;;; @matrix-1: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;;; @matrix-2: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;;;			   Optional, to allow unary functions to be performed on only matrix-1.
(defun b-build-fn-matrix (fn-name matrix-1 &optional (matrix-2 0))
;;	2 cases to recursively construct the matrix:
;;	1, Base:
;;	If we reach the end of the input matrix (i.e. matrix-1 is null), then we attach nil to the 
;;	constructed matrix to signify its end.
	(if (null matrix-1)
		nil
;;		2. Recursive:
;;		Otherwise, we have 2 recursive cases:
		(if (not (listp matrix-2))
;;			i. If a second matrix has not been provided (i.e. matrix-2 not a list, and set to be
;;			the default value of 0), then we only recurse down with the first matrix. We construct a
;;			matrix (list of lists) from the recursive calls: call the helper function to build the 
;;			first row of the matrix, and then recurse down on the remaining rows (cdr matrix-1).
			(cons
				(b-build-fn-matrix-row fn-name (car matrix-1))
				(b-build-fn-matrix fn-name (cdr matrix-1)))
;;			ii. If a second matrix is provided, then we recurse down with both. The same construction
;;			process is used as the first case, just using both matrices instead.
			(cons
				(b-build-fn-matrix-row fn-name (car matrix-1) (car matrix-2))
				(b-build-fn-matrix fn-name (cdr matrix-1) (cdr matrix-2))))))

;;; Helper function called from b-build-fn-matrix
;;; Recursively 'performs' the provided function element-wise on individual rows of the input 
;;; matrices.
;;; @fn-name: the function to be applied to each element in the rows
;;; @row-1: matrix row of length m with form (a11 ... a1m) (i.e. a list)
;;; @row-2: matrix row of length m with form (a11 ... a1m) (i.e. a list)
;;;			Optional, to allow unary functions to be performed on only row-1
(defun b-build-fn-matrix-row (fn-name row-1 &optional (row-2 0))
;;	2 cases to recursively construct the row of a matrix:
;;	1. Base:
;;	If we reach the end of the input row (i.e. row-1 is null), then we attach nil to the constructed
;;	row to signify its end.
	(if (null row-1)
		nil
;;		2. Recursive
;;		Otherwise, we have 2 recursive cases:
		(if (not (listp row-2))
;;			i. If a second row has not been provided (i.e. row-2 not a list, and set to the default
;;			value of 0), then only recurse down the first row. We construct a row (list) as follows:
;;				((<fn-name> <car row-1>) [repeat for each element of row-1])
			(cons 
				(list fn-name (car row-1))
				(b-build-fn-matrix-row fn-name (cdr row-1)))
;;			ii. If a second row is provided, then recurse down with both to construct a new row. The
;;			same construction process is used as above, this time constructing a list as follows:
;;				((<fn-name> <car row-1> <car row-2>) [repeat for each element of row-1 and row-2])
			(cons 
				(list fn-name (car row-1) (car row-2))
				(b-build-fn-matrix-row fn-name (cdr row-1) (cdr row-2))))))

;;; ================================================================================================
;;; 5. Simplifying (rewriting) a matrix
;;; ================================================================================================

;;; Driver function to simplify the elements of a matrix
;;; @matrix: input n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
(defun b-simplify (matrix)
;	Ensure matrix is well-formed before doing anything, otherwise return nil
	(if (not (b-check matrix))
		nil
;;		We recursively move through each element of the matrix, simplifying each one individually,
;;		and create a new matrix with these simplified elements. To recursively construct this matrix
;;		we have 2 cases:
;;		1. Base
;;		If we reach the end of the input matrix (i.e. matrix is null), then we attach nil to the 
;;		constructed matrix to signify its end.
		(if (null matrix)
			nil
;;			2. Recursive
;;			Otherwise, we construct a list (of lists) by recursing down the first row of the input
;;			matrix and attaching this new list to the remainder of the lists (by recursing down the
;;			remainder of the matrix).
			(cons
				(b-simplify-row (car matrix))
				(b-simplify (cdr matrix))))))

;;; Driver function to simplify the elements of a matrix row
;;; @row: matrix row of length m with form (a11 ... a1m) (i.e. a list)
(defun b-simplify-row (row)
;;	Recursively move through each element of the row, to simplify each individually, and create a
;;	new row with these simplified elements. 2 cases to construct this row:
;;	1. Base
;;	If we reach the end of the input row (i.e. row-1 is null), then we attach nil to the constructed
;;	row to signify its end.
	(if (null row)
		nil
;;		2. Recursive
;;		Otherwise, we attempt to simplify the current element of the row (the first element, car row) 
;;		and recurse down the remainder of the row
		(cons
			(b-simplify-element (car row))
			(b-simplify-row (cdr row)))))

;;; Driver function to simplify a matrix element
;;; @elem: element of a matrix (already known to be a valid expression in E)
;;; @prev-elem: previously simplified version of elem (to provide an exit condition: if elem = prev-elem
(defun b-simplify-element (elem &optional (recurse-count 0) (prev-elem nil))
;;	Recursively move through the element's expressions and sub-expressions.
	;(print recurse-count)
	(cond
;;		1. Base a:
;;		There's an upper limit of how many times we can recurse (so we don't end up with infinite
;;		recursion swapping back and forth between associative rules). 
;;		NOTE FOR MARKER: May need to change this, if performance is becoming an issue.
		((> recurse-count 100)
			elem)
;;		2. Base b:
;;		If we move through the simplification process without being able to make a change, simply
;;		return the original element (no stack overflows/infinite recursion).
		((equal prev-elem elem)
			elem)
;;		3. Recursive: If the element is a list, then we know it must be a function (b-and, b-or, b-not)
;;		So call the solver, containing the rewrite rules, for the function of the expression, as well
;;		as its sub-expressions.
		((listp elem)
			(if (eql 'b-not (car elem))
;				If b-not, only parse its applied expression
				(b-simplify-element-solve 
			 		(list (car elem) 
			 			(b-simplify-element (nth 1 elem)))
			 		recurse-count)
;				Otherwise, if b-and/b-or, parse both sub-expressions
				(b-simplify-element-solve 
			 		(list (car elem) 
			 			(b-simplify-element (nth 1 elem))
			 			(b-simplify-element (nth 2 elem)))
			 		recurse-count)))
;;		4. Base c:
;;		Otherwise, if the matrix element is a symbol, or 0 or 1, we cannot simplify this any further. 
;;		So return the element as is.
		((or (symbolp elem)
			 (or (= 0 elem) (= 1 elem)))
			elem)

		(T
			elem)))


;;; Driver function to call the appropriate rewrite helper function, depending on the function
;;; applied to the matrix element (i.e. 'directs' the simplification)
;;; @elem: element of a matrix (already known to be a valid expression in E)
;;; @prev-elem: previously simplified version of elem (to provide an exit condition: if elem = prev-elem
(defun b-simplify-element-solve (elem &optional (recurse-count 0) (prev-elem nil))
	(cond
;		Annulment, identity, and negation laws (i.e. function with constant as an expression)
		((numberp (nth 1 elem))
			(b-simplify-element (b-simplify-element-solve-constant (nth 0 elem) (nth 1 elem) (nth 2 elem)) 
				(+ recurse-count 1) elem))
		((numberp (nth 2 elem))
			(b-simplify-element (b-simplify-element-solve-constant (nth 0 elem) (nth 2 elem) (nth 1 elem)) 
				(+ recurse-count 1) elem))
;		'and' rewrite rules: idempotent, complement
		((eql 'b-and (car elem))
			(b-simplify-element (b-simplify-element-solve-and (nth 1 elem) (nth 2 elem)) 
				(+ recurse-count 1) elem))
;		'or' rewrite rules: idempotent, complement, distributive, absorption
		((eql 'b-or (car elem))
			(b-simplify-element (b-simplify-element-solve-or (nth 1 elem) (nth 2 elem)) 
				(+ recurse-count 1) elem))
;		'not' rewrite rules: double negation, De Morgan's
		((eql 'b-not (car elem))
			(b-simplify-element (b-simplify-element-solve-not (nth 1 elem)) 
				(+ recurse-count 1) elem))
;		Otherwise, if no valid expressions for this expression, recurse down the lhs and rhs of elem
		(T
		 	(b-simplify-element 
		 		(list (car elem) 
		 			(b-simplify-element (nth 1 elem))
		 			(b-simplify-element (nth 2 elem))) elem))))

;;; Helper function to simplify a function with a constant as one of its expressions
;;; Uses following rules:
;;;		- Annulment
;;;		- Identity
;;;		- Negation of constant
;;; @fn-name: name of function applied to number and expression
;;; @num: constant value in expression (0 or 1)
;;; @expr: non-constant component of expression
(defun b-simplify-element-solve-constant (fn-name num expr)
	(cond
;;		(b-and 0 expr)/(b-and expr 0) = 0
;;		(b-and 1 expr)/(b-and expr 1) = e
		((eql 'b-and fn-name) 
			(if (= num 0)
				0
				expr))
;;		(b-or 0 expr)/(b-or expr 0) = e
;;		(b-or 1 expr)/(b-or expr 1) = 1
		((eql 'b-or fn-name)
			(if (= num 0)
				expr
				1))
;;		(b-not 0) = 1
;;		(b-not 1) = 0
		((eql 'b-not fn-name)
			(if (= num 0)
				1
				0))))

;;; Helper function to simplify a b-and function.
;;; Uses following rules:
;;;		- Idempotent
;;;		- Complement
;;;		- Absorption
;;; @e1: lhs of b-and expression
;;; @e2: rhs of b-and expression
(defun b-simplify-element-solve-and (e1 e2)
	(cond
;; 		Idempotent:
;;		(b-and e e) = e
		((equal e1 e2)
			e1)

;; 		Complement:
;;		(b-and (b-not e) e)/(b-and e (b-not e)) = 0
		((or 
			(and (listp e1) (eql 'b-not (car e1)) (equal (nth 1 e1) e2))
			(and (listp e2) (eql 'b-not (car e2)) (equal (nth 1 e2) e1)))
			0)

;; 		Absorption:
		((and (listp e1) (eql 'b-or (car e1)))
			(cond
;				(b-and (b-or a b) a) = a
				((equal (nth 1 e1) e2)
					e2)
;				(b-and (b-or b a) a) = a
				((equal (nth 2 e1) e2)
					e2)
				(T
					(list 'b-and e1 e2))))
		((and (listp e2) (eql 'b-or (car e2)))
			(cond
;				(b-and a (b-or a b)) = a
				((equal (nth 1 e2) e1)
					e1)
;				(b-and a (b-or b a)) = a
				((equal (nth 2 e2) e1)
					e1)
				(T
					(list 'b-and e1 e2))))

;;		Association: (as a last resort)
;;		(b-and (b-and a b) c) = (b-and a (b-and b c))
		((and (listp e1) (eql 'b-and (car e1)))
			(list 'b-and (nth 1 e1) (list 'b-and (nth 2 e1) e2)))
;;		(b-and a (b-and b c)) = (b-and (b-and a b) c)
		((and (listp e2) (eql 'b-and (car e2)))
			(list 'b-and (list 'b-and e1 (nth 1 e2)) (nth 2 e2)))

;;		Otherwise, if no appropriate rules, return the original b-and expression
		(T
			(list 'b-and e1 e2))))

;;; Helper function to simplify a b-or function.
;;; Uses following rules:
;;;		- Idempotent
;;;		- Complement
;;;		- Distributive
;;;		- Absorption
;;; @e1: lhs of b-or expression
;;; @e2: rhs of b-or expression
(defun b-simplify-element-solve-or (e1 e2)
	(cond
;; 		Idempotent:
;;		(b-or e e) = e
		((equal e1 e2)
			e1)

;; 		Complement:
;;		(b-or (b-not e) e)/(b-or e (b-not e)) = 1
		((or 
			(and (listp e1) (eql 'b-not (car e1)) (equal (nth 1 e1) e2))
			(and (listp e2) (eql 'b-not (car e2)) (equal (nth 1 e2) e1)))
			1)

;;		Distributive:
;;		(b-or (b-and e1 A) (b-and e1 B)) = (b-and e1 (b-or A B))
		((and (listp e1) (listp e2)
			  (eql 'b-and (car e1)) (eql 'b-and (car e2)))
			(cond
;				(b-or (b-and e1 A) (b-and e1 B)) = (b-and e1 (b-or A B))
				((equal (nth 1 e1) (nth 1 e2))
					(list 'b-and (nth 1 e1) (list 'b-or (nth 2 e1) (nth 2 e2))))
;				(b-or (b-and e1 A) (b-and B e1)) = (b-and e1 (b-or A B))
				((equal (nth 1 e1) (nth 2 e2))
					(list 'b-and (nth 1 e1) (list 'b-or (nth 2 e1) (nth 1 e2))))
;				(b-or (b-and A e1) (b-and e1 B)) = (b-and e1 (b-or A B))
				((equal (nth 2 e1) (nth 1 e2))
					(list 'b-and (nth 2 e1) (list 'b-or (nth 1 e1) (nth 2 e2))))
;				(b-or (b-and A e1) (b-and B e1)) = (b-and e1 (b-or A B))
				((equal (nth 2 e1) (nth 2 e2))
					(list 'b-and (nth 2 e1) (list 'b-or (nth 1 e1) (nth 1 e2))))
				(T
					(list 'b-or e1 e2))))


;;		Absorption:
		((and (listp e1) (eql 'b-and (car e1)))
			(cond
;				(b-or (b-and a b) a) = a
				((equal (nth 1 e1) e2)
					e2)
;				(b-or (b-and b a) a) = a
				((equal (nth 2 e1) e2)
					e2)
				(T
					(list 'b-or e1 e2))))
		((and (listp e2) (eql 'b-and (car e2)))
			(cond
;				(b-or a (b-and a b)) = a
				((equal (nth 1 e2) e1)
					e1)
;				(b-or a (b-and b a)) = a
				((equal (nth 2 e2) e1)
					e1)
				(T
					(list 'b-or e1 e2))))

;;		Association: (as a last resort)
;;		(b-or (b-or a b) c) = (b-or a (b-or b c))
		((and (listp e1) (eql 'b-or (car e1)))
			(list 'b-or (nth 1 e1) (list 'b-or (nth 2 e1) e2)))
;;		(b-or a (b-or b c)) = (b-or (b-or a b) c)
		((and (listp e2) (eql 'b-or (car e2)))
			(list 'b-or (list 'b-or e1 (nth 1 e2)) (nth 2 e2)))

;;		Otherwise, if no appropriate rules, return the original b-or expression
		(T
			(list 'b-or e1 e2))))

;;; Helper function to simplify a b-not function.
;;; Uses following rules:
;;;		- Double negation
;;;		- De Morgan's laws
;;; @e: expression applied to b-not
(defun b-simplify-element-solve-not (e)
	(cond
;; 		Double negation:
;;		(b-not (b-not x)) = x
		((and (listp e) (eql 'b-not (car e)))
			(nth 1 e))

;;		De Morgan's laws:
;;		(b-not (b-and a b)) = (b-or (b-not a) (b-not b))
		((and (listp e) (eql 'b-and (car e)))
			(list 'b-or (list 'b-not (nth 1 e)) (list 'b-not (nth 2 e))))
;;		(b-not (b-or a b))  = (b-and (b-not a) (b-not b))
		((and (listp e) (eql 'b-or (car e)))
			(list 'b-and (list 'b-not (nth 1 e)) (list 'b-not (nth 2 e))))

;;		Otherwise, if no appropriate rules, return the original b-and expression
		(T
			(list 'b-not e))))

(load 'matrix.clisp)
;;; ================================================================================================
;;; TESTING FUNCTIONS
;;; Adapted from http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html
;;; ================================================================================================

;;; FORMAT HELP: http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm

;;;	Print test case result
;;; @testcase-result: the call to a particular function with a testcase (expecting to return t or nil)
;;; @testcase: quoted function input for this particular testcase
;;; @expected-result: whether the testcase is expected to return t or nil
;;;
;;;	- T: to stdout
;;;	- ~:[✗~;✓~]: conditional on comparison between testcase-result and expected-result inputs
;;;	 (non-matching results (0/nil) produces ✗, matching results (1/true) produces ✓)
;;; - ~:[INVALID~; VALID ~]: conditional on whether the testcase is a valid or invalid input
;;;	for the function being called
;;;	- ~a: essentially, a regular print statement for the testcase
;;;	- ~%: print newline 
(defun print-test (testcase-result testcase expected-result)
	(format T "~:[FAIL ✗~;PASS ✓~] - ~:[invalid~; valid ~] : ~a~%" 
; 		Perform correct equality check for checking whether passed/failed
		(case (type-of expected-result) 
			; Task 1: T or nil
			(boolean (eq testcase-result expected-result))
			(null (eq testcase-result expected-result))
			; Task 2: list
			(cons (equal testcase-result expected-result))
			; Task 3: symbol
			(symbol (eql testcase-result expected-result))
			(integer (= testcase-result expected-result))
			(bit (= testcase-result expected-result)))

		(case (type-of expected-result) 
			; Task 1: nil or T
			(boolean T)
			(null nil)
			; Task 2: list
			(cons T)
			; Task 3: symbol
			(symbol T)
			(integer T)
			(bit T))
 
		testcase))

;;; Print test case result (only displaying if passed/failed and the expected result)
;;; For use with Tasks 4 and 5
(defun print-test-simple (testcase-result expected-result)
	(format T "~:[FAIL ✗~;PASS ✓~] - Expected: ~a~%"
;		Perform correct equality check for checking whether passed/failed
		(case (type-of expected-result) 
			(boolean (eq testcase-result expected-result))
			(null (eq testcase-result expected-result))
			(cons (equal testcase-result expected-result))
			(symbol (eql testcase-result expected-result))
			(integer (= testcase-result expected-result))
			(bit (= testcase-result expected-result)))
		expected-result))

;;; Print test case result (only displaying if passed/failed and the expected result)
;;; For use with Tasks 4 and 5
(defun print-test-simplify (testcase testcase-result expected-result)
	(format T "~:[FAIL ✗~;PASS ✓~] : ~a -> ~a : Expected: ~a~%"
;		Perform correct equality check for checking whether passed/failed
		(case (type-of expected-result) 
			(boolean (eq testcase-result expected-result))
			(null (eq testcase-result expected-result))
			(cons (equal testcase-result expected-result))
			(symbol (eql testcase-result expected-result))
			(integer (= testcase-result expected-result))
			(bit (= testcase-result expected-result)))
		testcase testcase-result expected-result))

;;; Recursively call print-test with some function and a list of testcases (including expected results)
;;; @fn-name: the name of the function to be called (as in function declaration)
;;;		eg. b-check-expressions-row 
;;; @testcases: a list of lists containing individual testcases (function arguments) and their 
;;;	expected result, of the following form:
;;;		((<testcase-1> <expected-result-1>) ... (<testcase-n> <expected-result-n>))
;;;		eg. (('(1 0 1) T) (('1 0) nil))
(defun perform-tests (fn-name testcases)
	(if (not (null testcases))
		(progn
			(cond 
;;			Functions with inputs: matrix
				((or 
					(eql fn-name 'b-check)
					(eql fn-name 'b-check-shape)
					(eql fn-name 'b-check-expressions)
					(eql fn-name 'b-check-expressions-row)
					(eql fn-name 'b-check-expressions-row-function)
					(eql fn-name 'b-shape))
;;			print-test call should be of following form:
;;				(print-test (b-check-expressions-row '(1 1 0 0)) '(1 1 0 0) T)
;;				(print-test (<fn-name> <fn-arg1>) <fn arg1> <expected-result>)
;;			Testcases is of following form:
;;				((<testcase-1> <expected-result-1>) ... (<testcase-n> <expected-result-n>))
;;			We call print-test with the first testcase from this list of testcases.
;;				- (caar testcases)  = <testcase-1>
;;				- (cadar testcases) = (car (cdr (car testcases)))
;;									= (car (cdr ( (<testcase-1> <expected-result-1>) )))
;;									= (car (<expected-result-1>))
;;									= <expected-result-1>
					(funcall 'print-test (funcall fn-name (caar testcases)) 
						(caar testcases) (cadar testcases)))

;;			Function with inputs: matrix, n, and m
				((eql fn-name 'b-elem)
;;			print-test call should be of following form:
;;				(print-test (b-elem '((a b) (c d)) 1 2) '((a b) (c d)) 'b)
;;				(print-test (<fn-name> <matrix input> <n> <m>) <matrix input> <expected-result>)
;;			Testcases is of following form:
;;				((<matrixinput-1> <n-1> <m-1> <expected-result-1>) ... 
;;				 (<matrixinput-n> <n-n> <m-n> <expected-result-n>))
;;			We call print-test with the first testcase from this list of testcases.
;;				- (car testcases)   = (testcase 1)
;;				- (caar testcases)  = <matrixinput-1>
					(funcall 'print-test (funcall fn-name (caar testcases) (nth 1 (car testcases)) (nth 2 (car testcases)))
						(caar testcases) (nth 3 (car testcases))))

;;			Functions with inputs: matrix-1, matrix-2
				((or 
					(eql fn-name 'b-and)
					(eql fn-name 'b-or)
					(eql fn-name 'b-simplify-element-solve-and)
					(eql fn-name 'b-simplify-element-solve-or))
;;			print-test-simple call should be of following form:
;;				(print-test-simple (<fn-name> <matrix-1> <matrix-2>) <expected-result>)
;;			Testcases is of following form:
;;				((<matrix-1-1> <matrix-2-1> <expected-result-1>) ...
;;				 (<matrix-1-n> <matrix-2-n> <expected-result-n>))
					(funcall 'print-test-simple (funcall fn-name (nth 0 (car testcases)) (nth 1 (car testcases))) 
						(nth 2 (car testcases))))

;;			Functions with inputs: matrix/lisp
				((or
					(eql fn-name 'b-not)
					(eql fn-name 'b-simplify-element-solve-not))
;;			print-test-simple call should be of following form:
;;				(print-test-simple (<fn-name> <matrix>) <expected-result>)
;;			Testcases is of following form:
;;				((<matrix-1> <expected-result-1>) ...
;;				 (<matrix-n> <expected-result-n>))
					(funcall 'print-test-simple (funcall fn-name (nth 0 (car testcases))) 
						(nth 1 (car testcases))))

;;			Functions with inputs: fn-name, list-1, list-2
				((or 
					(eql fn-name 'b-build-fn-matrix)
					(eql fn-name 'b-build-fn-matrix-row)
					(eql fn-name 'b-simplify-element-solve-constant))
;;			print-test-simple call should be of following form:
;;				(print-test-simple (<fn-name> <fn-name-arg> <list-1> <list-2>) <expected-result>)
;;			Testcases is of following form:
;;				((<fn-name-arg-1> <matrix-1-1> <matrix-2-1> <expected-result-1>) ...
;;				 (<fn-name-arg-n> <matrix-1-n> <matrix-2-n> <expected-result-n>))
					(funcall 'print-test-simple (funcall fn-name (nth 0 (car testcases)) (nth 1 (car testcases)) (nth 2 (car testcases))) 
						(nth 3 (car testcases))))

;;			Functions with inputs: matrix/list
				((or
					(eql fn-name 'b-simplify)
					(eql fn-name 'b-simplify-row)
					(eql fn-name 'b-simplify-element-solve)
					(eql fn-name 'b-simplify-element))
;;			print-test-simplify call should be of following form:
;;				(print-test-simplify <list> (<fn-name> <list>) <expected-result>)
;;			Testcases is of following form:
;;				((<list-1> <expected-result-1>) ...
;;				 (<list-n> <expected-result-n>))
					(funcall 'print-test-simplify (nth 0 (car testcases)) 
						(funcall fn-name (nth 0 (car testcases))) 
						(nth 1 (car testcases))))
			)
;;			And then recurse down the remaining testcases.
			(perform-tests fn-name (cdr testcases)))))



;;; Call all function tests.
(defun execute-test-suite ()
	(format T "============================================================~%")
	(format T "|                            1                             |~%")
	(format T "============================================================~%~%")
	(tests-b-check)
	(tests-b-check-shape)
	(tests-b-check-expressions) 
	(tests-b-check-expressions-row)
	(tests-b-check-expressions-row-function)

	(format T "============================================================~%")
	(format T "|                            2                             |~%")
	(format T "============================================================~%~%")
	(tests-b-shape)

	(format T "============================================================~%")
	(format T "|                            3                             |~%")
	(format T "============================================================~%~%")
	(tests-b-elem)

	(format T "============================================================~%")
	(format T "|                            4                             |~%")
	(format T "============================================================~%~%")
	(tests-b-and)
	(tests-b-or)
	(tests-b-not)
	(tests-b-build-fn-matrix)
	(tests-b-build-fn-matrix-row)

	(format T "============================================================~%")
	(format T "|                            5                             |~%")
	(format T "============================================================~%~%")
	(tests-b-simplify)
	(tests-b-simplify-row)
	(tests-b-simplify-element)
	(tests-b-simplify-element-solve)
	(tests-b-simplify-element-solve-constant)
	(tests-b-simplify-element-solve-and)
	(tests-b-simplify-element-solve-or)
	(tests-b-simplify-element-solve-not)
	)

;;; ================================================================================================
;;; TASK 1
;;; ================================================================================================
;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: T, if the matrix is well-formed (all rows of equal size, and contains no 
;; invalid expressions)
(defun tests-b-check ()
	(format T "b-check~%")
	(format T "=====================================~%")
	(perform-tests 'b-check '(
		; VALID: 1x1 matrix with valid element
		(((x)) T)
		; INVALID: 1x1 matrix with null list element
		((()) nil)
		; INVALID: matrix of incorrect form (the empty list)
		(() nil)
		; INVALID: matrix of incorrect form (no inner list structure)
		((1 0 1) nil)
		; VALID: 1x3 matrix with correct form
		(((1 0 1)) T)
		; VALID: 2x3 matrix with valid elements
		(((0 1 x) ((b-and a b) (b-or (b-and 1 1) (b-not symbol)) 0)) T) ))
	(format T "=====================================~%~%"))

;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: T, if all rows (internal lists) are of equal size
(defun tests-b-check-shape ()
	(format T "b-check-shape~%")
	(format T "=====================================~%")
	(perform-tests 'b-check-shape '(
		; VALID: 1x1 matrix
		(((x)) T)
		; VALID: 3x3 matrix
		(((1 2 3) (x y z) (a b c)) T)
		; VALID: 1x5 matrix
		(((1 2 3 4 5)) T)
		; VALID: 4x1 matrix
		(((1) (1) (1) (1)) T)
		; INVALID: row with extra element
		(((1 2 3) (x y z) (a b c d)) nil)
		; INVALID: row with one less element
		(((1 2) (x y z) (a b c)) nil) ))
	(format T "=====================================~%~%"))

;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: T, if the matrix doesn't contain a function is an invalid expression
(defun tests-b-check-expressions ()
	(format T "b-check-expressions~%")
	(format T "=====================================~%")
	(perform-tests 'b-check-expressions '(
		; VALID: 1x1 matrix with valid element
		(((x)) T)
		; INVALID: 1x1 matrix with invalid element
		(((100)) nil)
		; VALID: 3x3 matrix with valid elements
		(((0 1 0) ((b-and 0 0) x y) (x y (b-or sym1 sym2))) T)
		; VALID: non-square 3x2 matrix with valid elements
		(((1 0) (a b) ((b-and 0 1) (b-not word))) T)
		; INVALID: 2x2 matrix with invalid element 
		(((1 0) ((b-and 1 1) 5)) nil)
		; INVALID: 2x2 matrix with null list element
		(((1 0) (() 1)) nil)
		; INVALID: 2x2 matrix with invalid nested element 
		(((1 0) ((b-and 5 1) 1)) nil)
		; INVALID: 2x2 matrix with function call with too many arguments
		(((1 0) (1 (b-and 0 1 0))) nil) ))
	(format T "=====================================~%~%"))

;; Function input: matrix row of length m with form (a11 ... a1m) (i.e. a list)
;; Expected result: T, if the row doesn't contain an invalid expression
(defun tests-b-check-expressions-row ()
	;(print-test (b-check-expressions-row '(1 1 0 0)) '(1 1 0 0) T))
	(format T "b-check-expressions-row~%")
	(format T "=====================================~%")
	(perform-tests 'b-check-expressions-row '(
		; VALID: 0/1 matrix elements
		((1 1 0 0) T)
		; VALID: symbolp matrix elements
		((a b c) T)
		; VALID: b-and function matrix element
		((1 (b-and x y) 1) T)
		; INVALID: number != 0 or 1 as matrix element
		((1 (b-and x y) 5) nil)
		; INVALID: nil list as matrix element
		((1 () 2) nil)
		; VALID: combination of all possible elements
		(((b-and 0 1) (b-or y 0) (b-not (b-or 1 1)) 1 x) T) ))
	(format T "=====================================~%~%"))
	

;; Function input: a list from within one of the rows in the matrix (i.e. an expected function)
;; Expected result: T, if the function is a valid expression
(defun tests-b-check-expressions-row-function ()
	(format T "b-check-expressions-row-function~%")
	(format T "=====================================~%")
	(perform-tests 'b-check-expressions-row-function '(
		; INVALID: no function to be called
		(() nil)
		; VALID: b-and function call
		((b-and 0 1) T)
		; VALID: b-or function call
		((b-or x y) T)
		; VALID: b-not function call
		((b-not 0) T)
		; INVALID: unknown function call
		((b-xor y 0) nil)
		; INVALID: too many function arguments
		((b-and 1 1 1) nil)
		; VALID: nested function calls
		((b-and (b-or 1 0) (b-not a)) T)
		; VALID: arbitrary nesting of function calls
		((b-not (b-not (b-and 0 (b-not 1)))) T) ))
	(format T "=====================================~%~%"))

;;; ================================================================================================
;;; TASK 2
;;; ================================================================================================
;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: a list containing number of rows and columns; (<rows> <columns>)
(defun tests-b-shape ()
	(format T "b-shape~%")
	(format T "=====================================~%")
	(perform-tests 'b-shape '(
		; VALID: 1x1 matrix
		(((1)) (1 1))
		; VALID: no function to be called
		(((1 x y) (1 x y)) (2 3))
		; VALID: 1x5 matrix
		(((1 1 1 1 1)) (1 5))
		; VALID: 5x1 matrix
		(((1) (1) (1) (1) (1)) (5 1))
		; INVALID: ill-formed matrix
		(((1 y) (1 x y)) nil)
		; VALID: function calls
		((((b-and 1 0) (b-or 1 1)) ((b-not a) x)) (2 2))
		; VALID: nested functions
		(((1 (b-and 0 1)) ((b-not (b-or 1 0)) a)) (2 2)) ))
	(format T "=====================================~%~%"))

;;; ================================================================================================
;;; TASK 3
;;; ================================================================================================
;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;;				   n: row number to select, m: column number to select
;; Expected result: matrix element contained at row n and column m
(defun tests-b-elem ()
	(format T "b-elem~%")
	(format T "=====================================~%")
	(perform-tests 'b-elem '(
		; VALID: 1x1 matrix
		(((1)) 1 1 1)
		; VALID: 3x3 matrix (first element)
		(((a b c) (d e f) (g h i)) 1 1 a) 
		; VALID: 3x3 matrix (last element)
		(((a b c) (d e f) (g h i)) 3 3 i) 
		; VALID: n and m correct ordering (non-square matrix, n != m)
		(((a b c d) (e f g h)) 2 3 g)
		; VALID: function list return
		(((0 1) ((b-and 0 1) 0)) 2 1 (b-and 0 1))
		; VALID: nested function list return
		(((0 1) ((b-and (b-not 0) 1) 0)) 2 1 (b-and (b-not 0) 1))
		; INVALID: ill-formed matrix
		(((a b c) (d e) (g h i)) 2 3 nil)
		; INVALID: out of bounds position
		(((a b) (c d)) 3 3 nil) 
		; INVALID: n, m = 0
		(((a b) (c d)) 0 0 nil)
		))
	(format T "=====================================~%~%"))

;;; ================================================================================================
;;; TASK 4
;;; ================================================================================================
;; Function input: 2x n x m matrices of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: 1x matrix with b-and applied to the matrices, element-wise
(defun tests-b-and ()
	(format T "b-and~%")
	(format T "=====================================~%")
	(perform-tests 'b-and '(
		; VALID: 1x1 matrices
		(((1)) ((x)) (((b-and 1 x))))
		; VALID: 2x3 matrices
		(((a b c) (d e f)) ((g h i) (j k l)) 
			(((b-and a g) (b-and b h) (b-and c i)) ((b-and d j) (b-and e k) (b-and f l))))
		; INVALID: ill-formed matrix
		(((2)) ((x)) nil)
		; INVALID: non-matching shapes of matrices
		(((1)) ((x) (1)) nil)
		; INVALID: null matrix
		((()) ((1)) nil)
		; INVALID: null element in matrix
		(((a ()) (c d)) ((0 1) (1 0)) nil)
		; VALID: nested functions in input matrices
		(((a (b-not 0)) (1 1)) 
		 ((0 0) ((b-or 0 1) z))
			(((b-and a 0) (b-and (b-not 0) 0)) ((b-and 1 (b-or 0 1)) (b-and 1 z))))
		))
	(format T "=====================================~%~%"))

;; Function input: 2x n x m matrices of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: 1x matrix with b-or applied to the matrices, element-wise
(defun tests-b-or ()
	(format T "b-or~%")
	(format T "=====================================~%")
	(perform-tests 'b-or '(
		; VALID: 1x1 matrices
		(((1)) ((x)) (((b-or 1 x))))
		; VALID: 2x3 matrices
		(((a b c) (d e f)) ((g h i) (j k l)) 
			(((b-or a g) (b-or b h) (b-or c i)) ((b-or d j) (b-or e k) (b-or f l))))
		; INVALID: ill-formed matrix
		(((2)) ((x)) nil)
		; INVALID: non-matching shapes of matrices
		(((1)) ((x) (1)) nil)
		; INVALID: null matrix
		((()) ((1)) nil)
		; INVALID: null element in matrix
		(((a ()) (c d)) ((0 1) (1 0)) nil)
		; VALID: nested functions in input matrices
		(((a (b-not 0)) (1 1)) 
		 ((0 0) ((b-and 0 1) z))
			(((b-or a 0) (b-or (b-not 0) 0)) ((b-or 1 (b-and 0 1)) (b-or 1 z))))
		))
	(format T "=====================================~%~%"))

;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: matrix with b-not applied to the matrix, element-wise
(defun tests-b-not ()
	(format T "b-not~%")
	(format T "=====================================~%")
	(perform-tests 'b-not '(
		; VALID: 1x1 matrix
		(((1)) (((b-not 1))))
		; VALID: 2x3 matrix
		(((a b c) (d e f)) (((b-not a) (b-not b) (b-not c)) ((b-not d) (b-not e) (b-not f))))
		; INVALID: ill-formed matrix
		(((2)) nil)
		; INVALID: null matrix
		((()) nil)
		; INVALID: null element in matrix
		(((a ()) (c d)) nil)
		; VALID: nested functions in input matrices
		(((a (b-and 0 1)) (1 1)) 
			(((b-not a) (b-not (b-and 0 1))) ((b-not 1) (b-not 1))))
		))
	(format T "=====================================~%~%"))

;; Function input: function to be applied to each element in matrices
;;				2x n x m matrices of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: 1x matrix with fn-name applied to the matrices, element-wise
(defun tests-b-build-fn-matrix ()
	(format T "b-build-fn-matrix~%")
	(format T "=====================================~%")
	(perform-tests 'b-build-fn-matrix '(
		; VALID: 1x1 matrices
		(b-or ((1)) ((x)) (((b-or 1 x))))
		; VALID: singular 1x1 matrix (number 0 parsed for purpose of testing framework)
		(b-not ((expr)) 0 (((b-not expr))))
		; VALID: 2x3 matrices (can build with any arbitrary function name)
		(b-xor ((a b c) (d e f)) ((g h i) (j k l)) 
			(((b-xor a g) (b-xor b h) (b-xor c i)) ((b-xor d j) (b-xor e k) (b-xor f l))))
		; VALID: singular 2x3 matrix (number 0 parsed for purpose of testing framework)
		(b-not ((a b c) (d e f)) 0 (((b-not a) (b-not b) (b-not c)) ((b-not d) (b-not e) (b-not f))))
		; VALID: nested functions in input matrices
		(b-and ((a (b-not 0)) (1 1)) ((0 0) ((b-or 0 1) z))
			(((b-and a 0) (b-and (b-not 0) 0)) ((b-and 1 (b-or 0 1)) (b-and 1 z))))
		))
	(format T "================================~%~%"))

;; Function input: function to be applied to each element in matrix rows
;;				2x matrix rows of length m, of form: (a11 ... a1m) (i.e. a list)
;; Expected result: 1x matrix row with fn-name applied to the rows, element-wise
(defun tests-b-build-fn-matrix-row ()
	(format T "b-build-fn-matrix-row~%")
	(format T "=====================================~%")
	(perform-tests 'b-build-fn-matrix-row '(
		; VALID: lists of length 1
		(b-or (1) (x) ((b-or 1 x)))
		; VALID: singular list of length 1 (number 0 parsed for purpose of testing framework)
		(b-not (expr) 0 ((b-not expr)))
		; VALID: lists of length 5 (can build with any arbitrary function name)
		(b-xor (a b c d e) (f g h i j) 
			((b-xor a f) (b-xor b g) (b-xor c h) (b-xor d i) (b-xor e j)))
		; VALID: singular list of length 5
		(b-not (a b c d e) 0 ((b-not a) (b-not b) (b-not c) (b-not d) (b-not e)))
		; VALID: nested functions in input lists
		(b-and (a (b-not 0)) ((b-or 0 1) z) ((b-and a (b-or 0 1)) (b-and (b-not 0) z)))
		))
	(format T "=====================================~%~%"))

;;; ================================================================================================
;;; TASK 5
;;; ================================================================================================
;; Function input: n x m matrix of form: ((a11 ... a1m) ... (an1 ... anm)) (i.e. a list of lists)
;; Expected result: matrix with each element simplified
(defun tests-b-simplify ()
	(format T "b-simplify~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify '(
		; VALID: 1x1 matrix
		(((1)) ((1)))
		; VALID: 2x3 matrix
		(((a b c) (d e f)) ((a b c) (d e f)))
		; INVALID: ill-formed matrix
		(((2)) nil)
		; VALID: 3x2 matrix with simplifiable expressions in each element
		((((b-and x x) (b-or y y)) ((b-not (b-not z)) (b-not 0)) ((b-and a 0) (b-or b 0))) ((x y) (z 1) (0 b)))
		; VALID: 2x2 matrix with more complex expressions
		((((b-or (b-and x y) (b-and x (b-not y))) (b-and d (b-or (b-and b c) (b-and b 1))))
		 ((b-or a (b-and a b)) (b-or (b-not a) (b-not (b-and a b)))))
			((x (b-and d b)) (a (b-or (b-not a) (b-not b)))))
		))
	(format T "=====================================~%~%"))

;; Function input: matrix row of length m with form (a11 ... a1m) (i.e. a list)
;; Expected result: row of same size with simplified matrix elements
(defun tests-b-simplify-row ()
	(format T "b-simplify-row~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-row '(
		; VALID: row of length 1
		((1) (1))
		; VALID: row of length 5
		((1 1 1 1 1) (1 1 1 1 1))
		; VALID: row with single simplifiable element at first index
		(((b-and a 0) a b) (0 a b))
		; VALID: row with single simplifiable element at last index
		((a b (b-and a 0)) (a b 0))
		; VALID: row with simplifiable expressions in each element
		(((b-and x x) (b-or y y) (b-not (b-not z))) (x y z))
		; VALID: row with some more complex expressions
		(((b-or (b-and x y) (b-and x (b-not y))) 1 (b-or (b-and x y) (b-and x (b-not y)))) (x 1 x))
		))
	(format T "=====================================~%~%"))

;; Function input: 1x matrix element to simplify (and another referring to the previous iteration)
;; Expected result: simplified expression (or the original expression)
(defun tests-b-simplify-element ()
	(format T "b-simplify-element~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element '(
		; xy or (!x)y = x
		; Uses distributive, complement, identity
		((b-or (b-and x y) (b-and x (b-not y))) x)
		; d(bc or b1) = db
		; (b-and d (b-or (b-and b c) (b-and b 1))) = (b-and d b)
		; Uses identity, absorption
		((b-and d (b-or (b-and b c) (b-and b 1))) (b-and d b))
		; !(CA) or (C1 or DD)0 = !C or !A
		; Uses De Morgan's, identity
		((b-or (b-not (b-and c a)) (b-and (b-or (b-and c 1) (b-and d d)) 0)) (b-or (b-not c) (b-not a)))
		; !A(!(!(B)B or !(CD)) or 0B
		; Uses De Morgan's, double negation, identity, complement
		((b-or (b-not (b-and a (b-or (b-and (b-not b) b) (b-not (b-and c d))))) (b-and 0 b)) (b-or (b-not a) (b-and c d)))
		; !(C(10 or BB)) or !BB
		; Uses De Morgan's, annulment, identity, complement, idompetent
		((b-or (b-not (b-and c (b-or (b-and 1 0) (b-and b b)))) (b-and (b-not b) b)) (b-or (b-not C) (b-not b)))
		; (b-or (b-not (b-and (b-or (b-not (b-and c b)) (b-not d d)) b)) (b-and 1 c)) = (b-or (b-not b) c)
		; Uses De Morgan's, double negation, identity, idempotent, absorption
		; NOTE: will fail. Reaches: (B-OR (B-OR (B-AND (B-AND C B) D) (B-NOT B)) C)
		; i.e. (CBD or !B or C)
		; But no notion of associativity in rules, so cannot reach final state using absorption rule.
		((b-or (b-not (b-and (b-or (b-not (b-and c b)) (b-not d d)) b)) (b-and 1 c)) (b-or (b-not b) c))
		))
	(format T "=====================================~%~%"))

;; Function input: 1x matrix element to simplify (and another referring to the previous iteration)
;; Expected result: simplified expression (or the original expression)
(defun tests-b-simplify-element-solve ()
	(format T "b-simplify-element-solve~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element-solve '(
		; 1 or 0 in first expression
		((b-and 0 x) 0)
		; 1 or 0 in second expression
		((b-or x 1) 1)
		; b-and as function
		((b-and (b-not x) x) 0)
		; b-or as function
		((b-or (b-and x y) (b-and x z)) (b-and x (b-or y z)))
		; b-not as function
		((b-not (b-and a b)) (b-or (b-not a) (b-not b)))
		))
	(format T "=====================================~%~%"))

;; Function input: fn-name applied to the 2 expressions
;;				   the constant (0 or 1)
;;				   the non-constant expression
;; Expected result: 0, 1, or the non-constant expression
(defun tests-b-simplify-element-solve-constant ()
	(format T "b-simplify-element-solve-constant~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element-solve-constant '(
		; (b-and 0 0) = 0
		(b-and 0 0 0)
		; (b-and 0 1) = 0
		(b-and 0 1 0)
		; (b-and 1 0) = 0
		(b-and 1 0 0)
		; (b-and 0 expr)/(b-and expr 0) = 0
		(b-and 0 x 0)
		; (b-and 1 expr)/(b-and expr 1) = expr
		(b-and 1 x x)

		; (b-or 0 0) = 0
		(b-or 0 0 0)
		; (b-or 0 1) = 1
		(b-or 0 1 1)
		; (b-or 1 0) = 1
		(b-or 1 0 1)
		; (b-or 0 expr)/(b-or expr 0) = expr
		(b-or 0 x x)
		; (b-or 1 expr)/(b-or expr 1) = 1
		(b-or 1 x 1)

		; (b-not 0) = 1
		(b-not 0 nil 1)
		; (b-not 1) = 0
		(b-not 1 nil 0)
		))
	(format T "=====================================~%~%"))

;; Function input: 2x expressions applied to a b-or function
;; Expected result: simplified expression (or the original expression)
(defun tests-b-simplify-element-solve-and ()
	(format T "b-simplify-element-solve-and~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element-solve-and '(
		; Complement tests
		; (b-and (b-not e) e) = 0
		((b-not e) e 0)
		; (b-and e (b-not e)) = 0
		(e (b-not e) 0)
		; Nested function also goes to 0
		((b-not (b-and 1 0)) (b-and 1 0) 0)

		; Idempotent tests
		; (b-and e e) = e
		(e e e)
		; Nested function
		((b-and 0 1) (b-and 0 1) (b-and 0 1))

		; Absorption tests
		; (b-and (b-or a b) a) = a
		((b-or a b) a a)
		; (b-and (b-or b a) a) = a
		((b-or b a) a a)
		; (b-and a (b-or a b)) = a
		(a (b-or a b) a)
		; (b-and a (b-or b a)) = a
		(a (b-or b a) a)
		; Nested functions
		; (b-and (b-or <expr> b) <expr>) = <expr>
		((b-or (b-not (b-and 1 0)) b) (b-not (b-and 1 0)) (b-not (b-and 1 0)))

		; Association tests
		; (b-and (b-and a b) c) = (b-and a (b-and b c))
		((b-and a b) c (b-and a (b-and b c)))
		; (b-and a (b-and b c)) = (b-and (b-and a b) c)
		(a (b-and b c) (b-and (b-and a b) c))
		))
	(format T "=====================================~%~%"))

;; Function input: 2x expressions applied to a b-or function
;; Expected result: simplified expression (or the original expression)
(defun tests-b-simplify-element-solve-or ()
	(format T "b-simplify-element-solve-or~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element-solve-or '(
		; Distributive tests
		; (b-or (b-and e1 A) (b-and e1 B)) = (b-and e1 (b-or A B))
		((b-and e1 A) (b-and e1 B) (b-and e1 (b-or A B)))
		; (b-or (b-and e1 A) (b-and B e1)) = (b-and e1 (b-or A B))
		((b-and e1 A) (b-and B e1) (b-and e1 (b-or A B)))
		; (b-or (b-and A e1) (b-and e1 B)) = (b-and e1 (b-or A B))
		((b-and A e1) (b-and e1 B) (b-and e1 (b-or A B)))
		; (b-or (b-and A e1) (b-and B e1)) = (b-and e1 (b-or A B))
		((b-and A e1) (b-and B e1) (b-and e1 (b-or A B)))
		; Nested function
		((b-and (b-not x) A) (b-and (b-not x) B) (b-and (b-not x) (b-or A B)))

		; Complement tests
		; (b-or (b-not e) e) = 1
		((b-not e) e 1)
		; (b-or e (b-not e)) = 1
		(e (b-not e) 1)
		; Nested function also goes to 1
		((b-not (b-and 1 0)) (b-and 1 0) 1)

		; Idempotent tests
		; (b-or e e) = e
		(e e e)
		; Nested function
		((b-and 0 1) (b-and 0 1) (b-and 0 1))

		; Absorption tests
		; (b-or (b-and a b) a) = a
		((b-and a b) a a)
		; (b-or (b-and b a) a) = a
		((b-and b a) a a)
		; (b-or a (b-and a b)) = a
		(a (b-and a b) a)
		; (b-or a (b-and b a)) = a
		(a (b-and b a) a)
		; Nested functions
		; (b-or (b-and <expr> b) <expr>) = <expr>
		((b-and (b-not (b-and 1 0)) b) (b-not (b-and 1 0)) (b-not (b-and 1 0)))

		; Association tests
		; (b-or (b-or a b) c) = (b-or a (b-or b c))
		((b-or a b) c (b-or a (b-or b c)))
		; (b-or a (b-or b c)) = (b-or (b-or a b) c)
		(a (b-or b c) (b-or (b-or a b) c))
		))
	(format T "=====================================~%~%"))

;; Function input: 1x expression applied to a b-not function
;; Expected result: simplified expression (or the original expression)
(defun tests-b-simplify-element-solve-not ()
	(format T "b-simplify-element-solve-not~%")
	(format T "=====================================~%")
	(perform-tests 'b-simplify-element-solve-not '(
		; Double negation tests
		; Single symbol: (b-not (b-not x)) = x
		((b-not x) x)
		; More complex inner expression: (b-not (b-not <expr>)) = x
		((b-not (b-and (b-or x y) 1)) (b-and (b-or x y) 1))

		; De Morgan's laws tests
		; (b-not (b-and a b)) = (b-or (b-not a) (b-not b))
		; Simple expression
		((b-and a b) (b-or (b-not a) (b-not b)))
		; More complex expression
		((b-and (b-not a) (b-and 1 1)) (b-or (b-not (b-not a)) (b-not (b-and 1 1))))
		; (b-not (b-or a b))  = (b-and (b-not a) (b-not b))
		; Simple expression
		((b-or a b) (b-and (b-not a) (b-not b)))
		; More complex expression
		((b-or (b-not a) (b-and 1 1)) (b-and (b-not (b-not a)) (b-not (b-and 1 1))))
		))
	(format T "=====================================~%~%"))

(execute-test-suite)

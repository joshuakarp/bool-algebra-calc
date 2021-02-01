Boolean algebra matrix calculator
=================================
A calculator to perform boolean algebra operations and simplifications element-wise on matrices. Built with Common Lisp, and compiled with CLISP 2.49.60+.

For example, a 2 x 3 matrix, A, containing the elements (from left to right, top to bottom) of a, b, c, d, e, f, is represented as the following list of lists:

`(setq A '((a b c) (d e f)))`

That is, `A` is a list containing 2 lists, with each of these lists containing 3 elements. We can extend this representation for any n x m matrix.

The calculator performs simplifications on each element of a given matrix according to boolean algebra laws (uses a rewrite system), with AND (b-and), OR (b-or) and NOT (b-not) operators. The following boolean algebra laws are implemented:
* **Annulment:**
	* `(b-and x 0)` = `0`
	* `(b-or x 1)` = `1`
* **Identity:**
	* `(b-and x 1)` = `x`
	* `(b-or x 0)` = `x`
* **Idempotent:**
	* `(b-and x x)` = `x`
	* `(b-or x x)` = `x`
* **Complement:**
	* `(b-and x (b-not x))` = `0`
	* `(b-or x (b-not x))` = `1`
* **Absorption:**
	* `(b-and (b-or a b))` = `a`
	* `(b-or (b-and a b))` = `a`
* **Double negation:** `(b-not (b-not x))` = `x`
* **Distributive:** 
	* `(b-or a (b-and b c))` = `(b-and (b-or a b) (b-or a c))`
	* `(b-and a (b-or b c))` = `(b-or (b-and a b) (b-and a c))`
* **Associative:** 
	* `(b-or (b-or a b) c)` = `(b-or a (b-or b c))`
	* `(b-and (b-and a b) c)` = `(b-and a (b-and b c))`
* **De Morgan's:**
	* `(b-not (b-and a b))` = `(b-or (b-not a) (b-not b))`
	* `(b-not (b-or a b))`  = `(b-and (b-not a) (b-not b))`

For example, given the element `(b-or (b-and x y) (b-and x (b-not y)))` in some matrix, this can be simplified as follows:

	(b-or (b-and x y) (b-and x (b-not y)))
	= (b-and x (b-or y (b-not y)))          (distributive)
	= (b-and x 1)                           (complement)
	= x
	
Consequently, a call to `b-simplify` with some n x m matrix with every matrix element containing this boolean expression will result in an n x m matrix with every element containing `x`.

Testing suite
=============

The testing suite provides valid and invalid inputs for each function, ensuring that expected and unexpected results are covered. Tests can be run by executing the `tests.clisp` file, which will provide a printout of test results in stdout.

Example of test output for `b-check-shape`:

	b-check-shape
	=====================================
	PASS ✓ -  valid  : ((X))
	PASS ✓ -  valid  : ((1 2 3) (X Y Z) (A B C))
	PASS ✓ -  valid  : ((1 2 3 4 5))
	PASS ✓ -  valid  : ((1) (1) (1) (1))
	PASS ✓ - invalid : ((1 2 3) (X Y Z) (A B C D))
	PASS ✓ - invalid : ((1 2) (X Y Z) (A B C))
	=====================================

Expected output for remaining tests can be found in [testsExpected.txt](https://github.com/joshuakarp/bool-algebra-calc/blob/master/testsExpected.txt).

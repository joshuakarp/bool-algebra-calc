For example, a 2 x 3 matrix, A, containing the elements (from left to right, top to bottom) of a, b, c, d, e, f, is represented as the following list of lists:

`(setq A '((a b c) (d e f)))`

That is, A is a list containing 2 lists, with each of these lists containing 3 elements. We can extend this representation for any n x m matrix.

The calculator performs simplifications on each element of a given matrix according to boolean algebra laws (uses a rewrite system), with AND (b-and), OR (b-or) and NOT (b-not) operators. The following boolean algebra laws are implemented:
* Annulment: 
	* (b-and x 0) = 0
	* (b-or x 1) = 1
* Identity:
	* 

For example, given the 2 x 2 matrix:

`'((`

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

Expected output for remaining tests can be found in testsResult.txt.

test:
	emacs -Q --batch -l test/harness/init.el -l ert -l test/*.el -f ert-run-tests-batch-and-exit

.PHONY: test

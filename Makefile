test:
	emacs -Q --batch -l test/harness/init.el -l test/*.el -f ert-run-tests-batch-and-exit


.PHONY: test

test:
	emacs -Q --batch -l test/harness/init.el -l ert -L . -l sops-file-test.el -f ert-run-tests-batch-and-exit

.PHONY: test

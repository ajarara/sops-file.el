test:
	emacs -batch -l ert -l sops-file.el -l sops-file-test.el -f ert-run-tests-batch-and-exit

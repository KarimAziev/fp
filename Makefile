EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch -L .

.PHONY: check test checkdoc compile clean

check: test checkdoc compile

test:
	$(EMACS_BATCH) -l fp.el -l test/fp-test.el \
	  -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS_BATCH) --eval \
	  '(progn (require (quote checkdoc)) (find-file "fp.el") \
	     (let ((checkdoc-autofix-flag nil)) (checkdoc-current-buffer)))'

compile:
	$(EMACS_BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile fp.el test/fp-test.el

clean:
	$(RM) fp.elc test/fp-test.elc

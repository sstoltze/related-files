.POSIX:
EMACS = emacs

compile: related-files.elc related-files-test.elc

*-test.elc: *-files.elc

test: related-files-test.elc
	$(EMACS) -Q --batch -L . -l related-files-test.elc -f ert-run-tests-batch

clean:
	rm -f *.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

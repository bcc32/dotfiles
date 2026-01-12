TEST_FILES=$(wildcard *-test.el)
LOAD_TEST_FILES=$(patsubst %,-l %,$(TEST_FILES))

test: *.el
	emacs -batch -l ert $(LOAD_TEST_FILES) -f ert-run-tests-batch-and-exit

.PHONY: test

EMACS ?= emacs
CASK ?= cask
RM ?= rm

all: test

test: clean
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile notify.el

clean:
	${RM} -f *.elc

PHONY: all test unit

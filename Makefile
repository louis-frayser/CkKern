.SUFFIXES: .rkt
Srcs= ckkern.rkt params.rkt io.rkt  kmods.rkt
TestSrcs=unit-tests.rkt
% : %.rkt
	raco exe $<

all: ckkern
clean:
	@echo CLEAN:
	@for x in '#'* *~; do { test -e "$$x" && rm -v "$$x"; } || true ;done

ckkern: ${Srcs}
	raco exe $@.rkt


wc:
	@wc -l *.rkt Makefile *.md |sort -n

test run : ckkern
	./ckkern

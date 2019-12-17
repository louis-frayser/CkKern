.SUFFIXES: .rkt
Srcs= ckkern.rkt io.rkt 
TestSrcs=unit-tests.rkt
% : %.rkt
	raco exe $<
all:
clean:
	@echo CLEAN:
	@for x in '#'* *~; do { test -e "$$x" && rm -v "$$x"; } || true ;done

ckkern: ${Srcs}
	raco exe $@.rkt


wc:
	@wc -l *.rkt Makefile *.md |sort -n

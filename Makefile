.SUFFIXES: .rkt
Srcs=check-modules.rkt ckkern.rkt kernel-io.rkt kernsrc.rkt
TestSrcs=unit-tests.rkt
% : %.rkt
	raco exe $<
all:
clean:
	@echo CLEAN:
	@for x in *~; do { test -e "$$x" && rm -v "$$x"; } || true ;done

ckkern: ${Srcs}
	raco exe $@.rkt

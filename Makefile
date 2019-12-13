all:
clean:
	@echo CLEAN:
	@for x in *~; do { test -e "$$x" && rm -v "$$x"; } || true ;done

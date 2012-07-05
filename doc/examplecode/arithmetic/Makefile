
QPLPROGS := $(wildcard *.qpl)
QPLC := ../../compiler/src/qc

QPLCOMPILEOPTS := 


all:	$(QPLC) $(QPLCOMPILOPTS) $(QPLPROGS)

%.qpo: %.qpl
	$(QPLC) $(QPLCOMPILOPTS) $<



clean:
	@echo "Cleaning..."
	rm -f *~ *.qpo


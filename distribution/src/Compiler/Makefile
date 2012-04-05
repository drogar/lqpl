EXECUTABLE = qc

GENBASE:=../../src/

SYSDIR:=$(GENBASE)System/
DATADIR:=$(GENBASE)Data/
SHAREDDIR:=$(GENBASE)Shared/


LHS := $(wildcard *.lhs) $(SYSDIR)FilePath.lhs $(DATADIR)/Tuples.lhs $(DATADIR)/Stack.lhs

HS := Qparser.hs  Qlexer.hs $(SHAREDDIR)Version.hs
GRAMMER := Qparser.ly

GHCPARMS:=-fglasgow-exts -fwarn-incomplete-patterns 
GHCIPARMS:=-fglasgow-exts 
GHCGENSRCPARMS:=-fglasgow-exts 


GHC:=ghc
GHCI:=ghci

HAPPY:=happy
HAPPYOPTS:=-g -a -c

all: $(EXECUTABLE)

$(EXECUTABLE): $(LHS) $(HS)
	$(GHC) -c --make $(GHCGENSRCPARMS) $(HS) -i$(GENBASE)
	$(GHC) --make  $(GHCPARMS) -o $(EXECUTABLE) -i$(GENBASE) Main.lhs

doc:	$(LHS)
	lhs2TeX --poly driver.lhs > Qstack.tex
	latex Qstack
	makeindex Qstack
	bibtex Qstack
	latex Qstack

%.hs: %.ly
	$(HAPPY) $(HAPPYOPTS) $< -o $@

Qlexer.hs: Qlexer.x
	alex --ghc Qlexer.x

tags: $(LHS) 
	hasktags $(LHS)

winclean:
	@echo "Cleaning..."
	rm -f *.o *.hi tags TAGS $(EXECUTABLE) core Qlexer.hs* Qparser.hs* Qparser.info
clean:
	@echo "Cleaning..."
	rm -f *~ *.o *.hi tags TAGS $(EXECUTABLE) core Qlexer.hs* Qparser.hs* Qparser.info


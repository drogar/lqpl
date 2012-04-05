###########################
# These are the two macros that define input and output filenames
# They are probably the only two lines that you should be editing but you might want to customize some of the other parts of the file and feel free to do so
# Note that as of April 2007, code no longer included.
INFILE = thesis
INASREPORT = thesisAsReport
OUTFILE = thesis

LTXFILES = $(wildcard *.tex)
###########################

# This makefile has been modified by Tony Schellenberg based on the one available at: http://xpt.sourceforge.net/techdocs/Latex/MakefileForTeX/Latex08.000.html
# This file is not an exact copy and includes a variety of different additions but operates fundamentally on the basic priciples of its predecessor.
VPATH = .:../src/QSM:../src/Data
LATEX = latex
BIBTEX = bibtex
L2H = latex2html
PDFLATEX = pdflatex
LHS2TEX = lhs2TeX
PS2PDF = ps2pdf
DVIPS = dvips
DVIPSOPTS = -t letter -u +pdftex.map -u +Ttbbold.map -u +wolfram.map
PS2PDFOPTS = -dPDFSETTINGS=/prepress
LHS2TEXOPTS = --poly 

RERUN = "(There were undefined references|Rerun to get (cross-references|the bars) right)"
RERUNBIB = "No file.*\.bbl|Citation.*undefined"

GOAL = $(INFILE).ps
DVIFILE = $(INFILE).dvi

LHSFILES = 
#LHSFILES = Permutation.tex Stack.tex EitherLocal.tex ClassicalData.tex ClassicalStack.tex ClassComp.tex Matrix.tex LazyNum.tex Transformations.tex Basis.tex  QuantumStack.tex ControlStack.tex Instructions.tex InfList.tex Stream.tex MachineErrors.tex Instructions.tex Dump.tex QSM.tex

COPY = if test -r $*.toc; then cp $*.toc $*.toc.bak; fi 
RM = rm -f

default:		
		make clean
		make all

latex:
		make default

pdflatex:	
		make clean
		$(COPY);$(PDFLATEX) $(INFILE).tex > $(INFILE).log
		egrep -c $(RERUNBIB) $(INFILE).log && ($(BIBTEX) $(INFILE);$(COPY);$(PDFLATEX) $(INFILE).tex > $(INFILE).log) ; true
		egrep $(RERUN) $(INFILE).log && ($(COPY);$(PDFLATEX) $(INFILE).tex  > $(INFILE).log) ; true
		egrep $(RERUN) $(INFILE).log && ($(COPY);$(PDFLATEX) $(INFILE).tex  > $(INFILE).log) ; true
#		if cmp -s $(INFILE).toc $(INFILE).toc.bak; then . ;else $(PDFLATEX) $(INFILE).tex  > $(INFILE).log; fi
		$(RM) $(INFILE).toc.bak
		mv $(INFILE).pdf $(OUTFILE).pdf
# Display relevant warnings
		egrep -i "(Reference|Citation).*undefined" $(INFILE).log ; true

main:		$(DVIFILES)

all:		$(GOAL) 

$(GOAL):	$(DVIFILE) 
$(DVIFILE):	$(INFILE).tex $(LTXFILES) $(LHSFILES) mylhs.sty
		$(LATEX) $<
		$(BIBTEX) $*
		$(LATEX) $<
#for now - may add again later		$(LATEX) $<

mylhs.sty:	dr.lhs
		$(LHS2TEX) $(LHS2TEXOPTS) -o mylhs.sty dr.lhs

%.tex:		%.lhs
		$(LHS2TEX) $(LHS2TEXOPTS) -o $@ $<

#%.dvi:		%.tex
#		$(COPY);$(LATEX) $<
#		egrep -c $(RERUNBIB) $*.log && ($(BIBTEX) $*;$(COPY);$(LATEX) $<) ; true
#		egrep $(RERUN) $*.log && ($(COPY);$(LATEX) $<) ; true
#		egrep $(RERUN) $*.log && ($(COPY);$(LATEX) $<) ; true
#		if cmp -s $*.toc $*.toc.bak; then . ;else $(LATEX) $< ; fi
#		$(RM) $*.toc.bak
# Display relevant warnings
#		egrep -i "(Reference|Citation).*undefined" $*.log ; true

%.ps:		%.dvi
		dvips $< $(DVIPSOPTS) -o $@

%.pdf:		%.ps
		$(PS2PDF) $(PS2PDFOPTS) $< $@

clean:	
		rm -f *~ *.aux *.log *.bbl *.blg *.brf *.cb *.ind *.idx *.ilg *.qci \
		*.inx $(INFILE).ps *.dvi $(OUTFILE).pdf *.toc *.out *.lof *.lot \
		$(LHSFILES) mylhs.sty

NAME = salmagundi

SOURCES = $(wildcard *.texi)

FIGURES = $(wildcard *.svg)

PNGS = $(subst .svg,.png,$(FIGURES))

all: $(NAME).pdf

%.png: %.svg
	inkscape $< --export-png=$@ --export-area-drawing

$(NAME).pdf: $(SOURCES) $(PNGS)
	texi2pdf $(NAME).texi

clean:
	rm -f *.png *.pdf *.toc *.log *.aux *.cp *.cps *.fn *.fns *~ 

# Makefile for modified linkages 

EXE = linkages
SRC = linkages.f 

FC = gfortran
FFLAGS_DEBUG = -g -fbounds-check
FFLAGS_RELEASE = -O2

.PHONY: help clean

help:
	@ echo ""
	@ echo "Instructions for compiling modified linkages program:"
	@ echo ""
	@ echo "	make release: compiles a 'fast' executable for production runs"
	@ echo "	make debug: compiles an executable suitable for debugging"
	@ echo "	make clean: removes all build files"
	@ echo "	make help: displays this message (default)"
	@ echo ""
	@ echo "A successful build will create the executable: $(EXE)"
	@ echo ""

debug: FFLAGS = $(FFLAGS_DEBUG)
debug: $(EXE) ;

release: FFLAGS = $(FFLAGS_RELEASE)
release: $(EXE) ; 
	
$(EXE): $(SRC)
	$(FC) $(FFLAGS) $^ -o $@

clean: 
	rm -f $(EXE)

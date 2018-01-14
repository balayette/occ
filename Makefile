#*********************************************************************#
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 1998 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#*********************************************************************#

SOURCES = src/compiler.ml src/parser.mly
SRC_DIR = src

EXEC = compiler


CAMLC = ocamlfind ocamlc
CAMLOPT = ocamlfind ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = menhir

LIBS="batteries,ppx_nanocaml"

all:: .depend.input .depend $(EXEC)

opt : $(EXEC).opt

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmx=.cmx)

$(EXEC): $(OBJS)
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) $(CUSTOM) -o $(EXEC) -linkpkg $(OBJS)

$(EXEC).opt: $(OBJS)
	$(CAMLOPT) -I $(SRC_DIR) -package $(LIBS) $(CUSTOM) -o $(EXEC) -linkpkg $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $<

.mli.cmi:
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $<

.ml.cmx:
	$(CAMLOPT) -I $(SRC_DIR) -package $(LIBS) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -I $(SRC_DIR) -package $(LIBS) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $*.mli
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -I $(SRC_DIR) -package $(LIBS) -c $*.mli
	$(CAMLOPT) -I $(SRC_DIR) -package $(LIBS) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -I $(SRC_DIR) -package $(LIBS) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean::
	rm -f $(SRC_DIR)/*.cm[iox] *~ .*~ #*#
	rm -f $(SRC_DIR)/*.o
	rm -f $(EXEC)
	rm -f $(EXEC).opt

.depend.input: Makefile
	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (rm -f .depend.new) || \
	    (mv .depend.new .depend.input)

depend: .depend

.depend:: $(SMLIY) .depend.input
	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend

include .depend

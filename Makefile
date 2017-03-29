#
# Mettre la liste des fichiers .ml, .mly, .mll et .mli
# constituant le projet. Si un fichier b.ml dÃ©pend d'un fichier
# a.ml, a.ml doit se trouver avant dans la liste.
#

SOURCES = ast.mli parser.mly lexer.mll typing.ml amd64.ml compile.ml main.ml

# Nom du binaire

EXEC = minic

#######################################################################
# Partie gÃ©nÃ©rique, ne pas modifier.
#######################################################################


# Compilateurs

CAMLC = ocamlc nums.cma
CAMLOPT = ocamlopt nums.cmxa
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = menhir
CAMLYACC_FLAGS  = --explain --infer
# Sources

SRC_MLL = $(filter %.mll, $(SOURCES))
SRC_MLY = $(filter %.mly, $(SOURCES))
SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: depend $(EXEC)

opt: depend $(EXEC).opt


$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c -bin-annot $<

.mli.cmi:
	$(CAMLC) -c -bin-annot $<

.ml.cmx:
	$(CAMLOPT) -c -bin-annot $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c -bin-annot $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c -bin-annot $*.ml

.mly.cmo:
	$(CAMLYACC) $(CAMLYACC_FLAGS) $<
	$(CAMLC) -c -bin-annot $*.mli
	$(CAMLC) -c -bin-annot $*.ml

.mly.cmx:
	$(CAMLYACC) $(CAMLYACC_FLAGS) $<
	$(CAMLOPT) -c -bin-annot $*.mli
	$(CAMLOPT) -c -bin-annot $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c -bin-annot $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $(CAMLYACC_FLAGS) $<

clean:
	rm -f *.cm[iox] *.cmt *.cmti *~ .*~ *.o parser.conflicts
	rm -f $(SRC_MLL:.mll=.ml) $(SRC_MLY:.mly=.ml) $(SRC_MLY:.mly=.mli)
	rm -f $(EXEC)
	rm -f $(EXEC).opt
	@cd tests && $(MAKE) clean



depend: $(SMLIYL)
	echo $(SMLIYL)
	$(CAMLDEP) $(SMLIYL) $(SRC_MLY:.mly=.mli) > depend
	$(CAMLYACC) --depend $(SRC_MLY) >> depend 2>/dev/null

-include depend


# Tests unitaires (pas portable probablement...)

.PHONY: test

test:
	@cd tests && $(MAKE)

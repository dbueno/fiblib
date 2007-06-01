OCAMLMAKEFILE := ./OCamlMakefile

# Find otags.
OTAGS := $(shell which otags 2> /dev/null)

export OCAMLFLAGS += -w Aeszy -dtypes
export OCAMLDOCFLAGS += -keep-code -sort

PACKS ?= oUnit

SOURCES ?= \
  fibHeap.mli fibHeap.ml \
  fibHeap_full.ml

RESULT ?= fibheap

DOC_DIR := ../doc
DOC_FILES := $(filter-out %.mly %.mll, $(SOURCES))

# The following assignments must *not* use `:=', because the right-hand
# side should only be evaluated after all compilation, the first time
# LIBINSTALL_FILES is referred to. If `:=' is used, some .cmi files are
# missed in some cases. Hence, `='.
INSTALL_SOURCES = $(filter-out %_test.ml, $(SOURCES))
MLIS = $(filter %.mli, $(INSTALL_SOURCES))
CMIS = $(filter-out %_test.cmi, $(wildcard *.cmi))
LIBINSTALL_FILES = $(MLIS) $(CMIS) $(wildcard *.cma) $(wildcard *.cmxa) $(RESULT).a



all: debug-code-library
ifneq (,$(OTAGS))
	$(MAKE) TAGS
endif


# Regenerate tags if the $(RESULT) has changed.
TAGS: $(RESULT).cma
	$(OTAGS) -r .


test_fibheap: all
	$(MAKE) dc RESULT="test_fibheap" SOURCES="$(SOURCES) fibHeap_test.ml"

clean::
	rm -f *_test
	rm -f *.annot
	rm -f test_fibheap fibHeap_test.cm*


include $(OCAMLMAKEFILE)

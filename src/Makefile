# fiblib Makefile
# See ../LICENSE.

# OUnit is required if you would like to run the unit tests.

OCAMLMAKEFILE := ./OCamlMakefile

# Find otags.
OTAGS := $(shell which otags 2> /dev/null)

export OCAMLFLAGS += -w Aeszy -dtypes
export OCAMLDOCFLAGS += -keep-code -sort

SOURCES ?= fibHeap.mli fibHeap.ml

RESULT ?= fiblib

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



all: debug-code-library native-code-library
ifneq (,$(OTAGS))
	$(MAKE) TAGS
endif

.PHONY: dist
dist: all
	(cd ../ && ./dist.sh)
	@echo "Distribution tar is in the toplevel directory."


# Regenerate tags if the $(RESULT) has changed.
TAGS: $(RESULT).cma
	$(OTAGS) -r .


.PHONY: test_fibheap_prepare
test_fibheap_prepare:
	ln -sf fibHeap.ml fibHeap_full.ml

test_fibheap: all test_fibheap_prepare
	$(MAKE) dc RESULT="test_fibheap" PACKS="oUnit" SOURCES="$(SOURCES) fibHeap_full.ml fibHeap_test.ml"

clean::
	rm -f *_test
	rm -f *.annot
	rm -f test_fibheap fibHeap_test.cm*
	rm -f fibHeap_full.ml fibHeap_full.cm*
	rm -f TAGS


include $(OCAMLMAKEFILE)

BPLROOT=../bpl/src

MOSMLC  	= mosmlc
MLYACC          = mlyacc
MLLEX           = mllex
MAKEFILE	= Makefile
EXEEXT		=

MOSMLFLAGS 	= -liberal
BPLDEPENDENCIES := $(shell find lib/ -name Dependencies)
MOSMAKEDIRS 	:= . $(patsubst %/Dependencies,%,$(BPLDEPENDENCIES))
MOSMAKE 	= $(BPLROOT)/lib/mosmake

include $(MOSMAKE)/Makefile.inc

# Parsers and lexers
GRMFILES := $(shell find . -name '*.grm')
LEXFILES := $(shell find . -name '*.lex')
PARSINGFILES := $(patsubst %.grm,%.grm-sig.sml,$(GRMFILES)) $(patsubst %.grm,%.grm.sml,$(GRMFILES)) $(patsubst %.lex,%.lex.sml,$(LEXFILES))

%.lex.sml: %.lex
	   $(MLLEX) $<

%.grm-sig.sml %.grm.sml: %.grm
	   $(MLYACC) $< && mv $*.grm.sig $*.grm-sig.sml

BPLROOT=../../bpl/src

MOSMLC  	= mosmlc
MLYACC          = mlyacc
MLLEX           = mllex
MAKEFILE	= Makefile
EXEEXT		=
SHELL		= /bin/bash

MOSMLFLAGS 	= -liberal
DEPENDENCIES    := $(shell find -L . -name Dependencies)
MOSMAKEDIRS 	:= $(patsubst %/Dependencies,%,$(DEPENDENCIES))
MOSMAKE 	= $(BPLROOT)/lib/mosmake

config:
	echo $(MOSMAKEDIRS)

include $(MOSMAKE)/Makefile.inc

# Parsers and lexers
GRMFILES := $(shell find . -name '*.grm')
LEXFILES := $(shell find . -name '*.lex')
PARSINGFILES := $(patsubst %.grm,%.grm-sig.sml,$(GRMFILES)) $(patsubst %.grm,%.grm.sml,$(GRMFILES)) $(patsubst %.lex,%.lex.sml,$(LEXFILES))

%.lex.sml: %.lex
	   $(MLLEX) $<

%.grm-sig.sml %.grm.sml: %.grm
	   $(MLYACC) $< && mv $*.grm.sig $*.grm-sig.sml

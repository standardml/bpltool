BPLROOT=../bpl/src

MOSMLC  	= mosmlc
MAKEFILE	= Makefile
EXEEXT		=

MOSMLFLAGS 	= -liberal
BPLDEPENDENCIES := $(shell find lib/ -name Dependencies)
MOSMAKEDIRS 	:= . $(patsubst %/Dependencies,%,$(BPLDEPENDENCIES))
MOSMAKE 	= $(BPLROOT)/lib/mosmake

include $(MOSMAKE)/Makefile.inc


config:
	@echo $(MOSMAKEDIRS)
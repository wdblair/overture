#
# Building the Overture compiler with the in-tree ATS2 toolchain
#

PATSHOME ?= $(abspath $(CURDIR)/../ATS2-Postiats-int-0.4.2)
export PATSHOME
export PATH := $(PATSHOME)/bin:$(PATH)

include $(PATSHOME)/share/atsmake-pre.mk

MYTARGET=overture

SOURCES_SATS := \
  overture_location.sats \
  overture_symbol.sats \
  overture_error.sats \
  overture_lexing.sats \
  overture_fixity.sats \
  overture_syntax.sats \
  overture_parsing.sats \
  overture_staexp.sats \
  overture_trans12.sats \
  overture_constraint.sats \
  overture_trans3.sats \
  overture_solver.sats \
  overture_erasure.sats \
  overture_ccomp.sats

SOURCES_DATS := \
  overture_location.dats \
  overture_symbol.dats \
  overture_error.dats \
  overture_lexing.dats \
  overture_fixity.dats \
  overture_syntax.dats \
  overture_parsing.dats \
  overture_staexp.dats \
  overture_trans12.dats \
  overture_constraint.dats \
  overture_trans3.dats \
  overture_solver.dats \
  overture_erasure.dats \
  overture_ccomp.dats \
  overture_main.dats

include $(PATSHOME)/share/atsmake-post.mk

###### end of [Makefile] ######

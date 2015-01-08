#******************************************************************************
#
# This source code is part of the LAMB library.
#
# Written by Urban Borstnik.
#
# Inspired by CP2K and DBCSR.
#
# Copyright (C) 2012, 2013, 2014 Urban Borstnik.
#
# The LAMB library is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of the
# License, or (at your option) any later version.
#
# LAMB is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LAMB.  If not, see <http://www.gnu.org/licenses/>.
#
# If you want to redistribute modifications, please note that derived
# work must not be called LAMB, Lamb, lamb, libLAMB, libLamb, liblamb
# nor any other case variation.  Details are found in the README &
# COPYING files.  If they are missing, get the official version at the
# http://lamb.borstnik.net/ website.
#
# We ask you to cite the published articles on this software.  You can
# find a list in the README file in the main directory.
#
#******************************************************************************


.SUFFIXES:

.PHONY: all deps clean


space := $(subst ,, )

LASTDIR = $(lastword $(subst /, ,$(dir $(PWD)/)))
ifeq ($(LASTDIR),src)
ROOTDIR=$(PWD)/..
endif
ifeq ($(LASTDIR),makefiles)
ROOTDIR=$(PWD)/..
endif

BUILDDIR = $(ROOTDIR)/..

ifeq ($(LAMB_VERSION),)
VERSION = popt
else
VERSION = $(LAMB_VERSION)
endif
ifeq ($(LAMB_ARCH),)
ARCH = $(shell hostname -s)
else
ARCH = $(LAMB_ARCH)
endif

PKGNAME = lamb
ARCHDIR = $(ROOTDIR)/arch
SRCDIR = $(ROOTDIR)/src
GENDIR = autogen
SRCGENDIR = $(ROOTDIR)/src/$(GENDIR)
SRCTOUCHDIR = $(ROOTDIR)/src/touchup
OBJDIR = $(BUILDDIR)/obj/$(ARCH).$(VERSION)
EXEDIR = $(BUILDDIR)/exe/$(ARCH)
TOOLDIR = $(ROOTDIR)/tools
MAKEFILE = $(ROOTDIR)/makefiles/Makefile

all: $(EXEDIR)/$(PKGNAME)_test.$(VERSION).done



ALL_VERSION = popt pdbg sopt sdbg hopt hdbg

ARCHFILE = $(ARCHDIR)/$(ARCH).$(VERSION)
include $(ARCHFILE)

vpath %.F90 $(SRCDIR) $(SRCGENDIR) $(SRCTOUCHDIR)
vpath %.h $(SRCDIR) $(SRCGENDIR) $(SRCTOUCHDIR)
vpath %.uses $(SRCGENDIR)
vpath %.publics $(SRCGENDIR)
vpath %.interfaces $(SRCGENDIR)
vpath %.contains $(SRCGENDIR)
vpath %.common_decl $(SRCTOUCHDIR)
vpath %.d $(OBJDIR)
vpath %.d2 $(OBJDIR)

.PHONY: deps
deps: $(SRCDIR)/lamb.dep

include $(SRCDIR)/SOURCE_MANIFEST
LAMB_MAIN_OBJ = $(addsuffix .o,$(LAMB_MAIN_MOD))
LAMB_MAIN_SRC = $(addsuffix .F90,$(LAMB_MAIN_MOD))
LAMB_MAIN_DEP = $(addsuffix .d,$(LAMB_MAIN_MOD))

ifeq ($(REAL_RUN),1)
include $(addprefix $(OBJDIR)/,$(LAMB_MAIN_SRC:.F90=.d))
endif

$(SRCGENDIR):
	mkdir -p $@

$(OBJDIR):
	mkdir -p $@

$(EXEDIR):
	mkdir -p $@

# The test program
.PHONY: $(EXEDIR)/$(PKGNAME)_test.$(VERSION).done
$(EXEDIR)/$(PKGNAME)_test.$(VERSION).done: | $(EXEDIR) $(OBJDIR)
	+$(MAKE) -C $(OBJDIR) -f $(MAKEFILE) REAL_RUN=1 $(basename $@)

$(EXEDIR)/$(PKGNAME)_test.$(VERSION): $(LAMB_MAIN_OBJ) $(LAMB_MAIN_DEP)
	$(LD) $(LDFLAGS) -o $@ $(LAMB_MAIN_OBJ) $(LIBS)


####################################
# Multi-template users. Files that use files generated templates.

# First define some helper functions
define wordify
	$(subst _, , $(basename $(1)))
endef
define n_u_words
	$(words $(call wordify,$(1)))
endef
define n_u_words_m1
	$(words $(wordlist 2, $(call n_u_words,$(call wordify,$(1))), $(call wordify,$(1))))
endef
define insert_X
	$(subst $(space),_,$(wordlist 1,$(call n_u_words_m1,$(1)),$(call wordify,$(1)))_X_$(lastword $(call wordify,$(1)))).F90
endef

define interface_template
# The .o and .mod files built or compiled from src/touchoup/*.F90 depend on the *.F90 file itself as well as the included files.
$$(basename $(1)).uses $$(basename $(1)).publics $$(basename $(1)).interfaces $$(basename $(1)).contains: $$(notdir $$(call insert_X,$(1))) | $(SRCGENDIR)
	cd $$(dir $$<); python ../tools/makeInterface.py $$(notdir $$<)

$$(basename $(1)).mod $$(basename $(1)).o: $$(OBJDIR)/$$(basename $(1)).uses.d

ifeq ($(REAL_RUN),1)
include $$(OBJDIR)/$$(basename $(1)).uses.d
endif

endef

$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*util.F90),$(eval $(call interface_template,$(notdir $(touchname)))))
$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*types.F90),$(eval $(call interface_template,$(notdir $(touchname)))))
$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*methods.F90),$(eval $(call interface_template,$(notdir $(touchname)))))
$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*operations.F90),$(eval $(call interface_template,$(notdir $(touchname)))))
$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*tests.F90),$(eval $(call interface_template,$(notdir $(touchname)))))
$(foreach touchname,$(wildcard $(SRCTOUCHDIR)/*tests.F90),$(shell echo $(call interface_template,$(notdir $(touchname)))))
# End of template users.
####################################

####################################
# Templates.

# Instructions for building the auto-generated files from templates.
define autogen_template
.PRECIOUS: %_$(1)_$(2).F90
%_$(1)_$(2).F90: $$(SRCDIR)/%_X_$(2).F90 | $(SRCGENDIR)
	sed -e s/_X/_$(1)/g -e "s/doubleprecision/$(3)/g" < $$^ > $(SRCGENDIR)/$$@

$$(OBJDIR)/%_$(1)_$(2).d: %_$(1)_$(2).F90
	$$(TOOLDIR)/finddep.sh $$(SRCGENDIR)/$$(notdir $$<) > $$@
endef

.PRECIOUS: %_d_types.F90 %_d_methods.F90 %_d_operations.F90 %_d_tests.F90 %_d_util.F90
.PRECIOUS: %_s_types.F90 %_s_methods.F90 %_s_operations.F90 %_s_tests.F90 %_s_util.F90
.PRECIOUS: %_c_types.F90 %_c_methods.F90 %_c_operations.F90 %_c_tests.F90 %_c_util.F90
.PRECIOUS: %_z_types.F90 %_z_methods.F90 %_z_operations.F90 %_z_tests.F90 %_z_util.F90

.PRECIOUS: %_i_types.F90 %_i_methods.F90 %_i_operations.F90 %_i_tests.F90 %_i_util.F90
.PRECIOUS: %_l_types.F90 %_l_methods.F90 %_l_operations.F90 %_l_tests.F90 %_l_util.F90
.PRECIOUS: %_b_types.F90 %_b_methods.F90 %_b_operations.F90 %_b_tests.F90 %_b_util.F90
.PRECIOUS: %_w_types.F90 %_w_methods.F90 %_w_operations.F90 %_w_tests.F90 %_w_util.F90

.PRECIOUS: %_u_types.F90 %_u_methods.F90 %_u_operations.F90 %_u_tests.F90 %_u_util.F90

# This could be looped (over s, d, ...) but would become even less clear.
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,s,$(ftype),real(kind=real_4))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,d,$(ftype),real(kind=real_8))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,c,$(ftype),complex(kind=real_4))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,z,$(ftype),complex(kind=real_8))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,i,$(ftype),integer(kind=int_4))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,l,$(ftype),integer(kind=int_8))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,b,$(ftype),integer(kind=int_1))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,w,$(ftype),integer(kind=int_2))))
$(foreach ftype,types methods operations tests util,$(eval $(call autogen_template,u,$(ftype),character)))
# End of templates.
####################################

####################################
# General pattern rules.

%.o: | $(OBJDIR)

%.mod: | $(OBJDIR)

%.mod: %.o %.F90 $(OBJDIR)/%.d
	@true

%.o: $(SRCGENDIR)/%.F90 $(OBJDIR)/%.d
	$(FC) -c $(FCFLAGS) -I$(SRCDIR) -I$(SRCGENDIR) $(filter %.F90,$<)

%.o: $(SRCTOUCHDIR)/%.F90 $(OBJDIR)/%.d
	$(FC) -c $(FCFLAGS) -I$(SRCDIR) -I$(SRCGENDIR) $(filter %.F90,$<)

%.o: $(SRCDIR)/%.F90 $(OBJDIR)/%.d
	$(FC) -c $(FCFLAGS) -I$(SRCDIR) -I$(SRCGENDIR) $(filter %.F90,$<)

$(OBJDIR)/%.uses.d: %.uses | $(OBJDIR)
	$(TOOLDIR)/finddep.sh $(SRCGENDIR)/$(notdir $<) > $@

$(OBJDIR)/%.d: %.F90 | $(OBJDIR)
	$(TOOLDIR)/finddep.sh $< > $@
# End of general pattern rules.
####################################



.PHONY: clean clean_obj clean_exe clean_gen superclean
clean: | clean_obj clean_exe
superclean: | clean clean_gen
clean_obj:
	rm -fr $(OBJDIR)
clean_exe:
	rm -fr $(EXEDIR)
clean_gen: 
	rm -fr $(SRCGENDIR)

.PHONY: dep
dep: $(LAMB_MAIN_DEP)

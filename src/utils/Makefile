#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2017, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include Makefile.KIM_Config

VPATH = ../

# Set defines
KIMDEFINES = $(KIM_DEFINE_VALUES)

all: $(full_package_name)-build-config \
     $(full_package_name)-collections-info

# Add the local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

%uild-config.o: KIM_DEFINE_VALUES += -D 'CC_STRING="$(CC)"' \
                                     -D 'CXX_STRING="$(CXX)"' \
                                     -D 'FC_STRING="$(FC)"' \
                                     -D 'LD_STRING="$(LD)"' \
                                     -D 'INCLUDES_STRING="$(if $(filter-out .build-config.o,$@),$(KIMINCLUDEFLAGS),-I$(package_dir)/include)"' \
                                     -D 'CFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(CFLAGS)"' \
                                     -D 'CXXFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(CXXFLAGS)"' \
                                     -D 'FFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(FFLAGS)"' \
                                     -D 'LDFLAGS_STRING="$(ARCHFLAG) $(if $(filter-out .build-config.o,$@),$(KIMLDFLAGS),-L$(package_dir)) $(LDFLAGS)"' \
                                     -D 'LDLIBS_STRING="$(KIMLDLIBS) $(XLANGLDLIBS) $(LDLIBS)"' \
                                     -D 'OBJONLYFLAG_STRING="$(OBJONLYFLAG)"' \
                                     -D 'OUTPUTINFLAG_STRING="$(OUTPUTINFLAG)"' \
                                     -D 'MAKEFILEKIMCONFIG_STRING="$(if $(filter-out .build-config.o,$@),$(KIM_DIR),$(package_dir))/Makefile.KIM_Config"' \
                                     -D 'VERSION_STRING="$(VERSION_FULL)"'
%build-config.o: build-config.cpp $(KIM_MAKE_FILES)
	@printf "Creating... $@.\n"
	$(QUELL)$(CXX) $(KIM_CXXTYPE_STANDARD_COMPILE_LIST) $< $(OUTPUTINFLAG) $@

$(full_package_name)-build-config: build-config.o .build-config.o
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< $(XLANGLDLIBS)
	$(QUELL)$(LD) $(ARCHFLAG) $(LDFLAGS) $(OUTPUTINFLAG) .$@ .$< $(XLANGLDLIBS)

collections-info.o: collections-info.cpp old_KIM_API_DIRS.h $(KIM_MAKE_FILES)

$(full_package_name)-collections-info: collections-info.o lib$(KIM_LIB_BUILD).so .lib$(KIM_LIB_BUILD).so

	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< ../lib$(KIM_LIB_BUILD).so $(XLANGLDLIBS) $(LDLIBS)
	$(QUELL)$(LD) $(ARCHFLAG) $(LDFLAGS) $(OUTPUTINFLAG) .$@ $< ../.lib$(KIM_LIB_BUILD).so $(XLANGLDLIBS) $(LDLIBS)


.PHONY: all clean install installdirs uninstall

clean:
	@printf "Cleaning... src/utils.\n"
	$(QUELL)rm -f *.o .*.o $(full_package_name)-build-config .$(full_package_name)-build-config \
                   $(full_package_name)-collections-info .$(full_package_name)-collections-info


install_bindir = $(dest_package_dir)/bin
install_nodest_bindir = $(package_dir)/bin
install_bin = $(full_package_name)-build-config $(full_package_name)-collections-info

install-check:
        # should we check that the installed stuff is actually dynamic-load and the right settings (32bit, etc.)?
	$(QUELL)if test -d "$(dest_package_dir)"; then \
                  rm -rf "$(install_bindir)"; \
                  for fl in $(install_bin); do rm -f "$(DESTDIR)$(bindir)/$$fl"; done; \
                fi

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(bindir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_bindir)"

install: all installdirs
	@printf "Installing...($(dest_package_dir))................................. bin"
        # Install bin files
	$(QUELL)for fl in $(install_bin); do $(INSTALL_PROGRAM) -m 0755 ".$$fl" "$(install_bindir)/$$fl"; done
	$(QUELL)for fl in $(install_bin); do ln -fs "$(install_nodest_bindir)/$$fl" "$(DESTDIR)$(bindir)/$$fl"; done
	@printf ".\n"

uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. bin.\n"
        # Uninstall bin files
	$(QUELL)for fl in $(install_bin); do if test -L "$(DESTDIR)$(bindir)/$$fl"; then rm -f "$(DESTDIR)$(bindir)/$$fl"; fi; done

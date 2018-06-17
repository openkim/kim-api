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
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard ../../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../../Makefile.KIM_Config

VPATH = ../

# Set include flags
KIMINCLUDEFLAGS = -I../

all: $(full_package_name)-descriptor-file-match \
     $(full_package_name)-build-config \
     $(full_package_name)-collections-info \
     $(full_package_name)-collections-management \
     $(full_package_name)-simulator-model \
     $(full_package_name)-activate \
     $(full_package_name)-deactivate

# Add the local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

descriptor-file-match.o: descriptor-file-match.cpp KIM_API.h KIM_API_status.h $(KIM_MAKE_FILES)

$(full_package_name)-descriptor-file-match: descriptor-file-match.o lib$(KIM_LIB_BUILD).so
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< ../lib$(KIM_LIB_BUILD).so $(XLANGLDLIBS) $(LDLIBS)

build-config.o: KIMDEFINES = -D 'CC_STRING="$(CC)"' \
                             -D 'CXX_STRING="$(CXX)"' \
                             -D 'FC_STRING="$(FC)"' \
                             -D 'LD_STRING="$(LD)"' \
                             -D 'INCLUDES_STRING="-I$(package_dir)/include"' \
                             -D 'CFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(CFLAGS)"' \
                             -D 'CXXFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(CXXFLAGS)"' \
                             -D 'FFLAGS_STRING="$(ARCHFLAG) $(PICFLAG) $(FFLAGS)"' \
                             -D 'LDFLAGS_STRING="$(ARCHFLAG) -L$(package_dir) $(LDFLAGS)"' \
                             -D 'LDLIBS_STRING="-l$(KIM_LIB) $(LDLIBS)"' \
                             -D 'XLANGLDLIBS_STRING="$(XLANGLDLIBS)"' \
                             -D 'FNOMAINFLAG_STRING="$(FNOMAINFLAG)"' \
                             -D 'OBJONLYFLAG_STRING="$(OBJONLYFLAG)"' \
                             -D 'OUTPUTINFLAG_STRING="$(OUTPUTINFLAG)"' \
                             -D 'MAKEFILEKIMCONFIG_STRING="$(package_dir)/Makefile.KIM_Config"' \
                             -D 'LIBEXECPATH_STRING="$(libexecdir)/$(full_package_name)"' \
                             -D 'VERSION_STRING="$(VERSION_FULL)"'

build-config.o: build-config.cpp $(KIM_MAKE_FILES)

$(full_package_name)-build-config: build-config.o
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< $(XLANGLDLIBS)

collections-info.o: collections-info.cpp KIM_API_DIRS.h $(KIM_MAKE_FILES)

$(full_package_name)-collections-info: collections-info.o lib$(KIM_LIB_BUILD).so
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< ../lib$(KIM_LIB_BUILD).so $(LDDYNAMICLIB) $(XLANGLDLIBS) $(LDLIBS)

simulator-model.o: KIMDEFINES = -D 'MODELLIBFILE="lib$(MODEL_LIB)"'
simulator-model.o: simulator-model.cpp KIM_API_DIRS.h $(KIM_MAKE_FILES)

$(full_package_name)-simulator-model: simulator-model.o lib$(KIM_LIB_BUILD).so
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDFLAGS) $(OUTPUTINFLAG) $@ $< ../lib$(KIM_LIB_BUILD).so $(LDDYNAMICLIB) $(XLANGLDLIBS) $(LDLIBS)

$(full_package_name)-collections-management: collections-management.sh $(KIM_MAKE_FILES)
	@printf "Creating... $@.\n"
	$(QUELL)sed -e 's|###COLLECTIONS#INFO#UTILITY###|$(package_dir)/libexec/$(full_package_name)/$(full_package_name)-collections-info|g' \
                    -e 's|###BUILD#CONFIG###|$(package_dir)/bin/$(full_package_name)-build-config|g' \
                    -e 's|###MAJOR#VERSION###|$(VERSION_MAJOR)|g' collections-management.sh > $@
	$(QUELL)chmod 0755 $@

$(full_package_name)-activate: activate.sh $(KIM_MAKE_FILES)
	@printf "Creating... $@.\n"
	$(QUELL)sed -e 's|###FULL#PACKAGE#NAME###|$(full_package_name)|g' \
                    -e 's|###PACKAGE#DIR###|$(package_dir)|g' \
                    -e 's|###SYSCONFDIR###|$(sysconfdir)|g' activate.sh > $@
	$(QUELL)chmod 0755 $@

$(full_package_name)-deactivate: deactivate.sh $(KIM_MAKE_FILES)
	@printf "Creating... $@.\n"
	$(QUELL)sed -e 's|###FULL#PACKAGE#NAME###|$(full_package_name)|g' \
                    -e 's|###PACKAGE#DIR###|$(package_dir)|g' \
                    -e 's|###SYSCONFDIR###|$(sysconfdir)|g' deactivate.sh > $@
	$(QUELL)chmod 0755 $@


.PHONY: all clean install installdirs uninstall

clean:
	@printf "Cleaning... src/utils.\n"
	$(QUELL)rm -f *.o .*.o $(full_package_name)-descriptor-file-match \
                   $(full_package_name)-build-config \
                   $(full_package_name)-collections-info \
                   $(full_package_name)-collections-management \
                   $(full_package_name)-simulator-model \
                   $(full_package_name)-activate \
                   $(full_package_name)-deactivate


install_bindir = $(dest_package_dir)/bin
install_nodest_bindir = $(package_dir)/bin
install_libexecdir = $(dest_package_dir)/libexec
install_nodest_libexecdir = $(package_dir)/libexec
install_bin = $(full_package_name)-build-config $(full_package_name)-collections-management \
              $(if $(filter-out $(patsubst %,%/bin,$(standard_system_prefixes)),$(bindir)),$(full_package_name)-activate $(full_package_name)-deactivate,)
install_libexec = $(full_package_name)-descriptor-file-match $(full_package_name)-collections-info $(full_package_name)-simulator-model

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(bindir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(libexecdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_bindir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_libexecdir)/$(full_package_name)"

install: all installdirs
	@printf "Installing...($(dest_package_dir))................................. bin.\n"
	@printf "Installing...($(dest_package_dir))................................. libexec.\n"
        # Install bin files
	$(QUELL)for fl in $(install_bin); do $(INSTALL_PROGRAM) -m 0755 "$$fl" "$(install_bindir)/$$fl"; done
	$(QUELL)for fl in $(install_bin); do ln -fs "$(install_nodest_bindir)/$$fl" "$(DESTDIR)$(bindir)/$$fl"; done
	$(QUELL)for fl in $(install_libexec); do $(INSTALL_PROGRAM) -m 0755 "$$fl" "$(install_libexecdir)/$(full_package_name)/$$fl"; done
	$(QUELL)ln -fhs "$(install_nodest_libexecdir)/$(full_package_name)" "$(DESTDIR)$(libexecdir)/$(full_package_name)"

uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. bin.\n"
	@printf "Uninstalling...($(dest_package_dir))................................. libexec.\n"
        # Uninstall bin files
	$(QUELL)for fl in $(install_bin); do if test -L "$(DESTDIR)$(bindir)/$$fl"; then rm -f "$(DESTDIR)$(bindir)/$$fl"; fi; done
	$(QUELL) rmdir "$(DESTDIR)$(bindir)" > /dev/null 2>&1 || true
	$(QUELL)for fl in $(install_bin); do if test -f "$(DESTDIR)$(install_bindir)/$$fl"; then rm -f "$(DESTDIR)$(install_bindir)/$$fl"; fi; done
	$(QUELL) rmdir "$(install_bindir)" > /dev/null 2>&1 || true
	$(QUELL)for fl in $(install_libexec); do if test -f "$(DESTDIR)$(install_libexecdir)/$(full_package_name)/$$fl"; then rm -f "$(DESTDIR)$(install_libexecdir)/$(full_package_name)/$$fl"; fi; done
	$(QUELL) rmdir "$(install_libexecdir)/$(full_package_name)" > /dev/null 2>&1 || true
	$(QUELL) rmdir "$(install_libexecdir)" > /dev/null 2>&1 || true
	$(QUELL) if test -L "$(DESTDIR)$(libexecdir)/$(full_package_name)"; then rm -f "$(DESTDIR)$(libexecdir)/$(full_package_name)"; fi
	$(QUELL) rmdir "$(DESTDIR)$(libexecdir)" > /dev/null 2>&1 || true

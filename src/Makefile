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
#    Ellad B. Tadmor
#    Valeriu Smirichinski
#

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard ../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../Makefile.KIM_Config

# Definition of c and fortran .o file list
OBJC = KIM_API.o KIM_API_C.o kim_str.o Unit_Handling.o KIM_AUX.o
OBJF90 = KIM_API_F.o
OBJF03 = KIM_API_F03.o KIM_API_F03_Helper.o

#fortran on/of
ifdef KIM_NO_FORTRAN
   ALLOBJ = $(OBJC)
else
   ALLOBJ = $(OBJC) $(OBJF03)
endif

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

all: objects libs

objects: $(ALLOBJ) KIM_API_DIRS.o
libs: lib$(KIM_LIB).so

lib$(KIM_LIB).so: lib$(KIM_LIB_BUILD).so
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).so" "lib$(KIM_LIB).so"

lib$(KIM_LIB_BUILD).a: $(ALLOBJ) KIM_API_DIRS.o
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(ALLOBJ) KIM_API_DIRS.o

# Update LINKSOPATH, if prefix is non-standard
ifeq ($(filter $(standard_system_prefixes),$(prefix)),)
   LINKSOPATH = $(package_dir)/
endif

lib$(KIM_LIB_BUILD).so: lib$(KIM_LIB_BUILD).a
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(LINKSOPATH)lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) $@ \
                      $(LDWHOLEARCHIVESTARTFLAG) $< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(LDLIBS)

KIM_API_Version.h: KIM_API_Version.h.tpl $(KIM_MAKE_FILES)
	@printf "Creating... KIM_API_Version.h.\n"
	$(QUELL)cat KIM_API_Version.h.tpl | sed -e 's/versionMajor/$(VERSION_MAJOR)/g' \
                                                -e 's/versionMinor/$(VERSION_MINOR)/g' \
                                                -e 's/versionPatch/$(VERSION_PATCH)/g' \
                                                -e 's/versionPrerelease/"$(VERSION_PRERELEASE)"/g' \
                                                -e 's/versionBuildMetadata/"$(VERSION_BUILD_METADATA)"/g' \
                                                -e 's/versionString/"$(VERSION_FULL)"/g' \
                                           > $@

KIM_API.h: KIM_API_Version.h $(KIM_MAKE_FILES)

KIM_API_C.h: KIM_API_Version.h $(KIM_MAKE_FILES)

KIM_API_status.h: KIM_API_Version.h $(KIM_MAKE_FILES)

KIM_API.o: KIMDEFINES = -D KIM_LINK_VALUE=$(KIM_LINK_VALUE)                \
                        -D STANDARD_KIM_STR_NAME=standard_kim_str          \
                        -D STANDARD_KIM_STR_LEN_NAME=standard_kim_str_len  \
                        -D KIM_STR_NAME=\"kim_str\"                        \
                        -D MODELLIBFILE=\"lib$(MODEL_LIB)\"
KIM_API.o: KIM_API.h KIM_API_status.h KIM_API_DIRS.h KIM_API.cpp $(KIM_MAKE_FILES)

KIM_API_C.o: KIM_API.h KIM_API_C.h KIM_API_status.h KIM_API_C.cpp $(KIM_MAKE_FILES)

KIM_API_F03.o: KIM_API_status.h KIM_API_F03_Multiples.incl KIM_API_F03_Helper.o $(KIM_MAKE_FILES)

KIM_API_F03_Multiples.incl: scripts/CreateGetSetMultipleRoutines.sh $(KIM_MAKE_FILES)
	@printf "Creating... KIM_API_F03_Multiples.incl.\n"
	$(QUELL)scripts/CreateGetSetMultipleRoutines.sh > KIM_API_F03_Multiples.incl

KIM_API_F03_Helper.o: KIM_API_status.h $(KIM_MAKE_FILES)

KIM_AUX.o: KIM_AUX.h KIM_API.h KIM_API_status.h $(KIM_MAKE_FILES)

KIM_API_DIRS.o: KIM_API_DIRS.h $(KIM_MAKE_FILES)

KIM_API_DIRS.o: KIMDEFINES = -D PACKAGEDIR=\"$(package_dir)\" \
                             -D KIMDIR=\"$(KIM_DIR)\" \
                             -D KIMLIBBUILD=\"$(KIM_LIB_BUILD).so\" \
                             -D MODELDRIVERSDIR=\"$(modeldriversdir)\" \
                             -D MODELSDIR=\"$(modelsdir)\" \
                             -D PACKAGENAME=\"$(package_name)\" \
                             -D USERROOT=$(if $(filter-out $(HOME),$(user_config_file_root_name)),1,0) \
                             -D USERCONFIGFILEROOTNAME=\"$(user_config_file_root_name)\" \
                             -D USERCONFIGFILEDIRNAME=\"$(user_config_file_dir_name)\" \
                             -D VERSION_MAJOR=\"$(VERSION_MAJOR)\" \
                             -D MODELLIBFILE=\"lib$(MODEL_LIB)\" \
                             -D MODELDRIVERLIBFILE=\"lib$(MODEL_DRIVER_LIB)\"

Unit_Handling.o: Unit_Handling.h KIM_API.h KIM_API_status.h $(KIM_MAKE_FILES)


# standard descriptor file make rule
.INTERMEDIATE: kim_str.c
kim_str.c: standard.kim $(KIM_MAKE_FILES)
	@printf "Creating... intermediate file... $@.\n"
	$(QUELL)varnm=`printf "standard.kim" | \
                       sed -e 's/^\([[:digit:]]\)/__\1/' -e 's/[^[:alnum:]]/_/g'` && \
                xxd -i standard.kim | \
                sed -e "s/$${varnm}/standard_kim_str/g" > $@

.PHONY: all objects libs clean install objects-install libs-install installdirs uninstall objects-uninstall install-check libs-uninstall

clean:
	@printf "Cleaning... src.\n"
	$(QUELL)rm -f *.o *.a *.so *.mod  \
                      KIM_API_Version.h KIM_API_F03_Multiples.incl \
                      kim_str.c


install_modelsdir = $(dest_package_dir)/$(modelsdir)
install_modeldriversdir = $(dest_package_dir)/$(modeldriversdir)
install_includedir = $(dest_package_dir)/include
install_nodest_includedir = $(package_dir)/include
install_include = KIM_API.h KIM_API_C.h KIM_API_status.h KIM_API_Version.h KIM_API_DIRS.h Unit_Handling.h KIM_AUX.h kim_api_f03.mod

install-check:
	$(QUELL)if test -d "$(dest_package_dir)"; then \
                  rm -rf "$(install_includedir)"; \
                  rm -f  "$(DESTDIR)$(includedir)/$(full_package_name)"; \
                  rm -f  "$(dest_package_dir)/lib*.so"; \
                  rm -f  "$(DESTDIR)$(libdir)/lib$(full_package_name).so"; \
                fi

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(includedir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_includedir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modelsdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modeldriversdir)"

install: objects-install libs-install

objects-install: install-check objects
	@true

libs-install: libs installdirs
	@printf "Installing...($(dest_package_dir))................................. src"
        # Install include files
	$(QUELL)for fl in $(install_include); do $(INSTALL_PROGRAM) -m 0644 "$$fl" "$(install_includedir)/$$fl"; done
	$(QUELL)ln -fs "$(install_nodest_includedir)" "$(DESTDIR)$(includedir)/$(full_package_name)"
        # Install lib files
	$(QUELL)$(INSTALL_PROGRAM) -m 0755 "lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB_BUILD).so"
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB).so"
	$(QUELL)ln -fs "$(full_package_name)/lib$(KIM_LIB).so" "$(DESTDIR)$(libdir)/lib$(KIM_LIB).so"
	@printf ".\n"

uninstall: objects-uninstall libs-uninstall

objects-uninstall:
	@true

libs-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. src.\n"
	$(QUELL)if test -d "$(dest_package_dir)"; then rm -rf "$(dest_package_dir)"; fi
        # Uninstall include files
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(full_package_name)" && if test -L "$$fl"; then rm -f "$$fl"; fi
        # Uninstall lib files
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi

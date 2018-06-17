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

ifeq ($(wildcard Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Please create this file in order to compile the KIM API package)
endif
include Makefile.KIM_Config

#
# List of available targets
#
.PHONY: help

help:
	@printf "TARGETS FOR HELP\n"
	@printf "'help'                       -- print this list of targets\n"
	@printf "\n"
	@printf "TARGETS FOR BUILDING AND CLEANING THE KIM API PACKAGE\n"
	@printf "'all'                        -- build the KIM API library and all 'added'\n"
	@printf "                                Model Drivers and Models; same as 'make'\n"
	@printf "'clean'                      -- delete appropriate .o, .mod, .a, .so and\n"
	@printf "                                executable files from src/ directory and\n"
	@printf "                                its subdirectories\n"
	@printf "\n"
	@printf "TARGETS FOR INSTALLING THE KIM API PACKAGE\n"
	@printf "'install'                    -- install KIM API library, associated\n"
	@printf "                                executable utilities, and 'added' Model\n"
	@printf "                                Drivers and Models to system-wide location.\n"
	@printf "\n"
	@printf "TARGETS FOR UNINSTALLING THE KIM API PACKAGE\n"
	@printf "'uninstall'                  -- delete files installed by 'make install'\n"
	@printf "\n"


#
# Main build settings and rules
#
.PHONY: all kim-api-fortran-all kim-api-c-all kim-api-cpp-all utils-all completion-all

all: kim-api-fortran-includes-all kim-api-c-includes-all kim-api-cpp-includes-all kim-api-fortran-all kim-api-c-all kim-api-cpp-all utils-all completion-all

# Add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += $(KIM_DIR)/Makefile


# build targets involved in "make all"

kim-api-fortran-includes-all: kim-api-fortran-includes-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/include all

kim-api-c-includes-all: kim-api-c-includes-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/include all

kim-api-cpp-includes-all: kim-api-cpp-includes-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/include all

kim-api-fortran-all: kim-api-fortran-includes-all kim-api-fortran-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/src all

kim-api-c-all: kim-api-c-includes-all kim-api-c-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/src all

kim-api-cpp-all: kim-api-cpp-includes-all kim-api-cpp-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src all

utils-all: kim-api-cpp-all cpp/src/utils-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src/utils all

completion-all: completion-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C completion all


#
# Main clean rules and targets
#
.PHONY: clean kim-api-cpp-includes-clean kim-api-fortran-clean kim-api-c-clean kim-api-cpp-clean utils-clean completion-clean

clean: kim-api-fortran-includes-clean kim-api-c-includes-clean kim-api-cpp-includes-clean kim-api-fortran-clean kim-api-c-clean kim-api-cpp-clean utils-clean completion-clean docs-clean

# build targets involved in "make clean"
kim-api-fortran-includes-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/include clean
kim-api-c-includes-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/include clean
kim-api-cpp-includes-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/include clean
kim-api-fortran-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/src clean
kim-api-c-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/src clean
kim-api-cpp-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src clean

utils-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src/utils clean

completion-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C completion clean

docs-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C docs clean

#
# Main install settings and rules
#
.PHONY: install dirs-install config-install build-system-install kim-api-fortran-install kim-api-c-install kim-api-cpp-install utils-install completion-install

install: dirs-install config-install build-system-install kim-api-fortran-install kim-api-c-install kim-api-cpp-install utils-install completion-install

dirs-install:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(includedir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(libdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(bindir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(libexecdir)"

config-install: dirs-install
	@printf "Installing...($(dest_package_dir))................................. config.\n"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(dest_package_dir)"
        # Install KIM_Config file
	$(QUELL)fl="Makefile.KIM_Config" && \
                sed -e 's|^[[:space:]]*KIM_DIR[[:space:]]*:*=.*$$|KIM_DIR = $(package_dir)|' \
                    -e 's|^[[:space:]]*prefix[[:space:]]*:*=.*$$|prefix = $(prefix)|' \
                $$fl > "$(dest_package_dir)/$$fl" && \
                chmod 0644 "$(dest_package_dir)/$$fl"
        # Install version file
ifeq (true,$(shell git rev-parse --is-inside-work-tree 2> /dev/null))
	$(QUELL)sed -e 's|^VERSION_BUILD_METADATA.*$$|VERSION_BUILD_METADATA = $(VERSION_BUILD_METADATA)|' Makefile.Version > "$(dest_package_dir)/Makefile.Version" && \
                chmod 0644 "$(dest_package_dir)/Makefile.Version"
else
	$(QUELL)$(INSTALL_PROGRAM) -m 0644 Makefile.Version "$(dest_package_dir)/Makefile.Version"
endif
        # create links
	$(QUELL)if test -e "$(DESTDIR)$(includedir)/$(full_package_name)"; then \
                  rm -rf "$(DESTDIR)$(includedir)/$(full_package_name)"; fi && \
                ln -s "$(package_dir)/include" "$(DESTDIR)$(includedir)/$(full_package_name)"

build-system-install: dirs-install
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C build_system install

kim-api-fortran-install: dirs-install kim-api-fortran-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/src install

kim-api-c-install: dirs-install kim-api-c-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/src install

kim-api-cpp-install: dirs-install kim-api-cpp-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src install

utils-install: dirs-install utils-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src/utils install

completion-install: dirs-install completion-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C completion install

#
# Main uninstall settings and rules
#
.PHONY: uninstall completion-uninstall utils-uninstall kim-api-cpp-uninstall kim-api-c-uninstall kim-api-fortran-uninstall build-system-uninstall config-uninstall dirs-uninstall

uninstall: completion-uninstall utils-uninstall kim-api-cpp-uninstall kim-api-c-uninstall kim-api-fortran-uninstall build-system-uninstall config-uninstall dirs-uninstall

# targets involved in "make uninstall"
completion-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C completion uninstall

utils-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src/utils uninstall

kim-api-cpp-uninstall: utils-uninstall
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C cpp/src uninstall

kim-api-c-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C c/src uninstall

kim-api-fortran-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C fortran/src uninstall

build-system-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C build_system uninstall

config-uninstall: completion-uninstall utils-uninstall kim-api-cpp-uninstall kim-api-c-uninstall kim-api-fortran-uninstall build-system-uninstall
	@printf "Uninstalling...($(dest_package_dir))................................. config.\n"
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(full_package_name)" && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(dest_package_dir)/Makefile.KIM_Config" && if test -f "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(dest_package_dir)/Makefile.Version" && if test -f "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)if test -d "$(dest_package_dir)"; then rmdir "$(dest_package_dir)" > /dev/null 2>&1 || true; fi

dirs-uninstall: config-uninstall
	$(QUELL)if test -d "$(DESTDIR)$(libexecdir)"; then rmdir "$(DESTDIR)$(libexecdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(bindir)"; then rmdir "$(DESTDIR)$(bindir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(libdir)"; then rmdir "$(DESTDIR)$(libdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(includedir)"; then rmdir "$(DESTDIR)$(includedir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(prefix)"; then rmdir "$(DESTDIR)$(prefix)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)"; then rmdir "$(DESTDIR)" > /dev/null 2>&1 || true; fi


########### for internal use ###########
%-making-echo:
	@printf "\n%79s\n" " " | sed -e 's/ /*/g'
	@printf "%-77s%2s\n" "** Building... `printf "$(patsubst %-all,%,$*)" | sed -e 's/@/ /g'`" "**"
	@printf "%79s\n" " " | sed -e 's/ /*/g'

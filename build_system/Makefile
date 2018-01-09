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

ifeq ($(wildcard ../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Please create this file in order to compile the KIM API package)
endif
include ../Makefile.KIM_Config

#
# Main build settings and rules
#
.PHONY: all

all:

#
# Main install settings and rules
#
.PHONY: install dirs-install build-system-install

install_builddir = $(dest_package_dir)/$(builddir)
install_make = Makefile.Generic \
               Makefile.LoadDefaults \
               Makefile.ModelDriver \
               Makefile.ParameterizedModel \
               Makefile.SanityCheck \
               Makefile.SimulatorModel \
               Makefile.StandAloneModel
install_compilerdir = $(dest_package_dir)/$(buildcompilerdir)
install_compiler = Makefile.GCC \
                   Makefile.INTEL
install_linkerdir = $(dest_package_dir)/$(buildlinkerdir)
install_linker = Makefile.DARWIN \
                 Makefile.FREEBSD \
                 Makefile.LINUX

install: dirs-install build-system-install

dirs-install:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_builddir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_compilerdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_linkerdir)"

build-system-install:
	$(QUELL)for fl in $(install_make); do $(INSTALL_PROGRAM) -m 0644 "$(KIM_DIR)/$(builddir)/$$fl" "$(install_builddir)/$$fl"; done
	$(QUELL)for fl in $(install_compiler); do $(INSTALL_PROGRAM) -m 0644 "$(KIM_DIR)/$(buildcompilerdir)/$$fl" "$(install_compilerdir)/$$fl"; done
	$(QUELL)for fl in $(install_linker); do $(INSTALL_PROGRAM) -m 0644 "$(KIM_DIR)/$(buildlinkerdir)/$$fl" "$(install_linkerdir)/$$fl"; done


#
# Main uninstall settings and rules
#
.PHONY: uninstall build-system-uninstall dirs-uninstall

uninstall: build-system-uninstall dirs-uninstall

build-system-uninstall:
	$(QUELL)for fl in $(install_linker); do if test -f "$(install_linkerdir)/$$fl"; then rm -f "$(install_linkerdir)/$$fl"; fi; done
	$(QUELL)for fl in $(install_compiler); do if test -f "$(install_compilerdir)/$$fl"; then rm -f "$(install_compilerdir)/$$fl"; fi; done
	$(QUELL)for fl in $(install_make); do if test -f "$(install_builddir)/$$fl"; then rm -f "$(install_builddir)/$$fl"; fi; done

dirs-uninstall:
	$(QUELL)if test -d "$(install_linkerdir)"; then rmdir "$(install_linkerdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(install_compilerdir)"; then rmdir "$(install_compilerdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(install_builddir)"; then rmdir "$(install_builddir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(dest_package_dir)"; then rmdir "$(dest_package_dir)" > /dev/null 2>&1 || true; fi

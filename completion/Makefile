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
# Copyright (c) 2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard ../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../Makefile.KIM_Config


all: $(full_package_name)-completion.bash


# Add the local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

$(full_package_name)-completion.bash: completion.bash.sh $(KIM_MAKE_FILES)
	@printf "Creating... $@.\n"
	$(QUELL)sed -e 's|###SANITIZED#FULL#PACKAGE#NAME###|$(subst -,_,$(full_package_name))|g' \
                    -e 's|###FULL#PACKAGE#NAME###|$(full_package_name)|g' \
                    -e 's|###COLLECTIONS#INFO#UTILITY###|$(package_dir)/libexec/$(full_package_name)/$(full_package_name)-collections-info|g' \
                    -e 's|###MAJOR#VERSION###|$(VERSION_MAJOR)|g' \
                completion.bash.sh > $@


.PHONY: all clean install installdirs uninstall

clean:
	@printf "Cleaning... completion.\n"
	$(QUELL)rm -f $(full_package_name)-completion.bash


install_sysconfdir = $(DESTDIR)$(sysconfdir)
install_bashdir = bash_completion.d
install_bash = $(full_package_name)-completion.bash

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(sysconfdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(sysconfdir)/$(install_bashdir)"

install: all installdirs
	@printf "Installing...($(DESTDIR)$(sysconfdir))............................................ sysconfdir.\n"
        # Install bin files
	$(QUELL)for fl in $(install_bash); do $(INSTALL_PROGRAM) -m 0644 "$$fl" "$(DESTDIR)$(sysconfdir)/$(install_bashdir)/$$fl"; done

uninstall:
	@printf "Uninstalling...($(DESTDIR)$(sysconfdir))............................................ sysconfdir.\n"
        # Uninstall bin files
	$(QUELL)for fl in $(install_bash); do if test -f "$(DESTDIR)$(sysconfdir)/$(install_bashdir)/$$fl"; then rm -f "$(DESTDIR)$(sysconfdir)/$(install_bashdir)/$$fl"; fi; done
	$(QUELL) rmdir "$(DESTDIR)$(sysconfdir)/$(install_bashdir)" > /dev/null 2>&1 || true
	$(QUELL) rmdir "$(DESTDIR)$(sysconfdir)" > /dev/null 2>&1 || true

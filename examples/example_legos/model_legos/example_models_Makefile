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

export MODELS_LIST := $(wildcard ex_model*)

all: $(patsubst %,%-all,$(MODELS_LIST))

install-system-all: $(patsubst %,%-install-system,$(MODELS_LIST))
install-user-all: $(patsubst %,%-install-user,$(MODELS_LIST))

uninstall-system-all: $(patsubst %,%-uninstall-system,$(MODELS_LIST))
uninstall-user-all: $(patsubst %,%-uninstall-user,$(MODELS_LIST))

clean: $(patsubst %,%-clean,$(MODELS_LIST))

.PHONY: all clean $(patsubst %,%-all,$(MODELS_LIST))              $(patsubst %,%-clean,$(MODELS_LIST))          \
                  $(patsubst %,%-install-system,$(MODELS_LIST))   $(patsubst %,%-install-user,$(MODELS_LIST))   \
                  $(patsubst %,%-uninstall-system,$(MODELS_LIST)) $(patsubst %,%-uninstall-user,$(MODELS_LIST))

########### for internal use ###########
%-making-echo:
	@printf "\n%79s\n" " " | sed -e 's/ /*/g'
	@printf "%-77s%2s\n" "** Building... `printf "$(patsubst %-all,%,$*)" | sed -e 's/@/ /g'`" "**"
	@printf "%79s\n" " " | sed -e 's/ /*/g'

$(patsubst %,%-all,$(MODELS_LIST)): %: Makefile Model..........@%-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-all,%,$@) all

$(patsubst %,%-clean,$(MODELS_LIST)):
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-clean,%,$@) clean

$(patsubst %,%-install-system,$(MODELS_LIST)): %: Makefile Model..........@%-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-install-system,%,$@) install-system

$(patsubst %,%-install-user,$(MODELS_LIST)): %: Makefile Model..........@%-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-install-user,%,$@) install-user

$(patsubst %,%-uninstall-system,$(MODELS_LIST)): %: Makefile Model..........@%-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-uninstall-system,%,$@) uninstall-system

$(patsubst %,%-uninstall-user,$(MODELS_LIST)): %: Makefile Model..........@%-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %-uninstall-user,%,$@) uninstall-user

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


#
# Main build settings and rules
#
.PHONY: all-system all-user model-drivers-all-system model-drivers-all-user models-all-system models-all-user openkim-tests-all

all-system: model-drivers-all-system models-all-system openkim-tests-all
all-user: model-drivers-all-user models-all-user openkim-tests-all

model-drivers-all-system: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) install-system-all
model-drivers-all-user: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) install-user-all

models-all-system: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) install-system-all
models-all-user: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) install-user-all

openkim-tests-all: Example\ OpenKIM\ Tests-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C openkim_tests all


#
# Main clean settings and rules
#
.PHONY: clean model-drivers-clean models-clean openkim-tests-clean

clean: model-drivers-clean models-clean openkim-tests-clean

model-drivers-clean: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) uninstall-system-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) uninstall-user-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) clean

models-clean: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) uninstall-system-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) uninstall-user-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) clean

openkim-tests-clean: Example\ OpenKIM\ Tests-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C openkim_tests clean


########### for internal use ###########
%-making-echo:
	@printf "\n%79s\n" " " | sed -e 's/ /*/g'
	@printf "%-77s%2s\n" "** Working on... `printf "$(patsubst %-all,%,$*)" | sed -e 's/@/ /g'`" "**"
	@printf "%79s\n" " " | sed -e 's/ /*/g'

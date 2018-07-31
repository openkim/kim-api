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

ifneq (clean,$(MAKECMDGOALS))
  KIM_API_BUILD_CONFIG = kim-api-v2-build-config
  ifeq ($(shell $(KIM_API_BUILD_CONFIG) --version 2> /dev/null),)
    $(error $(KIM_API_BUILD_CONFIG) utility is not available.  Something is wrong with your KIM API package setup)
  endif
endif
include $(shell $(KIM_API_BUILD_CONFIG) --master-config)


#
# Main build settings and rules
#
.PHONY: all-system all-user model-drivers-all-system model-drivers-all-user models-all-system models-all-user simulators-all

all-system: model-drivers-all-system models-all-system simulators-all
all-user: model-drivers-all-user models-all-user simulators-all

model-drivers-all-system: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) install-system-all
model-drivers-all-user: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) install-user-all

models-all-system: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) install-system-all
models-all-user: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) install-user-all

simulators-all: Example\ Simulators-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C simulators KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) all


#
# Main clean settings and rules
#
.PHONY: clean model-drivers-clean models-clean simulators-clean

clean: model-drivers-clean models-clean simulators-clean

model-drivers-clean: Example\ Model\ Drivers-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) uninstall-system-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) uninstall-user-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modeldriversdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) clean

models-clean: Example\ Model-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) uninstall-system-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) uninstall-user-all
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(modelsdir) KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) clean

simulators-clean: Example\ Simulators-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C simulators KIM_API_BUILD_CONFIG=$(KIM_API_BUILD_CONFIG) clean


########### for internal use ###########
%-making-echo:
	@printf "\n%79s\n" " " | sed -e 's/ /*/g'
	@printf "%-77s%2s\n" "** Working on... `printf "$(patsubst %-all,%,$*)" | sed -e 's/@/ /g'`" "**"
	@printf "%79s\n" " " | sed -e 's/ /*/g'

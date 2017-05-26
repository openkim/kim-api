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
	@printf "TARGETS FOR MANIPULATING THE SYSTEM-COLLECTION AT BUILD TIME\n"
	@printf "'ls-all'                     -- list all 'added' Model Drivers and Models\n"
	@printf "'ls-model-drivers'           -- list all 'added' Model Drivers\n"
	@printf "'ls-models'                  -- list all 'added' Models\n"
	@printf "\n"
	@printf "'add-OpenKIM'                -- 'add' all OpenKIM Model Drivers and Models\n"
	@printf "'add-<Extended KIM ID>'      -- 'add' Model <Extended KIM ID> from OpenKIM\n"
	@printf "                                including the associated Model Driver, if\n"
	@printf "                                appropriate\n"
	@printf "'add-examples'               -- 'add' example Model Drivers and Models\n"
	@printf "\n"
	@printf "'rm-all'                     -- 'rm' all 'added' Model Drivers and Models\n"
	@printf "'rm-all-model-drivers'       -- 'rm' all 'added' Model Drivers\n"
	@printf "'rm-all-models'              -- 'rm' all 'added' Models\n"
	@printf "'rm-<Extended KIM ID>'       -- 'rm' the 'added' <Extended KIM ID> item\n"
	@printf "'rm-examples'                -- 'rm' the 'added' example Drivers and Models\n"
	@printf "\n"
	@printf "TARGETS FOR INSTALLING THE KIM API PACKAGE\n"
	@printf "'install'                    -- install KIM API library, associated\n"
	@printf "                                executable utilities, and 'added' Model\n"
	@printf "                                Drivers and Models to system-wide location\n"
	@printf "                                as described in item 7 below.\n"
	@printf "'install-set-default-to-vX'  -- create generic\n"
	@printf "                                $(includedir)/$(package_name) and\n"
	@printf "                                ${libdir}/${package_name} symlinks to the\n"
	@printf "                                corresponding $(package_name)-vX versions.\n"
	@printf "                                This effectively sets the 'default'\n"
	@printf "                                library available for users on the system.\n"
	@printf "\n"
	@printf "TARGETS FOR UNINSTALLING THE KIM API PACKAGE\n"
	@printf "'uninstall'                  -- delete files installed by 'make install'\n"
	@printf "'uninstall-set-default'      -- remove the generic\n"
	@printf "                                $(includedir)/$(package_name) and\n"
	@printf "                                $(libdir)/$(package_name) symlinks.\n"
	@printf "\n"
	@printf "\n"
	@printf "TARGETS FOR BUILDING AND CLEANING THE EXAMPLES\n"
	@printf "'examples'                   -- build all provided examples\n"
	@printf "'examples-all'               -- same as 'make examples'\n"
	@printf "'examples-clean'             -- delete appropriate .o, .mod, .a, .so and\n"
	@printf "                                executable files from examples/ directory\n"
	@printf "                                and its subdirectories\n"
	@printf "\n"


#
# List of "installed" model drivers and models
#
export MODEL_DRIVERS_LIST := $(filter-out $(if $(wildcard $(srcdir)/$(modeldriversdir)/.kimignore),$(shell cat $(srcdir)/$(modeldriversdir)/.kimignore),),$(patsubst $(srcdir)/$(modeldriversdir)/%/,%,$(filter-out $(srcdir)/$(modeldriversdir)/,$(sort $(dir $(wildcard $(srcdir)/$(modeldriversdir)/*/))))))
export MODELS_LIST        := $(filter-out $(if $(wildcard $(srcdir)/$(modelsdir)/.kimignore),$(shell cat $(srcdir)/$(modelsdir)/.kimignore),),$(patsubst $(srcdir)/$(modelsdir)/%/,%,$(filter-out $(srcdir)/$(modelsdir)/,$(sort $(dir $(wildcard $(srcdir)/$(modelsdir)/*/))))))


#
# Main build settings and rules
#
.PHONY: all models_check config \
        $(patsubst %,%-all,$(MODEL_DRIVERS_LIST) $(MODELS_LIST)) \
        utils-all kim-api-objects kim-api-libs

all: config kim-api-objects kim-api-libs utils-all $(patsubst %,%-all,$(MODEL_DRIVERS_LIST) $(MODELS_LIST))

# Add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

# build targets involved in "make all"

INPLACE_CONFIG = $(KIM_DIR)/$(user_config_file_dir_name)/config-v$(VERSION_MAJOR)

$(INPLACE_CONFIG): $(KIM_MAKE_FILES)
	@printf "Creating... User Config file.... $@.\n"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 $(KIM_DIR)/$(user_config_file_dir_name)
	$(QUELL)printf "model_drivers_dir = %s\n" $(KIM_DIR)/$(examplesdir)/$(modeldriversdir) >  $@; \
                printf "models_dir = %s\n" $(KIM_DIR)/$(examplesdir)/$(modelsdir)              >> $@

KIM_CONFIG_FILES = $(srcdir)/Makefile.KIM_Config \
                   $(srcdir)/utils/Makefile.KIM_Config \
                   $(srcdir)/$(modeldriversdir)/Makefile.KIM_Config \
                   $(srcdir)/$(modelsdir)/Makefile.KIM_Config \
                   $(KIM_DIR)/$(examplesdir)/Makefile.KIM_Config \
                   $(KIM_DIR)/$(examplesdir)/$(modelsdir)/Makefile.KIM_Config \
                   $(KIM_DIR)/$(examplesdir)/$(modeldriversdir)/Makefile.KIM_Config

KIM_SIMULATOR_CONFIG_FILES = $(KIM_DIR)/$(examplesdir)/openkim_tests/Makefile.KIM_Config_Helper \
                             $(KIM_DIR)/$(examplesdir)/simulators/Makefile.KIM_Config_Helper

config: $(KIM_CONFIG_FILES) $(INPLACE_CONFIG) $(KIM_SIMULATOR_CONFIG_FILES)

$(KIM_CONFIG_FILES): $(KIM_MAKE_FILES)
	$(QUELL)if test -d $(dir $@); then \
                  printf 'Creating... KIM_Config file..... $(patsubst $(KIM_DIR)/%,%,$@).\n'; \
                  printf '# This file is automatically generated by the KIM API build system.\n' >  $@; \
                  printf '# Do not edit!\n'                                                      >> $@; \
                  printf '\n'                                                                    >> $@; \
                  printf 'include $(KIM_DIR)/Makefile.KIM_Config\n'                              >> $@; \
                fi

$(KIM_SIMULATOR_CONFIG_FILES): $(KIM_MAKE_FILES)
	$(QUELL)if test -d $(dir $@); then \
                  printf 'Creating... KIM_Config_Helper file..... $(patsubst $(KIM_DIR)/%,%,$@).\n'; \
                  printf '# This file is automatically generated by the KIM API build system.\n'        >  $@; \
                  printf '# Do not edit!\n'                                                             >> $@; \
                  printf '\n'                                                                           >> $@; \
                  printf 'KIM_CONFIG_HELPER = $(KIM_DIR)/src/utils/$(full_package_name)-build-config\n' >> $@; \
                  printf '$$(shell find . -perm +u+x -and -type f): $(KIM_DIR)/src/lib$(KIM_LIB_BUILD).so\n' >> $@; \
                fi

kim-api-objects: $(KIM_MAKE_FILES) kim-api-objects-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) objects

kim-api-libs: $(KIM_MAKE_FILES) kim-api-libs-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) libs

utils-all: $(KIM_MAKE_FILES) src/utils-making-echo
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/utils all

$(patsubst %,%-all,$(MODEL_DRIVERS_LIST)): %: $(KIM_MAKE_FILES) Model@Driver...@%-making-echo | kim-api-objects
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modeldriversdir)/$(patsubst %-all,%,$@) all

$(patsubst %,%-all,$(MODELS_LIST)): %: $(KIM_MAKE_FILES) Model..........@%-making-echo | kim-api-objects
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modelsdir)/$(patsubst %-all,%,$@) all


#
# Main clean rules and targets
#
.PHONY: clean kim-api-clean config-clean utils-clean \
        $(patsubst %,%-clean,$(MODEL_DRIVERS_LIST) $(MODELS_LIST))

clean: config $(patsubst %,%-clean,$(MODEL_DRIVERS_LIST) $(MODELS_LIST)) kim-api-clean utils-clean config-clean

# build targets involved in "make clean"
$(patsubst %,%-clean,$(MODEL_DRIVERS_LIST)):
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modeldriversdir)/$(patsubst %-clean,%,$@) clean

$(patsubst %,%-clean,$(MODELS_LIST)):
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modelsdir)/$(patsubst %-clean,%,$@) clean

kim-api-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) clean
	$(QUELL)rm -f kim.log

utils-clean:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/utils clean

config-clean: inplace-config-clean
	@printf "Cleaning... KIM_Config files.\n"
	$(QUELL)rm -f $(KIM_CONFIG_FILES)
	$(QUELL)rm -f $(KIM_SIMULATOR_CONFIG_FILES)

inplace-config-clean:
	@printf "Cleaning... User config file....\n"
	$(QUELL)rm -rf $(KIM_DIR)/$(user_config_file_dir_name)


#
# Main ls-* settings and rules
#
.PHONY: ls-model-drivers ls-models ls-all

ls-model-drivers:
	$(QUELL)$(foreach mdr,$(notdir $(shell find "$(srcdir)/$(modeldriversdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          printf "*@existing.....@%-50s@found@in@@@$(srcdir)/$(modeldriversdir)\n" $(mdr)@ | sed -e 's/ /./g' -e 's/@/ /g';)

ls-models:
	$(QUELL)$(foreach ml,$(notdir $(shell find "$(srcdir)/$(modelsdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          printf "*@existing.....@%-50s@found@in@@@$(srcdir)/$(modelsdir)\n" $(ml)@ | sed -e 's/ /./g' -e 's/@/ /g';)

ls-all: ls-model-drivers ls-models


#
# Main add-* settings and rules
#
.PHONY: add-examples add-OpenKIM

add-examples:
	$(QUELL)$(foreach exmpl,$(notdir $(shell find "$(KIM_DIR)/$(examplesdir)/$(modeldriversdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          if test -e "$(srcdir)/$(modeldriversdir)/$(exmpl)"; then \
          printf "*@existing.....@%-50s@no@copy@performed!\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; else \
          printf "*@adding.......@%-50s@copied@to@@$(srcdir)/$(modeldriversdir)\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
          cp -r "$(KIM_DIR)/$(examplesdir)/$(modeldriversdir)/$(exmpl)" "$(srcdir)/$(modeldriversdir)/"; fi;)
	$(QUELL)$(foreach exmpl,$(notdir $(shell find "$(KIM_DIR)/$(examplesdir)/$(modelsdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          if test -e "$(srcdir)/$(modelsdir)/$(exmpl)"; then \
          printf "*@existing.....@%-50s@no@copy@performed!\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; else \
          printf "*@adding.......@%-50s@copied@to@@$(srcdir)/$(modelsdir)\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
          cp -r "$(KIM_DIR)/$(examplesdir)/$(modelsdir)/$(exmpl)" "$(srcdir)/$(modelsdir)/"; fi;)

add-OpenKIM:
	$(QUELL)query='query={"type":"mo","kim-api-version":{"$$regex":"^1\\."}}' && \
                query="$${query}"'&fields={"kimcode":1, "kim-api-version":1}' && \
                query="$${query}"'&database=obj' && \
                list=`wget -q -O - --post-data="$${query}" https://query.openkim.org/api \
                      | \
                      sed -e 's/\[//g' -e 's/\]//g' \
                      -e 's/{"kim-api-version": "\([0-9.]*\)", "kimcode": "\([^"]*\)"},*/\1:\2/g'` && \
                for model in $${list}; do \
                  minor=`printf "$${model}" | sed -e 's/1\.\([^.:]*\).*/\1/'`; \
                  modname=`printf "$${model}" | sed -e 's/.*://'`; \
                  if test $${minor} -ge 6; then \
                    $(MAKE) $(MAKE_FLAGS) add-$${modname}; \
                  fi; \
                done

add-%: config
	$(QUELL)if test x"__MD_" = x`printf "$*" | sed 's/.*\(__MD_\).*/\1/'`; then \
                  if test -e "$(srcdir)/$(modeldriversdir)/$*"; then \
                    printf "*@existing.....@%-50s@no@download@performed!\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                  else \
                    printf "*@adding.......@%-50s@to@$(srcdir)/$(modeldriversdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                    (cd "$(srcdir)/$(modeldriversdir)" && \
                     if wget -q --content-disposition 'https://openkim.org/download/$*.tgz'; then \
                       tar zxvf "$*.tgz" 2>&1 | sed -e 's/^/                /' && \
                       rm -f "$*.tgz"; \
                       if test 0 -lt `grep -c MAKE_SYSTEM $*/Makefile`; then \
                         printf "*** WARNING *** $* appears to be written for an older, incompatible, version of the KIM API.\n"; \
                       fi; \
                     else \
                       printf "                Unable to download $* from https://openkim.org.  Check the KIM Item ID for errors.\n" && false; \
                     fi); \
                  fi; \
                elif test x"__MO_" = x`printf "$*" | sed 's/.*\(__MO_\).*/\1/'`; then \
                  if test -e "$(srcdir)/$(modelsdir)/$*"; then \
                    printf "*@existing.....@%-50s@no@download@performed!\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                  else \
                    printf "*@adding.......@%-50s@to@$(srcdir)/$(modelsdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                    (cd "$(srcdir)/$(modelsdir)" && \
                     if wget -q --content-disposition 'https://openkim.org/download/$*.tgz'; then \
                       tar zxvf "$*.tgz" 2>&1 | sed -e 's/^/                /' && \
                       rm -f "$*.tgz" && \
                       if test 0 -lt `grep -c MAKE_SYSTEM $*/Makefile`; then \
                         printf "*** WARNING *** $* appears to be written for an older, incompatible, version of the KIM API.\n"; \
                       elif test x"ParameterizedModel" = x"`$(MAKE) $(MAKE_FLAGS) -C \"$*\" kim-item-type`"; then \
                         dvr="`$(MAKE) $(MAKE_FLAGS) -C \"$*\" model-driver-name`"; \
                         $(MAKE) $(MAKE_FLAGS) -C "$(KIM_DIR)" add-$${dvr}; \
                       fi; \
                     else \
                       printf "                Unable to download $* from https://openkim.org.  Check the KIM Item ID for errors.\n" && false; \
                     fi); \
                  fi; \
                elif test \( x"ex_model_driver_" = x`printf "$*" | sed 's/^\(ex_model_driver_\).*/\1/'` -a -d "$(KIM_DIR)/$(examplesdir)/$(modeldriversdir)/$*" \); then \
                   if test -e "$(srcdir)/$(modeldriversdir)/$*"; then \
                     printf "*@existing.....@%-50s@no@copy@performed!\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; else \
                     printf "*@adding.......@%-50s@copied@to@@$(srcdir)/$(modeldriversdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                     cp -r "$(KIM_DIR)/$(examplesdir)/$(modeldriversdir)/$*" "$(srcdir)/$(modeldriversdir)/"; \
                  fi; \
                elif test \( x"ex_model_" = x`printf "$*" | sed 's/^\(ex_model_\).*/\1/'` -a -d "$(KIM_DIR)/$(examplesdir)/$(modelsdir)/$*" \); then \
                   if test -e "$(srcdir)/$(modelsdir)/$*"; then \
                     printf "*@existing.....@%-50s@no@copy@performed!\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; else \
                     printf "*@adding.......@%-50s@copied@to@@$(srcdir)/$(modelsdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                     cp -r "$(KIM_DIR)/$(examplesdir)/$(modelsdir)/$*" "$(srcdir)/$(modelsdir)/"; \
                     if test x"ParameterizedModel" = x"`$(MAKE) $(MAKE_FLAGS) -C \"$(srcdir)/$(modelsdir)/$*\" kim-item-type`"; then \
                       dvr="`$(MAKE) $(MAKE_FLAGS) -C \"$(srcdir)/$(modelsdir)/$*\" model-driver-name`"; \
                       $(MAKE) $(MAKE_FLAGS) -C "$(KIM_DIR)" add-$${dvr}; \
                     fi; \
                   fi; \
                else \
                  printf "Unknown OpenKIM Model Driver or Model item or example name: $*.\n" && \
                  printf "NOTE: The KIM API only supports download of Models (*__MO_*) and\n" && \
                  printf "      Model Drivers (*__MD_*) from openkim.org.  OpenKIM Tests are\n" && \
                  printf "      just special cases of Simulators with which the KIM API may be\n" && \
                  printf "      linked.  Thus, no special support for downloading Tests (*__TE_*)\n" && \
                  printf "      is provided by the KIM API package.\n" && \
                  false; \
                fi


#
# Main rm-* settings and rules
#
.PHONY: rm-examples rm-all rm-all-model-drivers rm-all-models

rm-examples:
	$(QUELL)$(foreach exmpl,$(notdir $(shell find "$(KIM_DIR)/$(examplesdir)/$(modeldriversdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          if test -e "$(srcdir)/$(modeldriversdir)/$(exmpl)"; then \
          printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modeldriversdir)\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
          rm -rf "$(srcdir)/$(modeldriversdir)/$(exmpl)"; fi;)
	$(QUELL)$(foreach exmpl,$(notdir $(shell find "$(KIM_DIR)/$(examplesdir)/$(modelsdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
          if test -e $(srcdir)/$(modelsdir)/$(exmpl); then \
          printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modelsdir)\n" $(exmpl)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
          rm -rf "$(srcdir)/$(modelsdir)/$(exmpl)"; fi;)

rm-all-model-drivers:
	$(QUELL)$(foreach mdr,$(notdir $(shell find "$(srcdir)/$(modeldriversdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
                printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modeldriversdir)\n" $(mdr)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                rm -rf "$(srcdir)/$(modeldriversdir)/$(mdr)";)

rm-all-models:
	$(QUELL)$(foreach ml,$(notdir $(shell find "$(srcdir)/$(modelsdir)" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)),\
                printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modelsdir)\n" $(ml)@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                rm -rf "$(srcdir)/$(modelsdir)/$(ml)";)

rm-all: rm-all-model-drivers rm-all-models

rm-%:
	$(QUELL)if test \( -d "$(srcdir)/$(modeldriversdir)/$*" \); then \
                  printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modeldriversdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                  (cd "$(srcdir)/$(modeldriversdir)" && rm -rf "$*"); \
                elif test \( -d "$(srcdir)/$(modelsdir)/$*" \); then \
                  printf "*@removing.....@%-50s@rm'ed@from@$(srcdir)/$(modelsdir)\n" $*@ | sed -e 's/ /./g' -e 's/@/ /g'; \
                  (cd "$(srcdir)/$(modelsdir)" && rm -rf "$*"); \
                else \
                  printf "Item name, $*, not found.  Nothing removed.\n"; \
                fi


#
# Main install settings and rules
#
.PHONY: install install-check installdirs kim-api-objects-install kim-api-libs-install config-install utils-install\
        $(patsubst %,%-install,$(MODEL_DRIVERS_LIST) $(MODELS_LIST))

install: install-check config inplace-config-clean kim-api-objects-install kim-api-libs-install utils-install $(patsubst %,%-install,$(MODEL_DRIVERS_LIST) $(MODELS_LIST)) config-install


# build targets involved in "make install"
install_builddir = $(dest_package_dir)/$(builddir)
install_make = Makefile.LoadDefaults Makefile.Model Makefile.ModelDriver Makefile.ParameterizedModel Makefile.SanityCheck parameterized_model.cpp
install_compilerdir = $(dest_package_dir)/$(buildcompilerdir)
install_compiler = Makefile.GCC Makefile.INTEL
install_linkerdir = $(dest_package_dir)/$(buildlinkerdir)
install_linker = Makefile.DARWIN Makefile.FREEBSD Makefile.LINUX

install-check:
        # should we check that the installed stuff is actually dynamic-load and the right settings (32bit, etc.)?
	$(QUELL)if test -d "$(dest_package_dir)"; then \
                  rm -rf "$(install_linkerdir)"; \
                  rm -rf "$(install_compilerdir)"; \
                  rm -rf "$(install_builddir)"; \
                  rm -f  "$(dest_package_dir)/Makefile.KIM_Config"; \
                  rm -f  "$(dest_package_dir)/Makefile.Version"; \
                fi

kim-api-objects-install:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) objects-install

kim-api-libs-install:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) libs-install

utils-install:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/utils install

$(patsubst %,%-install,$(MODEL_DRIVERS_LIST)):
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modeldriversdir)/$(patsubst %-install,%,$@) install

$(patsubst %,%-install,$(MODELS_LIST)):
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/$(modelsdir)/$(patsubst %-install,%,$@) install

config-install: installdirs
	@printf "Installing...($(dest_package_dir))................................. KIM_Config files"
        # Install make directory
	$(QUELL)for fl in $(install_make); do $(INSTALL_PROGRAM) -m 0644 "$(builddir)/$$fl" "$(install_builddir)/$$fl"; done
	$(QUELL)fl="Makefile.Generic" && \
                sed -e 's|^[[:space:]]*srcdir[[:space:]]*:*=.*$$|srcdir = $$(KIM_DIR)|' \
                    -e 's|^[[:space:]]*KIMINCLUDEFLAGS[[:space:]]*:*=.*$$|KIMINCLUDEFLAGS = -I$$(KIM_DIR)/include|' $(builddir)/$$fl \
                > "$(install_builddir)/$$fl" && \
                chmod 0644 "$(install_builddir)/$$fl"
        # Install compiler defaults directory
	$(QUELL)for fl in $(install_compiler); do $(INSTALL_PROGRAM) -m 0644 "$(buildcompilerdir)/$$fl" "$(install_compilerdir)/$$fl"; done
        # Install linker defaults directory
	$(QUELL)for fl in $(install_linker); do $(INSTALL_PROGRAM) -m 0644 "$(buildlinkerdir)/$$fl" "$(install_linkerdir)/$$fl"; done
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
	@printf ".\n"

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_builddir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_compilerdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_linkerdir)"

# targets for setting default system-wide library
install-set-default-to-v%: EXT:=so
install-set-default-to-v%:
	@printf "Setting default $(package_name) to $(package_name)-v$*\n"
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-descriptor-file-match" && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "$(package_name)-v$*-descriptor-file-match" "$$fl"
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-build-config"          && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "$(package_name)-v$*-build-config" "$$fl"
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-collections-info"      && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "$(package_name)-v$*-collections-info" "$$fl"
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(package_name)"       && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "$(package_name)-v$*" "$$fl"
	$(QUELL)fl="$(DESTDIR)$(libdir)/$(package_name)"           && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "$(package_name)-v$*" "$$fl"
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(package_name).$(EXT)" && if test -L "$$fl"; then rm -f "$$fl"; fi && ln -fs "lib$(package_name)-v$*.$(EXT)" "$$fl"


#
# Main uninstall settings and rules
#
.PHONY: uninstall kim-api-objects-uninstall kim-api-libs-uninstall utils-uninstall config-uninstall

uninstall: config kim-api-objects-uninstall utils-uninstall kim-api-libs-uninstall config-uninstall

# targets involved in "make uninstall"
kim-api-objects-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) objects-uninstall

utils-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir)/utils uninstall

kim-api-libs-uninstall:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(srcdir) libs-uninstall

config-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. KIM_Config files.\n"
        # Make sure the package directory is gone
	$(QUELL)if test -d "$(dest_package_dir)"; then rm -rf "$(dest_package_dir)"; fi
        # Uninstall the rest
	$(QUELL)if test -d "$(DESTDIR)$(includedir)"; then rmdir "$(DESTDIR)$(includedir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(bindir)"; then rmdir "$(DESTDIR)$(bindir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(libdir)"; then rmdir "$(DESTDIR)$(libdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(exec_prefix)"; then rmdir "$(DESTDIR)$(exec_prefix)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(prefix)"; then rmdir "$(DESTDIR)$(prefix)" > /dev/null 2>&1 || true; fi

# targets for unsetting default system-wide library
uninstall-set-default: EXT:=so
uninstall-set-default:
	@printf "Removing default $(package_name) settings.\n"
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-descriptor-file-match" && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-build-config"          && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(DESTDIR)$(bindir)/$(package_name)-collections-info"      && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(package_name)"       && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(DESTDIR)$(libdir)/$(package_name)"           && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(package_name).$(EXT)" && if test -L "$$fl"; then rm -f "$$fl"; fi


#
# Examples build settings and rules
#
.PHONY: examples examples-all examples-clean examples-clean-all

examples: all examples-all

examples-all:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(KIM_DIR)/$(examplesdir) all

examples-clean: config examples-clean-all config-clean

examples-clean-all:
	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(KIM_DIR)/$(examplesdir) clean


########### for internal use ###########
%-making-echo:
	@printf "\n%79s\n" " " | sed -e 's/ /*/g'
	@printf "%-77s%2s\n" "** Building... `printf "$(patsubst %-all,%,$*)" | sed -e 's/@/ /g'`" "**"
	@printf "%79s\n" " " | sed -e 's/ /*/g'

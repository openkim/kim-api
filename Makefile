
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#
# 
# this make file builds/cleans everything 
#
#

include $(KIM_DIR)KIM_API/Include.mk

MODELS_LIST = $(notdir $(shell find $(KIM_MODELS_DIR) -maxdepth 2 -mindepth 2 -printf "%P\n" | grep -E "^([^/]*)/\1\.$(CODE_EXTENSIONS)$$" | sed -e "s/\([^/]*\)\/.*/\1/"))
TESTS_LIST  = $(notdir $(shell find $(KIM_TESTS_DIR)  -maxdepth 2 -mindepth 2 -printf "%P\n" | grep -E "^([^/]*)/\1\.$(CODE_EXTENSIONS)$$" | sed -e "s/\([^/]*\)\/.*/\1/"))

.PHONY: all therest clean pristine $(patsubst %,%-all,$(MODELS_LIST) $(TESTS_LIST)) $(patsubst %,%-clean,$(MODELS_LIST) $(TESTS_LIST)) \
        example_legos-all example_legos-pristine

all: example_legos-all
therest: kim-api-all $(patsubst %,%-all,$(MODELS_LIST)) $(patsubst %,%-all,$(TESTS_LIST))

clean: $(patsubst %,%-clean,$(MODELS_LIST)) $(patsubst %,%-clean,$(TESTS_LIST)) kim-api-clean

pristine: clean example_legos-pristine

kim-api-all:
	$(MAKE) -C $(KIM_API_DIR) all 
	@echo

kim-api-clean: | $(patsubst %,%-clean,$(MODELS_LIST)) $(patsubst %,%-clean,$(TESTS_LIST))
	$(MAKE) -C $(KIM_API_DIR) clean
	@echo

example_legos-all: 
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS all
	$(MAKE) -C $(KIM_DIR) therest

example_legos-pristine: 
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/model_legos pristine

$(patsubst %,%-all,$(MODELS_LIST)): | kim-api-all
	$(MAKE) -C $(KIM_MODELS_DIR)$(patsubst %-all,%,$@) all
	@echo

$(patsubst %,%-clean,$(MODELS_LIST)):
	$(MAKE) -C $(KIM_MODELS_DIR)$(patsubst %-clean,%,$@) clean
	@echo

$(patsubst %,%-all,$(TESTS_LIST)): | kim-api-all $(patsubst %,%-all,$(MODELS_LIST))
	$(MAKE) -C $(KIM_TESTS_DIR)$(patsubst %-all,%,$@) all
	@echo

$(patsubst %,%-clean,$(TESTS_LIST)):
	$(MAKE) -C $(KIM_TESTS_DIR)$(patsubst %-clean,%,$@) clean
	@echo

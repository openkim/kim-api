
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

MODELS_LIST = $(notdir $(filter-out .% $(F_FILTER_OUT),$(shell find $(KIM_MODELS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))
TESTS_LIST  = $(notdir $(filter-out .% $(F_FILTER_OUT),$(shell find $(KIM_TESTS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))

.PHONY: all clean $(patsubst %,%-all,$(MODELS_LIST) $(TESTS_LIST)) $(patsubst %,%-clean,$(MODELS_LIST) $(TESTS_LIST))

all: kim-api-all $(patsubst %,%-all,$(MODELS_LIST)) $(patsubst %,%-all,$(TESTS_LIST))

clean: $(patsubst %,%-clean,$(MODELS_LIST)) $(patsubst %,%-clean,$(TESTS_LIST)) kim-api-clean

kim-api-all:
	$(MAKE) -C $(KIM_API_DIR) all 
	@echo

kim-api-clean: | $(patsubst %,%-clean,$(MODELS_LIST)) $(patsubst %,%-clean,$(TESTS_LIST))
	$(MAKE) -C $(KIM_API_DIR) clean
	@echo

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

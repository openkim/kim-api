
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

MODELS_LIST = $(notdir $(filter-out .%,$(shell find $(KIM_MODELS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))
TESTS_LIST  = $(notdir $(filter-out .%,$(shell find $(KIM_TESTS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))

.PHONY: all openkim-api clean                              \
        kim-api-all kim-api-clean                          \
        $(patsubst %,%-all,$(MODELS_LIST) $(TESTS_LIST))   \
        $(patsubst %,%-clean,$(MODELS_LIST) $(TESTS_LIST))

# compile everything in the standard directories
all: models_check kim-api-all $(patsubst %,%-all,$(MODELS_LIST)) $(patsubst %,%-all,$(TESTS_LIST))

# other targets 
openkim-api: kim-api-all      # compile the openkim-api

# cleaning targets
clean: $(patsubst %,%-clean,$(MODELS_LIST)) $(patsubst %,%-clean,$(TESTS_LIST)) kim-api-clean


########### for internal use ###########
kim-api-all:
	$(MAKE) -C $(KIM_API_DIR) all 
	@echo

kim-api-clean:
	$(MAKE) -C $(KIM_API_DIR) clean
	rm -f kim.log
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

models_check:
	@if [[ "$(MODELS_LIST)" == "" && "$(KIM_DYNAMIC)" == "" ]]; then \
        echo "*************************************************************************"; \
        echo "*******  Can't compile the API for static linking with no Models  *******"; \
        echo "*************************************************************************"; \
        false; else true; fi

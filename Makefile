
#
# Release: This file is part of the openkim-api.git repository.
# 
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
# All rights reserved.
#
# Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#
#
# this make file builds/cleans everything
#
#

include $(KIM_DIR)KIM_API/Include.mk

MODEL_DRIVERS_LIST = $(notdir $(filter-out .%,$(shell find $(KIM_MODEL_DRIVERS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))
MODELS_LIST = $(notdir $(filter-out .%,$(shell find $(KIM_MODELS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))
TESTS_LIST  = $(notdir $(filter-out .%,$(shell find $(KIM_TESTS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))

.PHONY: all lib openkim-api examples examples-all clean clean-examples           \
        kim-api-all kim-api-lib kim-api-clean                                    \
        $(patsubst %,%-all,  $(MODELS_LIST) $(MODEL_DRIVERS_LIST) $(TESTS_LIST)) \
        $(patsubst %,%-clean,$(MODELS_LIST) $(MODEL_DRIVERS_LIST) $(TESTS_LIST))

# compile everything in the standard directories
ifdef KIM_DYNAMIC
   all: models_check kim-api-all kim-api-lib $(patsubst %,%-all,$(MODEL_DRIVERS_LIST) \
        $(MODELS_LIST)) $(patsubst %,%-all,$(TESTS_LIST))
else
   all: models_check kim-api-all $(patsubst %,%-all,$(MODEL_DRIVERS_LIST) $(MODELS_LIST)) \
        kim-api-lib $(patsubst %,%-all,$(TESTS_LIST))
endif

# other targets 
openkim-api: kim-api-all kim-api-lib     # compile the openkim-api
examples: examples-all                   # copy examples to appropriate directories then make

# cleaning targets
clean: $(patsubst %,%-clean,$(MODELS_LIST) $(MODEL_DRIVERS_LIST) $(TESTS_LIST)) kim-api-clean
clean-examples: 

########### for internal use ###########
kim-api-all:
	$(MAKE) -C $(KIM_API_DIR) all 
	@echo

kim-api-lib:
	$(MAKE) -C $(KIM_API_DIR) lib
	@echo

kim-api-clean:
	$(MAKE) -C $(KIM_API_DIR) clean
	rm -f kim.log
	@echo

examples-all:
	cp -r -n $(KIM_DIR)EXAMPLEs/MODEL_DRIVERs/* "$(KIM_MODEL_DRIVERS_DIR)"
	cp -r -n $(KIM_DIR)EXAMPLEs/MODELs/* "$(KIM_MODELS_DIR)"
	cp -r -n $(KIM_DIR)EXAMPLEs/TESTs/* "$(KIM_TESTS_DIR)"
	$(MAKE) all

examples-clean:
	$(foreach dr,$(notdir $(wildcard $(KIM_DIR)EXAMPLEs/MODEL_DRIVERs/*)), rm -rf "$(KIM_MODEL_DRIVERS_DIR)$(dr)";)
	$(foreach dr,$(notdir $(wildcard $(KIM_DIR)EXAMPLEs/MODELs/*)), rm -rf "$(KIM_MODELS_DIR)$(dr)";)
	$(foreach dr,$(notdir $(wildcard $(KIM_DIR)EXAMPLEs/TESTs/*)), rm -rf "$(KIM_TESTS_DIR)$(dr)";)

$(patsubst %,%-all,$(MODELS_LIST)): | kim-api-all
	$(MAKE) -C $(KIM_MODELS_DIR)$(patsubst %-all,%,$@) all
	@echo

$(patsubst %,%-clean,$(MODELS_LIST)):
	$(MAKE) -C $(KIM_MODELS_DIR)$(patsubst %-clean,%,$@) clean
	@echo

$(patsubst %,%-all,$(MODEL_DRIVERS_LIST)): | kim-api-all
	$(MAKE) -C $(KIM_MODEL_DRIVERS_DIR)$(patsubst %-all,%,$@) all
	@echo

$(patsubst %,%-clean,$(MODEL_DRIVERS_LIST)):
	$(MAKE) -C $(KIM_MODEL_DRIVERS_DIR)$(patsubst %-clean,%,$@) clean
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
        echo "*******           Maybe you want to do 'make examples'            *******"; \
        echo "*************************************************************************"; \
        false; else true; fi

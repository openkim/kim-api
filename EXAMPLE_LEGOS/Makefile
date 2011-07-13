
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Ryan S. Elliott
#
# 
# this make file builds/cleans the example models and tests
#
#

include $(KIM_DIR)KIM_API/Include.mk

.PHONY: all clean pristine

all: 
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/model_legos all
	@echo
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/test_legos  all

clean: 
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/model_legos clean
	@echo
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/test_legos  clean

pristine: 
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/model_legos pristine
	@echo
	$(MAKE) -C $(KIM_DIR)EXAMPLE_LEGOS/test_legos  pristine
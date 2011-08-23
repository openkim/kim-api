
#
# Release: This file is part of the openkim-api.git repository.
#
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
# All rights reserved.
#
# Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

TEST_NAME := test_Ar_multiple_models

ifndef KIM_API_DIR
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


LOCALOBJ = 

all: $(TEST_NAME)

$(TEST_NAME): $(TEST_NAME).o $(LOCALOBJ) $(if $(strip $(KIM_DYNAMIC)),,$(MODELOBJ)) $(KIM_LIB_FILE)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(LOCALOBJ) $< $(KIM_LIB) $(MODELOBJ) $(KIM_LIB) $(OUTPUTIN) $@
#	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(LOCALOBJ) $< $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@

$(TEST_NAME).o: $(LOCALOBJ) Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o $(TEST_NAME)

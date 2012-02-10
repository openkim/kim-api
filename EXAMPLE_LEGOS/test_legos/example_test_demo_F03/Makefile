
#
# Release: This file is part of the openkim-api.git repository.
#
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
# All rights reserved.
#
# Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

TEST_NAME := test_demo_F03

ifndef KIM_API_DIR
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


LOCALOBJ = neighborlistmod.o myqsort.o

all: $(TEST_NAME)

$(TEST_NAME): $(TEST_NAME).o $(LOCALOBJ) $(KIM_LIB_FILE)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(LOCALOBJ) $< $(KIM_LIB) $(OUTPUTIN) $@

$(TEST_NAME).o: $(LOCALOBJ) Makefile

neighborlistmod.o: myqsort.o Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o $(TEST_NAME)

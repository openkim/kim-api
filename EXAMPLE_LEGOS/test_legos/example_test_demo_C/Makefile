
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

TEST_NAME := test_demo_C

ifndef KIM_API_DIR
   KIM_DIR = ../../
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


ifdef KIM_NO_FORTRAN
   LOCALOBJ = sample_neighbor.o
else
   LOCALOBJ = neighborlistmod.o myqsort.o
endif

all: $(TEST_NAME)

$(TEST_NAME): $(TEST_NAME).o $(if $(strip $(KIM_DYNAMIC)),,$(MODELOBJ)) $(KIM_LIB_FILE)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(LOCALOBJ) $<  $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@

$(TEST_NAME).o: $(LOCALOBJ) Makefile  $(KIM_API_DIR)KIMserviceC.h
neighborlistmod.o: myqsort.o Makefile $(KIM_API_DIR)KIMserviceC.h


Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o  $(TEST_NAME)


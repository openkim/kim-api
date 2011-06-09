
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


ifdef KIM_NO_FORTRAN
   LOCALOBJ = sample_neighbor.o
else
   LOCALOBJ = neighborlistmod.o myqsort.o
endif

all: Sample_01_compute_example_c

Sample_01_compute_example_c: lj_test.o $(ifndef KIM_DYNAMIC, $(LOCALOBJ),)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(LOCALOBJ) $<  $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@

lj_test.o: $(LOCALOBJ) Makefile  $(KIM_API_DIR)KIMserviceC.h
neighborlistmod.o: myqsort.o Makefile $(KIM_API_DIR)KIMserviceC.h


Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o  Sample_01_compute_example_c


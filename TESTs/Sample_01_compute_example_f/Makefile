
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


LOCALOBJ = neighborlistmod.o myqsort.o

all: Sample_01_compute_example_f

Sample_01_compute_example_f: lj_test.o $(LOCALOBJ) $(if $(strip $(KIM_DYNAMIC)),,$(MODELOBJ))
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG)  $(LOCALOBJ)  $< $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@

lj_test.o: $(LOCALOBJ) Makefile

neighborlistmod.o: myqsort.o Makefile


Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o  Sample_01_compute_example_f

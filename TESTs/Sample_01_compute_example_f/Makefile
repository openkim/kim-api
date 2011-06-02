#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski                                         
#

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


LOCALOBJ=  neighborlistmod.o myqsort.o

all:   Sample_01_compute_example_f  $(LOCALOBJ)

Sample_01_compute_example_f: lj_test.o $(LOCALOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG)  $(LOCALOBJ)  $< $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@



#local dependencies
lj_test.o: $(LOCALOBJ)

neighborlistmod.o: myqsort.o

clean:
	rm -f *.mod *.o  Sample_01_compute_example_f

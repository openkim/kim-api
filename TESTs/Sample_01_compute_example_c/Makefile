
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski                                         
#


include ../../KIM_API/Include.mk

LOCALOBJ= neighborlistmod.o myqsort.o
ifdef NO_FORTRAN
LOCALOBJ= simple_neighbor.o
endif

all: Sample_01_compute_example_c_c  Sample_01_compute_example_c_f  Sample_01_compute_example_c_cpp $(LOCALOBJ)

Sample_01_compute_example_c_c: lj_test_c.o $(LOCALOBJ) $(MODELOBJ) $(ALLOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ) lj_test_c.o $(OUTPUTIN) $@

ifdef NO_FORTRAN
Sample_01_compute_example_c_f: 
	echo Skip fortran LJ
else
Sample_01_compute_example_c_f: lj_test_f.o $(LOCALOBJ) $(MODELOBJ) $(ALLOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ)  lj_test_f.o $(OUTPUTIN) $@
endif

Sample_01_compute_example_c_cpp: lj_test_cpp.o $(LOCALOBJ) $(MODELOBJ) $(ALLOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ)  lj_test_cpp.o $(OUTPUTIN) $@


#local dependencies
lj_test_c.o: $(LOCALOBJ)
lj_test_cpp.o: $(LOCALOBJ)
lj_test_f.o: $(LOCALOBJ)
neighborlistmod.o: myqsort.o

clean:
	rm -f *.mod *.o  Sample_01_compute_example_c_c Sample_01_compute_example_c_f Sample_01_compute_example_c_cpp

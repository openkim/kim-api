#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski                                         
#


include ../../KIM_API/Include.mk

LOCALOBJ= neighborlistmod.o myqsort.o

all: Sample_01_compute_example_f_c  Sample_01_compute_example_f_f  Sample_01_compute_example_f_cpp $(LOCALOBJ)

Sample_01_compute_example_f_c: lj_test_c.o $(LOCALOBJ) $(ALLOBJ) $(MODELOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ) lj_test_c.o $(OUTPUTIN) $@

Sample_01_compute_example_f_f: lj_test_f.o $(LOCALOBJ) $(ALLOBJ) $(MODELOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ)  lj_test_f.o $(OUTPUTIN) $@

Sample_01_compute_example_f_cpp: lj_test_cpp.o $(LOCALOBJ) $(ALLOBJ) $(MODELOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $(LOCALOBJ)  lj_test_cpp.o $(OUTPUTIN) $@


#local dependencies
lj_test_c.o: $(LOCALOBJ)
lj_test_cpp.o: $(LOCALOBJ)
lj_test_f.o: $(LOCALOBJ)
neighborlistmod.o: myqsort.o

clean:
	rm -f *.mod *.o  Sample_01_compute_example_f_c Sample_01_compute_example_f_f Sample_01_compute_example_f_cpp

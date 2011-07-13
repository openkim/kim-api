
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


LOCALOBJ = 

all: test_Ar_multiple_models_c

test_Ar_multiple_models_c: test_Ar_multiple_models_c.o $(LOCALOBJ) $(if $(strip $(KIM_DYNAMIC)),,$(MODELOBJ)) $(KIM_LIB_FILE)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG)  $(LOCALOBJ)  $< $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@ $(KIM_LIB)
#	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG)  $(LOCALOBJ)  $< $(KIM_LIB) $(MODELOBJ) $(OUTPUTIN) $@

test_Ar_multiple_models_c.o: $(LOCALOBJ) Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.mod *.o  test_Ar_multiple_models_c

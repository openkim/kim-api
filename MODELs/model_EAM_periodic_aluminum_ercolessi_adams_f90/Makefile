
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_BUILD_TARGET:= model_EAM_periodic_aluminum_ercolessi_adams_f90.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

all: $(MODEL_BUILD_TARGET)

model_EAM_periodic_aluminum_ercolessi_adams_f90.a: model_EAM_periodic_aluminum_ercolessi_adams.o
	@ar rcs $@ *.o

model_EAM_periodic_aluminum_ercolessi_adams.o: Makefile 

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.o *.so *.mod *.a *.so


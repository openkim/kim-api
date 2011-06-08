
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_BUILD_TARGET:= Sample_01_lj_cutoff_c.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

all: $(MODEL_BUILD_TARGET)

Sample_01_lj_cutoff_c.a:  LJ_sample1.o
	@ar rcs  $@ *.o

LJ_sample1.o: Makefile $(KIM_API_DIR)KIMserviceC.h

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.o *.a *.so

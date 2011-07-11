
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_BUILD_TARGET := model_Ar_LJ_NEIGH_RVEC_F_c.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

all: $(MODEL_BUILD_TARGET)

model_Ar_LJ_NEIGH_RVEC_F_c.a: model_Ar_LJ_NEIGH_RVEC_F_c.o
	ar rcs $@ *.o

model_Ar_LJ_NEIGH_RVEC_F_c.o: Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile
clean:
	rm -f *.o *.a *.so


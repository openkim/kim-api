
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_BUILD_TARGET:= model_Ne_P_LJ_NEIGH_PURE_H.a

ifndef KIM_API_DIR
   KIM_DIR = ../../
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

model_Ne_P_LJ_NEIGH_PURE_H.a:  model_Ne_P_LJ_NEIGH_PURE_H.o model_Ne_P_LJ_NEIGH_PURE_H_compute.o
	ar rcs  $@ *.o

model_Ne_P_LJ_NEIGH_PURE_H.o: Makefile 
model_Ne_P_LJ_NEIGH_PURE_H_compute.o: Makefile 


Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.o *.mod *.a *.so

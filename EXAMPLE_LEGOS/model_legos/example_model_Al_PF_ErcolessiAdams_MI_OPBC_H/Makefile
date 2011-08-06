
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_NAME := model_Al_PF_ErcolessiAdams_MI_OPBC_H
MODEL_BUILD_TARGET := $(MODEL_NAME).a

ifndef KIM_API_DIR
   KIM_DIR = ../../
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

$(MODEL_NAME).a: $(MODEL_NAME).o
	ar rcs $@ *.o

$(MODEL_NAME).o: Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.o *.mod *.a *.so

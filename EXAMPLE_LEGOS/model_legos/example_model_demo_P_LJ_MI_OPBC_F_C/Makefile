
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_NAME := model_demo_P_LJ_MI_OPBC_F_C
MODEL_BUILD_TARGET:= $(MODEL_NAME).a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

$(MODEL_NAME).a:  $(MODEL_NAME).o
	ar rcs  $@ *.o

clean:
	rm -f *.o *.mod *.a *.so

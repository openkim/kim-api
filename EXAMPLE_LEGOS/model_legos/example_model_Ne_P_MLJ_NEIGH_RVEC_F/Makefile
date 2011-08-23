
#
# Release: This file is part of the openkim-api.git repository.
#
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
# All rights reserved.
#
# Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_NAME := model_Ne_P_MLJ_NEIGH_RVEC_F
MODEL_BUILD_TARGET := $(MODEL_NAME).a

ifndef KIM_API_DIR
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

$(MODEL_NAME).a: $(MODEL_NAME).o $(MODEL_NAME)_kim_str.o
	ar rcs $@ *.o

$(MODEL_NAME).o: Makefile

Makefile: $(KIM_API_DIR)Include.mk
	@touch Makefile

clean:
	rm -f *.o *.mod *.a *.so $(MODEL_NAME)_kim_str.{o,cpp}

#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

MODEL_BUILD_TARGET:= Sample_01_lj_cutoff_CLUSTER_f.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

Sample_01_lj_cutoff_CLUSTER_f.a:  LJ_mod_cluster.o
	@ar rcs  $@ *.o

clean:
	rm -f *.o *.mod *.a *.so



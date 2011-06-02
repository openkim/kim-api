
# KIM API alpha release, 11-Feb-2011
# 
# this make file builds/cleans everything 
#
#

configall:
	cd KIM_API/; make configmodels; cd ../
	cd KIM_API/; make configtests; cd ../
	

include KIM_API/Include.mk
# here all: is defined
ifeq ($(strip $(wildcard $(KIM_API_DIR)testsmake.mk)),$(KIM_API_DIR)testsmake.mk) 
include $(KIM_API_DIR)testsmake.mk
else
clean:
endif


cleanall: clean
	cd KIM_API/; make cleanall; cd ../	
	

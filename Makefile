
# KIM API alpha release, 11-Feb-2011
# 
# this make file builds/cleans everything 
#
#

configall:
	$(MAKE) -C KIM_API/ configmodels
	$(MAKE) -C KIM_API/ configtests

include KIM_API/Include.mk
# here all: is defined
ifeq ($(strip $(wildcard $(KIM_API_DIR)testsmake.mk)),$(KIM_API_DIR)testsmake.mk) 
include $(KIM_API_DIR)testsmake.mk
else
clean:
endif


cleanall: clean
	$(MAKE) -C KIM_API/ cleanall

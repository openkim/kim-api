#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Ryan S. Elliott
#

ifndef KIM_API_DIR
   KIM_DIR = ../../
   include $(KIM_DIR)KIM_API/Include.mk
else
   include $(KIM_API_DIR)Include.mk
endif

VPATH = $(KIM_TESTS_DIR)

SED_CMD = sed -e "s/TEST_NAME_STR/$@/g"

COMMON_FRAGMENTS = 

AUTO_GEN_TESTS = Sample_01_compute_example_c Sample_01_compute_example_f test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90 \
                 test_Ar_free_cluster_CLUSTER_f90 test_Ar_free_cluster_f90 test_Ar_multiple_models_c           \
                 test_Ar_periodic_FCC_MI_OPBC_f90 test_Ar_periodic_FCC_NEIGH_PURE_f90                          \
                 test_Ar_periodic_FCC_NEIGH_RVEC_f90 test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90


all: $(AUTO_GEN_TESTS) EXAMPLES.README

EXAMPLES.README: test_EXAMPLES.README Makefile
	cp test_EXAMPLES.README $(KIM_TESTS_DIR)EXAMPLES.README

Sample_01_compute_example_c: example_Sample_01_compute_example_c               \
                             $(wildcard example_Sample_01_compute_example_c/*) \
                             Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

Sample_01_compute_example_f: example_Sample_01_compute_example_f               \
                             $(wildcard example_Sample_01_compute_example_f/*) \
                             Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90: example_test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90               \
                                       $(wildcard example_test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90/*) \
                                       Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_free_cluster_CLUSTER_f90: example_test_Ar_free_cluster_CLUSTER_f90               \
                                  $(wildcard example_test_Ar_free_cluster_CLUSTER_f90/*) \
                                  Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_free_cluster_f90: example_test_Ar_free_cluster_f90               \
                          $(wildcard example_test_Ar_free_cluster_f90/*) \
                          Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_multiple_models_c: example_test_Ar_multiple_models_c               \
                           $(wildcard example_test_Ar_multiple_models_c/*) \
                           Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_periodic_FCC_MI_OPBC_f90: example_test_Ar_periodic_FCC_MI_OPBC_f90               \
                                  $(wildcard example_test_Ar_periodic_FCC_MI_OPBC_f90/*) \
                                  Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_periodic_FCC_NEIGH_PURE_f90: example_test_Ar_periodic_FCC_NEIGH_PURE_f90               \
                                     $(wildcard example_test_Ar_periodic_FCC_NEIGH_PURE_f90/*) \
                                     Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_periodic_FCC_NEIGH_RVEC_f90: example_test_Ar_periodic_FCC_NEIGH_RVEC_f90               \
                                     $(wildcard example_test_Ar_periodic_FCC_NEIGH_RVEC_f90/*) \
                                     Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90: example_test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90               \
                                         $(wildcard example_test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90/*) \
                                         Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/


.PHONY: clean pristine

clean: 

pristine: 
	rm -rf $(addprefix $(KIM_TESTS_DIR),$(AUTO_GEN_TESTS)) $(KIM_TESTS_DIR)EXAMPLES.README

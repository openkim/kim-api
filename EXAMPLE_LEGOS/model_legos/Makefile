#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Ryan S. Elliott
#

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

VPATH = $(KIM_MODELS_DIR)

SED_CMD = sed -e "s/MODEL_NAME_STR/$@/g"

COMMON_FRAGMENTS = module_start.frag compute_kim_variables.frag compute_initialize.frag module_end.frag model_README model_Makefile descriptor_file_start descriptor_file_input descriptor_file_output descriptor_file_params Makefile

AUTO_GEN_MODELS = model_Ar_LJ_CLUSTER_f90 model_Ar_LJ_MI_OPBC_H_F_f90 model_Ar_LJ_NEIGH_PURE_H_F_f90 model_Ar_LJ_NEIGH_RVEC_F_f90 model_Ar_LJ_NEIGH_RVEC_F_c model_Ar_Morse_c model_Ar_Ne_LJ_NEIGH_RVEC_F_f90

all: $(AUTO_GEN_MODELS)

model_Ar_LJ_CLUSTER_f90: $(COMMON_FRAGMENTS) cluster_compute_local_variables.frag cluster_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/CLUSTER/g"          >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start        | $(SED_CMD)                              >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "CLUSTER                 dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input        | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat cluster_compute_local_variables.frag                                   >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat cluster_compute_loops.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD)                                           >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_LJ_MI_OPBC_H_F_f90: $(COMMON_FRAGMENTS) mi_opbc_neigh_pure_compute_local_variables.frag mi_opbc_compute_extra_kim_variables.frag mi_opbc_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/MI-OPBC-[H|F]/g" >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start        | $(SED_CMD)                              >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input        | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_half_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_full_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "neighObject             pointer      none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]"                         >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_neigh_pure_compute_local_variables.frag                        >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_compute_extra_kim_variables.frag                               >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_compute_loops.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD)                                           >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_LJ_NEIGH_PURE_H_F_f90: $(COMMON_FRAGMENTS) mi_opbc_neigh_pure_compute_local_variables.frag neigh_pure_compute_extra_kim_variables.frag neigh_pure_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/NEIGH-PURE-[H|F]/g" >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD)                                     >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input        | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_half_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_full_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "neighObject             pointer      none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_neigh_pure_compute_local_variables.frag                        >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_pure_compute_extra_kim_variables.frag                            >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_pure_compute_loops.frag                                          >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD)                                           >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_LJ_NEIGH_RVEC_F_f90: $(COMMON_FRAGMENTS) neigh_rvec_compute_local_variables.frag neigh_rvec_compute_extra_kim_variables.frag neigh_rvec_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/NEIGH-RVEC-F/g"     >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD)                                     >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input        | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_half_neigh          method       none       none                    []                 optional     # Here so it can work with more Tests" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_full_neigh          method       none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "neighObject             pointer      none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params       | $(SED_CMD)                              >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_local_variables.frag                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_extra_kim_variables.frag                            >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_loops.frag                                          >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD)                                           >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_LJ_NEIGH_RVEC_F_c: example_model_Ar_LJ_NEIGH_RVEC_F_c $(wildcard example_model_Ar_LJ_NEIGH_RVEC_F_c/*) Makefile
	cp -r example_model_Ar_LJ_NEIGH_RVEC_F_c $(KIM_MODELS_DIR)model_Ar_LJ_NEIGH_RVEC_F_c


model_Ar_Morse_c: example_model_Ar_Morse_c $(wildcard example_model_Ar_Morse_c/*) Makefile
	cp -r example_model_Ar_Morse_c $(KIM_MODELS_DIR)model_Ar_Morse_c

model_Ar_Ne_LJ_NEIGH_RVEC_F_f90: example_model_Ar_Ne_LJ_NEIGH_RVEC_F_f90 $(wildcard example_model_Ar_Ne_LJ_NEIGH_RVEC_F_f90/*) Makefile
	cp -r example_model_Ar_Ne_LJ_NEIGH_RVEC_F_f90 $(KIM_MODELS_DIR)model_Ar_Ne_LJ_NEIGH_RVEC_F_f90


.PHONY: clean pristine

clean: 

pristine: 
	rm -rf $(addprefix $(KIM_MODELS_DIR),$(AUTO_GEN_MODELS))
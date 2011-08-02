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


VPATH = $(KIM_MODELS_DIR)

SED_CMD = sed -e "s/MODEL_NAME_STR/$@/g"

COMMON_FRAGMENTS = module_start.frag compute_kim_variables.frag compute_initialize.frag module_end.frag              \
                   model_LJ_README model_Makefile descriptor_file_start descriptor_file_input descriptor_file_output \
                   descriptor_file_params Makefile

AUTO_GEN_MODELS = model_Al_PF_ErcolessiAdams_MI_OPBC_H  \
                  model_ArNe_P_LJ_NEIGH_RVEC_F          \
                  model_Ar_P_LJ_CLUSTER                 \
                  model_Ar_P_LJ_MI_OPBC_H_F             \
                  model_Ar_P_LJ_NEIGH_PURE_H_F          \
                  model_Ar_P_LJ_NEIGH_RVEC_F            \
                  model_Ar_P_Morse                      \
                  model_Ne_P_LJ_NEIGH_RVEC_F            \
                  model_demo_P_LJ_CLUSTER_F             \
                  model_demo_P_LJ_MI_OPBC_H_C           \
                  model_demo_P_LJ_MI_OPBC_F_C           \
                  model_demo_P_LJ_NEIGH_PURE_H_C        \
                  model_demo_P_LJ_NEIGH_PURE_H_CPP      \
                  model_demo_P_LJ_NEIGH_PURE_H_F        \
                  model_demo_P_LJ_NEIGH_PURE_H_F77      \
                  model_demo_P_LJ_NEIGH_RVEC_F_F

all: $(AUTO_GEN_MODELS) EXAMPLES.README

EXAMPLES.README: model_EXAMPLES.README Makefile
	cp model_EXAMPLES.README $(KIM_MODELS_DIR)EXAMPLES.README

model_Ar_P_LJ_CLUSTER: $(COMMON_FRAGMENTS) cluster_compute_local_variables.frag cluster_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_LJ_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/CLUSTER/g"       >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD) \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    1/g' \
                                                                                   >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "CLUSTER                 dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input                                                  >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat cluster_compute_local_variables.frag                                   >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag | sed -e 's/SPECIES_CODE_STR/1/g'              >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat cluster_compute_loops.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD) \
                            | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
                            | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
                            | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'   \
                                                                                   >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_P_LJ_MI_OPBC_H_F: $(COMMON_FRAGMENTS) mi_opbc_neigh_pure_compute_local_variables.frag \
                           mi_opbc_compute_extra_kim_variables.frag mi_opbc_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_LJ_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/MI-OPBC-[H|F]/g" >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD) \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    1/g' \
                                                                                   >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "Neigh_LocaAccess        dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input                                                  >> $(KIM_MODELS_DIR)$@/$@.kim
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
	cat descriptor_file_output                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_neigh_pure_compute_local_variables.frag                        >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_compute_extra_kim_variables.frag                               >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag | sed -e 's/SPECIES_CODE_STR/1/g'              >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_compute_loops.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD) \
                            | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
                            | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
                            | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'   \
                                                                                   >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_P_LJ_NEIGH_PURE_H_F: $(COMMON_FRAGMENTS) mi_opbc_neigh_pure_compute_local_variables.frag \
                              neigh_pure_compute_extra_kim_variables.frag neigh_pure_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_LJ_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/NEIGH-PURE-[H|F]/g" >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD) \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    1/g' \
                                                                                   >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "Neigh_LocaAccess        dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input                                                  >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_half_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_full_neigh          method       none       none                    []                 optional" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "neighObject             pointer      none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat mi_opbc_neigh_pure_compute_local_variables.frag                        >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_pure_compute_extra_kim_variables.frag                            >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag | sed -e 's/SPECIES_CODE_STR/1/g'              >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_pure_compute_loops.frag                                          >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD) \
                            | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
                            | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
                            | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'   \
                                                                                   >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_P_LJ_NEIGH_RVEC_F: $(COMMON_FRAGMENTS) neigh_rvec_compute_local_variables.frag \
                            neigh_rvec_compute_extra_kim_variables.frag neigh_rvec_compute_loops.frag
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_LJ_README | $(SED_CMD) | sed -e "s/NBC_NAME_STR/NEIGH-RVEC-F/g"  >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start | $(SED_CMD) \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    1/g' \
                                                                                   >  $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "Neigh_LocaAccess        dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_input                                                  >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_half_neigh          method       none       none                    []                 optional     # Here so it can work with more Tests" >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "get_full_neigh          method       none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo "neighObject             pointer      none       none                    []"                          >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_output                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	echo                                                                       >> $(KIM_MODELS_DIR)$@/$@.kim
	cat descriptor_file_params                                                 >> $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat module_start.frag | $(SED_CMD)                                         >  $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_local_variables.frag                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_kim_variables.frag                                             >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_extra_kim_variables.frag                            >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat compute_initialize.frag | sed -e 's/SPECIES_CODE_STR/1/g'              >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	echo "    "                                                                >> $(KIM_MODELS_DIR)$@/$@.F90
	cat neigh_rvec_compute_loops.frag                                          >> $(KIM_MODELS_DIR)$@/$@.F90
	cat module_end.frag | $(SED_CMD) \
                            | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
                            | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
                            | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'   \
                                                                                   >> $(KIM_MODELS_DIR)$@/$@.F90

model_Ar_P_Morse: model_Morse_README model_Makefile model_P_Morse.kim  model_P_Morse.c Makefile
        # make directory
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
        # make README file in the directory
	cat model_Morse_README | $(SED_CMD)                                        >  $(KIM_MODELS_DIR)$@/README
        # make Makefile in the directory
	cat model_Makefile | $(SED_CMD)                                            >  $(KIM_MODELS_DIR)$@/Makefile
        # make .kim file in the directory
	cat model_P_Morse.kim  | $(SED_CMD) \
                               | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    1/g' \
                                                                                   >  $(KIM_MODELS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat model_P_Morse.c | $(SED_CMD) \
            | sed -e `echo $@ | tr A-Z a-z | sed -e 's/^\(.*\)/s\/MODEL_NAME_LC_STR\/\1\/g/'` \
            | sed -e 's/SPECIES_NAME_STR/Ar/g' \
            | sed -e 's/REFERENCE_STR/(Parameters taken from: G. E. Jelinek, Phys. Rev. B, Vol 5, no 8, 1972, pp3210--3217)/g' \
            | sed -e 's/SPECIES_CODE_STR/1/g' \
            | sed -e 's/CUTOFF_VALUE_STR/*model_cutoff = 8.15; \/* cutoff distance in angstroms *\//g' \
            | sed -e 's/EPSILON_VALUE_STR/*model_epsilon = - (156.41\/(1.160451911e4)); \/* Morse epsilon in eV *\//g' \
            | sed -e 's/C_VALUE_STR/*model_C = 1.545; \/* Morse C in 1\/Angstroms *\//g' \
            | sed -e 's/RZERO_VALUE_STR/*model_Rzero = 3.786; \/* Morse Rzero in Angstroms *\//g' \
                                                                                   > $(KIM_MODELS_DIR)$@/$@.c

model_Al_PF_ErcolessiAdams_MI_OPBC_H: example_model_Al_PF_ErcolessiAdams_MI_OPBC_H              \
                                      $(wildcard example_model_Al_PF_ErcolessAdams_MI_OPBC_H/*) \
                                      Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/


model_ArNe_P_LJ_NEIGH_RVEC_F: example_model_ArNe_P_LJ_NEIGH_RVEC_F               \
                              $(wildcard example_model_ArNe_P_LJ_NEIGH_RVEC_F/*) \
                              Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_Ne_P_LJ_NEIGH_RVEC_F: example_model_Ne_P_LJ_NEIGH_RVEC_F               \
                            $(wildcard example_model_Ne_P_LJ_NEIGH_RVEC_F/*) \
                            Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_CLUSTER_F: example_model_demo_P_LJ_CLUSTER_f               \
                               $(wildcard example_model_demo_P_LJ_CLUSTER_f/*) \
                               Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_MI_OPBC_H_C: example_model_demo_P_LJ_MI_OPBC_H_C               \
                                    $(wildcard example_model_demo_P_LJ_MI_OPBC_H_C/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_MI_OPBC_F_C: example_model_demo_P_LJ_MI_OPBC_F_C               \
                                    $(wildcard example_model_demo_P_LJ_MI_OPBC_F_C/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_PURE_F_F: example_model_demo_P_LJ_NEIGH_PURE_F_F               \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_PURE_F_F/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_PURE_H_C: example_model_demo_P_LJ_NEIGH_PURE_H_C               \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_PURE_H_C/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_PURE_H_CPP: example_model_demo_P_LJ_NEIGH_PURE_H_CPP            \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_PURE_H_CPP/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_PURE_H_F: example_model_demo_P_LJ_NEIGH_PURE_H_F               \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_PURE_H_F/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_PURE_H_F77: example_model_demo_P_LJ_NEIGH_PURE_H_F77               \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_PURE_H_F77/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/

model_demo_P_LJ_NEIGH_RVEC_F_F: example_model_demo_P_LJ_NEIGH_RVEC_F_F               \
                                    $(wildcard example_model_demo_P_LJ_NEIGH_RVEC_F_F/*) \
                                    Makefile
	mkdir -p $(KIM_MODELS_DIR)$@
	@touch $(KIM_MODELS_DIR)$@
	cp -rf example_$@/* $(KIM_MODELS_DIR)$@/


.PHONY: clean

clean: 

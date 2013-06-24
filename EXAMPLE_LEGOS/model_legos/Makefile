#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the openkim-api.git repository.
#


KIM_DIR := ../..
KIM_EX_MODELS_DIR := $(KIM_DIR)/EXAMPLES/MODELS
KIM_EX_MODEL_DRIVERS_DIR := $(KIM_DIR)/EXAMPLES/MODEL_DRIVERS

VPATH = $(KIM_EX_MODELS_DIR) $(KIM_EX_MODEL_DRIVERS_DIR)

SED_CMD = sed -e "s/MODEL_NAME_STR/$@/g"

ifeq ($(flavor debug),undefined)
   F_CAT := cat
   C_CAT := cat
   D_CAT := cat
else
   F_CAT := function f_cat () ( \
           echo "!!!! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Begin !!!!!!"; \
           cat $$1; \
           echo "!!!! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End   !!!!!!"; ); \
           f_cat
   C_CAT := function c_cat () ( \
           echo "/* !! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Begin !!!!!! */"; \
           cat $$1; \
           echo "/* !! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End   !!!!!! */"; ); \
           c_cat
   D_CAT := function d_cat () ( \
           echo "\# !! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Begin !!!!!"; \
           cat $$1; \
           echo "\# !! $$1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End   !!!!!"; ); \
           d_cat
endif

COMMON_FRAGMENTS = module_start.f.frag          \
                   compute_kim_variables.f.frag \
                   compute_initialize.f.frag    \
                   module_end.f.frag            \
                   model_MLJ_README             \
                   model_Makefile               \
                   descriptor_file_start        \
                   descriptor_file_input        \
                   descriptor_file_output       \
                   descriptor_file_params       \
                   Makefile

AUTO_GEN_MODELS = ex_model_Al_PF_ErcolessiAdams    \
                  ex_model_Cu_PF_Johnson           \
                  ex_model_ArNe_P_MLJ_NEIGH_RVEC_F \
                  ex_model_Ar_P_MLJ_C              \
                  ex_model_Ar_P_MLJ_CLUSTER_C      \
                  ex_model_Ar_P_MLJ_CLUSTER_F90    \
                  ex_model_Ar_P_MLJ_F90            \
                  ex_model_Ar_P_MLJ_MI_OPBC_H_F    \
                  ex_model_Ar_P_MLJ_NEIGH_PURE_H_F \
                  ex_model_Ar_P_MLJ_NEIGH_RVEC_F   \
                  ex_model_Ne_P_LJ_NEIGH_PURE_H    \
                  ex_model_Ne_P_MLJ_NEIGH_RVEC_H   \
                  ex_model_driver_P_Morse          \
                  ex_model_Ar_P_Morse              \
                  ex_model_Ne_P_Morse              \
                  ex_model_driver_P_LJ             \
                  ex_model_Ar_P_LJ                 \
                  ex_model_Ne_P_LJ                 \
                  ex_model_Ne_P_fastLJ

all: $(AUTO_GEN_MODELS) EXAMPLES.README

EXAMPLES.README: model_EXAMPLES.README model_driver_EXAMPLES.README Makefile
	cp model_EXAMPLES.README $(KIM_EX_MODELS_DIR)/EXAMPLES.README
	cp model_driver_EXAMPLES.README $(KIM_EX_MODEL_DRIVERS_DIR)/EXAMPLES.README

ex_model_Ar_P_MLJ_CLUSTER_C: model_P_MLJ_CLUSTER_C.c             \
                             model_MLJ_README                    \
                             model_Makefile                      \
                             descriptor_file_input               \
                             descriptor_file_output              \
                             Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/CLUSTER/g" \
               -e "s/LANGUAGE_STR/C/g"                                                                        >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_LANG/C/'                                       >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g'                >  $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
        # make .c file in the directory
	$(C_CAT) model_P_MLJ_CLUSTER_C.c | $(SED_CMD) \
            | sed -e 's/SPECIES_NAME_STR/Ar/g' \
            | sed -e 's/SPECIES_CODE_STR/1/g' \
            | sed -e 's/CUTOFF_VALUE_STR/8.15; \/* cutoff distance in angstroms *\//g' \
            | sed -e 's/SIGMA_VALUE_STR/3.40; \/* LJ Argon sigma in angstroms *\//g' \
            | sed -e 's/EPSILON_VALUE_STR/0.0104; \/* LJ Argon epsilon in eV *\//g' \
                                                                                                              > $(KIM_EX_MODELS_DIR)/$@/$@.c

ex_model_Ar_P_MLJ_CLUSTER_F90: $(COMMON_FRAGMENTS)                    \
                               cluster_compute_local_variables.f.frag \
                               cluster_compute_loops.f.frag
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/CLUSTER/g" \
               -e "s/LANGUAGE_STR/F90/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_LANG/Fortran/'                                 >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g'                >  $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) cluster_compute_local_variables.f.frag                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) cluster_compute_loops.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
               -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'                   >> $(KIM_EX_MODELS_DIR)/$@/$@.F90

ex_model_Ar_P_MLJ_MI_OPBC_H_F: $(COMMON_FRAGMENTS)                               \
                               mi_opbc_neigh_pure_compute_local_variables.f.frag \
                               mi_opbc_compute_extra_kim_variables.f.frag        \
                               mi_opbc_compute_loops.f.frag
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/MI_OPBC_[H|F]/g" \
               -e "s/LANGUAGE_STR/F90/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_LANG/Fortran/'                                 >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g'                >  $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "get_neigh                   method       none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "boxSideLengths              double       length              [3]"                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) mi_opbc_neigh_pure_compute_local_variables.f.frag                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) mi_opbc_compute_extra_kim_variables.f.frag                                                   >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) mi_opbc_compute_loops.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
               -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'                   >> $(KIM_EX_MODELS_DIR)/$@/$@.F90

ex_model_Ar_P_MLJ_NEIGH_PURE_H_F: $(COMMON_FRAGMENTS)                               \
                                  mi_opbc_neigh_pure_compute_local_variables.f.frag \
                                  neigh_pure_compute_extra_kim_variables.f.frag     \
                                  neigh_pure_compute_loops.f.frag
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/NEIGH_PURE_[H|F]/g" \
               -e "s/LANGUAGE_STR/F90/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_LANG/Fortran/'                                 >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
                                                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "get_neigh                   method       none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) mi_opbc_neigh_pure_compute_local_variables.f.frag                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) neigh_pure_compute_extra_kim_variables.f.frag                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) neigh_pure_compute_loops.f.frag                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
                -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F90

ex_model_Ar_P_MLJ_NEIGH_RVEC_F: $(COMMON_FRAGMENTS)                           \
                                neigh_rvec_compute_local_variables.f.frag     \
                                neigh_rvec_compute_extra_kim_variables.f.frag \
                                neigh_rvec_compute_loops.f.frag
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/NEIGH_RVEC_F/g" \
               -e "s/LANGUAGE_STR/F90/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_LANG/Fortran/'                                 >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g'                >  $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "get_neigh                   method       none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) neigh_rvec_compute_local_variables.f.frag                                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) neigh_rvec_compute_extra_kim_variables.f.frag                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) neigh_rvec_compute_loops.f.frag                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.F90
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15d0 ! cutoff distance in angstroms/g' \
         | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40d0 ! LJ Argon sigma in angstroms/g'    \
         | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104d0 ! LJ Argon epsilon in eV/g'                   >> $(KIM_EX_MODELS_DIR)/$@/$@.F90

ex_model_driver_P_Morse: model_driver_Morse_README \
                         model_driver_Makefile     \
                         model_driver_P_Morse.kim  \
                         model_driver_P_Morse.c    \
                         Makefile
        # make directory
	mkdir -p $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	@touch $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODEL_DRIVERS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_driver_Morse_README | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g'                            >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_driver_Makefile | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' -e 's/MODEL_DRIVER_LANG/C/'    >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_driver_P_Morse.kim                                                                     >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.kim
        # make .c file in the directory
	$(C_CAT) model_driver_P_Morse.c | $(SED_CMD) \
         | sed -e `echo $@ | tr A-Z a-z | sed -e 's/^\(.*\)/s\/MODEL_DRIVER_NAME_LC_STR\/\1\/g/'` \
               -e 's/MODEL_DRIVER_NAME_STR/$@/g'                                                              >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c

ex_model_Ar_P_Morse: model_Morse_README \
                     Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_Morse_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                     -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse/g' \
                                     -e 's/SPECIES_NAME_STR/Ar/g'                                             >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Morse_Makefile | sed -e 's/MODEL_NAME_STR/$@/g' \
                                       -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse/g' \
                                       -e 's/SPECIES_NAME_STR/Ar/g'                                           >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .params file in the directory
	echo "8.15"                                                                                           >  $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "-0.0134783698072604"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "1.545"                                                                                          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "3.786"                                                                                          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Parameters taken from: G. E. Jelinek, Phys. Rev. B, Vol 5, no 8, 1972, pp3210--3217"          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Release: This file is part of the openkim-api.git repository."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER START"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# The contents of this file are subject to the terms of the Common Development"                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# and Distribution License Version 1.0 (the "'"'"License"'"'")."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# You can obtain a copy of the license at"                                                      >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the"                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# specific language governing permissions and limitations under the License."                   >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# When distributing Covered Code, include this CDDL HEADER in each file and"                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# include the License file in a prominent location with the name LICENSE.CDDL."                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# If applicable, add the following below this CDDL HEADER, with the fields"                     >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# enclosed by brackets "'"'"[]"'"'" replaced with your own identifying information:"            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved."                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER END"                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Copyright (c) 2013, Regents of the University of Minnesota."                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params


ex_model_Ne_P_Morse: model_Morse_README \
                     Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_Morse_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                     -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse/g' \
                                     -e 's/SPECIES_NAME_STR/Ne/g'                                             >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Morse_Makefile | sed -e 's/MODEL_NAME_STR/$@/g' \
                                       -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse/g' \
                                       -e 's/SPECIES_NAME_STR/Ne/g'                                           >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .params file in the directory
	echo "8.15"                                                                                           >  $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "-0.00418802360867498"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "2.11"                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "3.0778"                                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Parameters taken from Table 2 of: H. R. Glyde, J. Phys. C: Solid State Phys.. Vol 3. 1970, pp810--819" \
                                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Release: This file is part of the openkim-api.git repository."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER START"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# The contents of this file are subject to the terms of the Common Development"                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# and Distribution License Version 1.0 (the "'"'"License"'"'")."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# You can obtain a copy of the license at"                                                      >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the"                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# specific language governing permissions and limitations under the License."                   >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# When distributing Covered Code, include this CDDL HEADER in each file and"                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# include the License file in a prominent location with the name LICENSE.CDDL."                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# If applicable, add the following below this CDDL HEADER, with the fields"                     >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# enclosed by brackets "'"'"[]"'"'" replaced with your own identifying information:"            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved."                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER END"                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Copyright (c) 2013, Regents of the University of Minnesota."                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params


ex_model_driver_P_LJ: model_driver_LJ_README \
                      model_driver_Makefile  \
                      model_driver_P_LJ.kim  \
                      model_driver_P_LJ.F90  \
                      Makefile
        # make directory
	mkdir -p $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	@touch $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODEL_DRIVERS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_driver_LJ_README | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g'                                 >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_driver_Makefile | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' -e 's/MODEL_DRIVER_LANG/Fortran/' >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_driver_P_LJ.kim                                                                          >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) model_driver_P_LJ.F90 | $(SED_CMD) \
         | sed -e `echo $@ | tr A-Z a-z | sed -e 's/^\(.*\)/s\/MODEL_DRIVER_NAME_LC_STR\/\1\/g/'` \
               -e 's/MODEL_DRIVER_NAME_STR/$@/g'                                                                >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.F90

ex_model_Ar_P_LJ: model_LJ_README \
                  Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_LJ_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                  -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_LJ/g' \
                                  -e 's/SPECIES_NAME_STR/Ar/g'                                                >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_LJ_Makefile | sed -e 's/MODEL_NAME_STR/$@/g' \
                                    -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_LJ/g' \
                                    -e 's/SPECIES_NAME_STR/Ar/g'                                              >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .params file in the directory
	echo "+8.150000000000000E+000"                                                                        >  $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "+1.040000000000000E-002"                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "+3.400000000000000E+000"                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Parameters taken from: E. B. Tadmor and R. E. Miller, 'Modeling Materials', Cambridge University Press, 2011" \
                                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Release: This file is part of the openkim-api.git repository."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER START"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# The contents of this file are subject to the terms of the Common Development"                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# and Distribution License Version 1.0 (the "'"'"License"'"'")."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# You can obtain a copy of the license at"                                                      >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the"                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# specific language governing permissions and limitations under the License."                   >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# When distributing Covered Code, include this CDDL HEADER in each file and"                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# include the License file in a prominent location with the name LICENSE.CDDL."                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# If applicable, add the following below this CDDL HEADER, with the fields"                     >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# enclosed by brackets "'"'"[]"'"'" replaced with your own identifying information:"            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved."                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER END"                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Copyright (c) 2013, Regents of the University of Minnesota."                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params


ex_model_Ne_P_LJ: model_LJ_README \
                  Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_LJ_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                  -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_LJ/g' \
                                  -e 's/SPECIES_NAME_STR/Ne/g'                                                >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_LJ_Makefile | sed -e 's/MODEL_NAME_STR/$@/g' \
                                    -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_LJ/g' \
                                    -e 's/SPECIES_NAME_STR/Ne/g'                                              >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .params file in the directory
	echo "+8.150000000000000E+000"                                                                        >  $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "+3.100000000000000E-003"                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "+2.740000000000000E+000"                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Parameters taken from: E. B. Tadmor and R. E. Miller, 'Modeling Materials', Cambridge University Press, 2011" \
                                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Release: This file is part of the openkim-api.git repository."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER START"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# The contents of this file are subject to the terms of the Common Development"                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# and Distribution License Version 1.0 (the "'"'"License"'"'")."                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# You can obtain a copy of the license at"                                                      >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the"                        >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# specific language governing permissions and limitations under the License."                   >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# When distributing Covered Code, include this CDDL HEADER in each file and"                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# include the License file in a prominent location with the name LICENSE.CDDL."                 >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# If applicable, add the following below this CDDL HEADER, with the fields"                     >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# enclosed by brackets "'"'"[]"'"'" replaced with your own identifying information:"            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved."                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# CDDL HEADER END"                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Copyright (c) 2013, Regents of the University of Minnesota."                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params


ex_model_Al_PF_ErcolessiAdams: dir_ex_model_Al_PF_ErcolessiAdams              \
                               $(wildcard dir_ex_model_Al_PF_ErcolessAdams/*) \
                               Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Cu_PF_Johnson: dir_ex_model_Cu_PF_Johnson                      \
                               $(wildcard dir_ex_model_Cu_PF_Johnson/*) \
                               Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_ArNe_P_MLJ_NEIGH_RVEC_F: dir_ex_model_ArNe_P_MLJ_NEIGH_RVEC_F               \
                                  $(wildcard dir_ex_model_ArNe_P_MLJ_NEIGH_RVEC_F/*) \
                                  Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Ar_P_MLJ_C: dir_ex_model_Ar_P_MLJ_C               \
                     $(wildcard dir_ex_model_Ar_P_MLJ_C/*) \
                     Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Ar_P_MLJ_F90: dir_ex_model_Ar_P_MLJ_F90               \
                       $(wildcard dir_ex_model_Ar_P_MLJ_F90/*) \
                       Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Ne_P_LJ_NEIGH_PURE_H: dir_ex_model_Ne_P_LJ_NEIGH_PURE_H               \
                               $(wildcard dir_ex_model_Ne_P_LJ_NEIGH_PURE_H/*) \
                               Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Ne_P_MLJ_NEIGH_RVEC_H: dir_ex_model_Ne_P_MLJ_NEIGH_RVEC_H               \
                                $(wildcard dir_ex_model_Ne_P_MLJ_NEIGH_RVEC_H/*) \
                                Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/

ex_model_Ne_P_fastLJ: dir_ex_model_Ne_P_fastLJ               \
                      $(wildcard dir_ex_model_Ne_P_fastLJ/*) \
                      Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_MODELS_DIR)/$@/


.PHONY: clean

clean:

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
# Copyright (c) 2013--2014, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


KIM_DIR := ../../..
KIM_TEMPLATES_DIR := $(KIM_DIR)/docs/templates
KIM_EX_MODELS_DIR := $(KIM_DIR)/examples/models
KIM_EX_MODEL_DRIVERS_DIR := $(KIM_DIR)/examples/model_drivers

include $(KIM_DIR)/Makefile.Version
# Clean up version variables
VERSION_MAJOR := $(strip $(VERSION_MAJOR))
VERSION_MINOR := $(strip $(VERSION_MINOR))
VERSION_PATCH := $(strip $(VERSION_PATCH))
VERSION_PRERELEASE := $(strip $(VERSION_PRERELEASE))
VERSION_BUILD_METADATA := $(strip $(VERSION_BUILD_METADATA))
VERSION_FULL := $(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)$(if $(VERSION_PRERELEASE),-$(VERSION_PRERELEASE))$(if $(VERSION_BUILD_METADATA),+$(VERSION_BUILD_METADATA))

VPATH = $(KIM_EX_MODELS_DIR) $(KIM_EX_MODEL_DRIVERS_DIR) $(KIM_TEMPLATES_DIR)


VER_REG_EXP =    \/\*..............\*\/
VER_REG_EXP_01 = \/\*\..............\*\/[[:space:]]*$$
VER_REG_EXP_02 = \/\*.\.............\*\/[[:space:]]*$$
VER_REG_EXP_03 = \/\*..\............\*\/[[:space:]]*$$
VER_REG_EXP_04 = \/\*...\...........\*\/[[:space:]]*$$
VER_REG_EXP_05 = \/\*....\..........\*\/[[:space:]]*$$
VER_REG_EXP_06 = \/\*.....\.........\*\/[[:space:]]*$$
VER_REG_EXP_07 = \/\*......\........\*\/[[:space:]]*$$
VER_REG_EXP_08 = \/\*.......\.......\*\/[[:space:]]*$$
VER_REG_EXP_09 = \/\*........\......\*\/[[:space:]]*$$
VER_REG_EXP_10 = \/\*.........\.....\*\/[[:space:]]*$$
VER_REG_EXP_11 = \/\*..........\....\*\/[[:space:]]*$$
VER_REG_EXP_12 = \/\*...........\...\*\/[[:space:]]*$$
VER_REG_EXP_13 = \/\*............\..\*\/[[:space:]]*$$
VER_REG_EXP_14 = \/\*.............\.\*\/[[:space:]]*$$

PARAMETER_SED_CMDS = \
            -e 's/<FILL_parameter_1>/epsilon/g' \
            -e 's/<FILL_parameter_2>/C/g' \
            -e 's/<FILL_parameter_3>/Rzero/g' \
            -e 's/<FILL_parameter_4>/shift/g' \
            -e 's/<FILL_number_of_parameters>/4/g' \
            -e 's/<FILL_number_of_parameters_plus_two>/6/g' \
            -e 's/<FILL_parameter_type_parameter_1>/PARAM_FREE_epsilon/g' \
            -e 's/<FILL_parameter_type_parameter_2>/PARAM_FREE_C/g' \
            -e 's/<FILL_parameter_type_parameter_3>/PARAM_FREE_Rzero/g' \
            -e 's/<FILL_parameter_type_parameter_4>/PARAM_FIXED_shift/g'

SED_VER_CMD = "s/VERSION_STR/$(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)/g"
SED_CMD = sed -e "s/MODEL_NAME_STR/$@/g" -e "s/MODEL_KIM_NAME_STR/${subst ex_,,$@}/g"

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

AUTO_GEN_MODELS = ex_model_Al_PF_ErcolessiAdams     \
                  ex_model_Cu_PF_Johnson            \
                  ex_model_ArNe_P_MLJ_NEIGH_RVEC_F  \
                  ex_model_Ar_P_MLJ_C               \
                  ex_model_Ar_P_MLJ_CLUSTER_C       \
                  ex_model_Ar_P_MLJ_CLUSTER_F03     \
                  ex_model_Ar_P_MLJ_F03             \
                  ex_model_Ar_P_MLJ_MI_OPBC_H_F     \
                  ex_model_Ar_P_MLJ_NEIGH_PURE_H_F  \
                  ex_model_Ar_P_MLJ_NEIGH_RVEC_F    \
                  ex_model_Ne_P_LJ_NEIGH_PURE_H     \
                  ex_model_Ne_P_MLJ_NEIGH_RVEC_H    \
                  ex_model_driver_P_Morse           \
                  ex_model_Ar_P_Morse               \
                  ex_model_Ne_P_Morse               \
                  ex_model_driver_P_LJ              \
                  ex_model_Ar_P_LJ                  \
                  ex_model_Ne_P_LJ                  \
                  ex_model_Ne_P_fastLJ              \
                  ex_model_Ar_P_Morse_01C           \
                  ex_model_Ar_P_Morse_02C           \
                  ex_model_Ar_P_Morse_03C           \
                  ex_model_Ar_P_Morse_04C           \
                  ex_model_Ar_P_Morse_05C           \
                  ex_model_Ar_P_Morse_06C           \
                  ex_model_Ar_P_Morse_07C           \
                  ex_model_Ar_P_Morse_08C           \
                  ex_model_Ar_P_Morse_09C           \
                  ex_model_Ar_P_Morse_10C           \
                  ex_model_Ar_P_Morse_11C           \
                  ex_model_Ar_P_Morse_12C           \
                  ex_model_driver_P_Morse_13C       \
                  ex_model_Ar_P_Morse_14C           \
                  model_driver_P_Template.c         \
                  model_driver_P_Template.c.kim.tpl \
                  model_El_P_Template.F03           \
                  model_El_P_Template.f.kim         \
                  model_El_P_Template.c             \
                  model_El_P_Template.c.kim         \
                  model_El_PF_Template.f.kim

all: $(AUTO_GEN_MODELS) EXAMPLES.README $(KIM_EX_MODELS_DIR)/Makefile $(KIM_EX_MODEL_DRIVERS_DIR)/Makefile

$(KIM_EX_MODELS_DIR)/Makefile: example_models_Makefile
	cp example_models_Makefile $(KIM_EX_MODELS_DIR)/Makefile

$(KIM_EX_MODEL_DRIVERS_DIR)/Makefile: example_model_drivers_Makefile
	cp example_model_drivers_Makefile $(KIM_EX_MODEL_DRIVERS_DIR)/Makefile

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
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
               -e $(SED_VER_CMD)                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
        # make .c file in the directory
	$(C_CAT) model_P_MLJ_CLUSTER_C.c | $(SED_CMD) \
            | sed -e 's/SPECIES_NAME_STR/Ar/g' \
            | sed -e 's/SPECIES_CODE_STR/1/g' \
            | sed -e 's/CUTOFF_VALUE_STR/8.15; \/* cutoff distance in angstroms *\//g' \
            | sed -e 's/SIGMA_VALUE_STR/3.40; \/* LJ Argon sigma in angstroms *\//g' \
            | sed -e 's/EPSILON_VALUE_STR/0.0104; \/* LJ Argon epsilon in eV *\//g' \
                                                                                                              > $(KIM_EX_MODELS_DIR)/$@/$@.c

ex_model_Ar_P_MLJ_CLUSTER_F03: $(COMMON_FRAGMENTS)                    \
                               cluster_compute_local_variables.f.frag \
                               cluster_compute_loops.f.frag
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_MLJ_README | $(SED_CMD) \
         | sed -e "s/NBC_NAME_STR/CLUSTER/g" \
               -e "s/LANGUAGE_STR/F03/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
               -e $(SED_VER_CMD)                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
        # make .F03 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) cluster_compute_local_variables.f.frag                                                       >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) cluster_compute_loops.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15_cd ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40_cd ! LJ Argon sigma in angstroms/g'    \
               -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104_cd ! LJ Argon epsilon in eV/g'                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03

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
               -e "s/LANGUAGE_STR/F03/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
               -e $(SED_VER_CMD)                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "numberContributingParticles integer      none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "get_neigh                   method       none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "boxSideLengths              double       length              [3]"                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
        # make .F03 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) mi_opbc_neigh_pure_compute_local_variables.f.frag                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) mi_opbc_compute_extra_kim_variables.f.frag                                                   >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) mi_opbc_compute_loops.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15_cd ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40_cd ! LJ Argon sigma in angstroms/g'    \
               -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104_cd ! LJ Argon epsilon in eV/g'                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03

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
               -e "s/LANGUAGE_STR/F03/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
                                  | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
                                        -e $(SED_VER_CMD)                                                     >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "numberContributingParticles integer      none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "get_neigh                   method       none                []                 optional"       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
        # make .F03 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) mi_opbc_neigh_pure_compute_local_variables.f.frag                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) neigh_pure_compute_extra_kim_variables.f.frag                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) neigh_pure_compute_loops.f.frag                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15_cd ! cutoff distance in angstroms/g' \
               -e 's/SIGMA_VALUE_STR/model_sigma = 3.40_cd ! LJ Argon sigma in angstroms/g'    \
                -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104_cd ! LJ Argon epsilon in eV/g'                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03

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
               -e "s/LANGUAGE_STR/F03/g"                                                                      >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    1/g' \
               -e $(SED_VER_CMD)                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "Neigh_LocaAccess            flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "get_neigh                   method       none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo "neighObject                 pointer      none                []"                                >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	echo                                                                                                  >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	$(D_CAT) descriptor_file_params                                                                       >> $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
        # make .F03 file in the directory
	$(F_CAT) module_start.f.frag | $(SED_CMD)                                                             >  $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) neigh_rvec_compute_local_variables.f.frag                                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_kim_variables.f.frag                                                                 >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) neigh_rvec_compute_extra_kim_variables.f.frag                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) compute_initialize.f.frag | sed -e 's/SPECIES_CODE_STR/1/g'                                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	echo "    "                                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) neigh_rvec_compute_loops.f.frag                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.F03
	$(F_CAT) module_end.f.frag | $(SED_CMD) \
         | sed -e 's/CUTOFF_VALUE_STR/model_cutoff = 8.15_cd ! cutoff distance in angstroms/g' \
         | sed -e 's/SIGMA_VALUE_STR/model_sigma = 3.40_cd ! LJ Argon sigma in angstroms/g'    \
         | sed -e 's/EPSILON_VALUE_STR/model_epsilon = 0.0104_cd ! LJ Argon epsilon in eV/g'                  >> $(KIM_EX_MODELS_DIR)/$@/$@.F03

model_driver_P_Template.c: model_driver_P_Morse.c \
                           Makefile
        # make .c file in the directory
	sed -e '/$(VER_REG_EXP_14)/d' \
            -e 's|[[:space:]]*$(VER_REG_EXP)[[:space:]]*$$||' \
               model_driver_P_Morse.c > $(KIM_TEMPLATES_DIR)/$@

model_driver_P_Template.c.kim.tpl: model_driver_P_Morse.kim.tpl \
                                   Makefile
	$(D_CAT) model_driver_P_Morse.kim.tpl | \
           sed -e $(SED_VER_CMD) \
               -e 's/^@//' \
           >  $(KIM_TEMPLATES_DIR)/$@

ex_model_driver_P_Morse_13C: model_driver_Morse_README    \
                             model_driver_Makefile        \
                             model_driver_P_Morse.kim.tpl \
                             model_driver_P_Morse.c       \
                             Makefile
        # make directory
	mkdir -p $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	@touch $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODEL_DRIVERS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_driver_Morse_README | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                                 -e 's/MODEL_DRIVER_KIM_NAME_STR/${subst ex_,,$@}/g'          >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_driver_Makefile | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                          -e 's/MODEL_DRIVER_KIM_FILE_TEMPLATE_STR/$(subst ex_,,$@).kim.tpl/' >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_driver_P_Morse.kim.tpl | \
           sed -e $(SED_VER_CMD)\
               -e 's/<FILL_parameter_type_parameter_1>/PARAM_FREE_epsilon         /g' \
               -e 's/<FILL_parameter_type_parameter_2>/PARAM_FREE_C               /g' \
               -e 's/<FILL_parameter_type_parameter_3>/PARAM_FREE_Rzero           /g' \
               -e 's/<FILL_parameter_type_parameter_4>/PARAM_FIXED_shift          /g' \
               -e 's/<FILL_parameter_1_unit>/energy          /g' \
               -e 's/<FILL_parameter_2_unit>/length\^-1       /g' \
               -e 's/<FILL_parameter_3_unit>/length          /g' \
               -e 's/<FILL_parameter_4_unit>/energy          /g' \
               >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl
	diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_12C/model_Ar_P_Morse_12C.kim \
              > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl.numbered; true
	diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_12C/model_Ar_P_Morse_12C.kim \
              | sed -e "s/^D.*/`printf '\x1b[31m'`&`printf '\x1b[0m'`/" -e "s/^A.*/`printf '\x1b[34m'`&`printf '\x1b[0m'`/" \
              > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl.colorized-numbered; true
        # make .c file in the directory
	sed -e '/$(VER_REG_EXP_13)/d' \
            -e 's|[[:space:]]*$(VER_REG_EXP)[[:space:]]*$$||' \
            -e 's/<FILL_model_driver_name>/$@/g' \
            $(PARAMETER_SED_CMDS) \
               model_driver_P_Morse.c > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c
	printf ',g/^[[:space:]]*[{}][[:space:]]*$$/s/^[[:space:]]*/ /\\\n-,.j\nw\nq\n' | ed $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c
	diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_12C/ex_model_Ar_P_Morse_12C.c \
              > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c.numbered; true
	diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_12C/ex_model_Ar_P_Morse_12C.c \
              | sed -e "s/^D.*/`printf '\x1b[31m'`&`printf '\x1b[0m'`/" -e "s/^A.*/`printf '\x1b[34m'`&`printf '\x1b[0m'`/" \
              > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c.colorized-numbered; true

ex_model_Ar_P_Morse_%C: model_driver_P_Morse.c      \
                        model_training_C_README     \
                        model_Makefile              \
                        model_training_C_descriptor \
                        Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_training_C_README | $(SED_CMD) \
           $(if $(filter-out 01,$*),,-e '/iterator/d' -e '/arguments/d' -e '/optional/d' -e "/\`virial'/d" -e '/process_/d' -e '/publish/d' -e '/flexible/d' -e '/Buffer/d' -e '/NEIGH_PURE_H/d' -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 02,$*),,-e '/iterator/d'                                    -e "/\`virial'/d" -e '/process_/d' -e '/publish/d' -e '/flexible/d' -e '/Buffer/d' -e '/NEIGH_PURE_H/d' -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 03,$*),,-e '/iterator/d'                                                      -e '/process_/d' -e '/publish/d' -e '/flexible/d' -e '/Buffer/d' -e '/NEIGH_PURE_H/d' -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 04,$*),,-e '/iterator/d'                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d' -e '/NEIGH_PURE_H/d' -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 05,$*),,-e '/iterator/d'                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d'                      -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 06,$*),,                                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d'                      -e '/NEIGH_PURE_F/d' -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 07,$*),,                                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d'                                           -e '/NEIGH_RVEC_H/d' -e '/NEIGH_RVEC_F/d' -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 08,$*),,                                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d'                                                                                     -e '/MI_OPBC_H/d' -e '/MI_OPBC_F/d') \
           $(if $(filter-out 09,$*),,                                                    -e "/\`virial'/d"                  -e '/publish/d' -e '/flexible/d' -e '/Buffer/d'                                                                                                                        ) \
           $(if $(filter-out 10,$*),,                                                    -e "/\`virial'/d"                                  -e '/flexible/d' -e '/Buffer/d'                                                                                                                        ) \
           $(if $(filter-out 11,$*),,                                                    -e "/\`virial'/d"                                                   -e '/Buffer/d'                                                                                                                        ) \
           $(if $(filter-out 12,$*),,                                                    -e "/\`virial'/d"                                                                                                                                                                                         ) \
                                                                                                              >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Makefile | $(SED_CMD) | sed -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/'            >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_training_C_descriptor \
         | sed -e $(SED_VER_CMD) \
            $(if $(filter-out 01,$*),,-e '/flexible/d'               -e "/IterAccess/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_H/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`" -e "/^numberContributing/`printf '{N\nd\n}'`" -e "/^boxSide/`printf '{N\nd\n}'`" -e "/^get_neigh/`printf '{N\nd\n}'`" -e "/^neighObject/`printf '{N\nd\n}'`" -e "/^process_/`printf '{N\nd\n}'`"                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`" -e "/^energy.*optional/`printf '{N\nd\n}'`"                                        -e "/^force/`printf '{N\nd\n}'`" -e "/^particleEnergy/`printf '{N\nd\n}'`" -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 02,$*),,-e '/flexible/d'               -e "/IterAccess/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_H/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`" -e "/^numberContributing/`printf '{N\nd\n}'`" -e "/^boxSide/`printf '{N\nd\n}'`" -e "/^get_neigh/`printf '{N\nd\n}'`" -e "/^neighObject/`printf '{N\nd\n}'`" -e "/^process_/`printf '{N\nd\n}'`"                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 03,$*),,-e '/flexible/d'               -e "/IterAccess/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_H/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`" -e "/^numberContributing/`printf '{N\nd\n}'`" -e "/^boxSide/`printf '{N\nd\n}'`" -e "/^get_neigh/`printf '{N\nd\n}'`" -e "/^neighObject/`printf '{N\nd\n}'`" -e "/^process_/`printf '{N\nd\n}'`"                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                                                                                                                                           -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 04,$*),,-e '/flexible/d'               -e "/IterAccess/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_H/`printf '{N\nd\n}'`" -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`" -e "/^numberContributing/`printf '{N\nd\n}'`" -e "/^boxSide/`printf '{N\nd\n}'`" -e "/^get_neigh/`printf '{N\nd\n}'`" -e "/^neighObject/`printf '{N\nd\n}'`"                                                                                  -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 05,$*),,-e '/flexible/d'               -e "/IterAccess/`printf '{N\nd\n}'`"                                         -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`"                                               -e "/^boxSide/`printf '{N\nd\n}'`"                                                                                                                                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 06,$*),,-e '/flexible/d'                                                                                            -e "/^NEIGH_PURE_F/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`"                                               -e "/^boxSide/`printf '{N\nd\n}'`"                                                                                                                                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 07,$*),,-e '/flexible/d'                                                                                                                                    -e "/^NEIGH_RVEC_H/`printf '{N\nd\n}'`" -e "/^NEIGH_RVEC_F/`printf '{N\nd\n}'`" -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`"                                               -e "/^boxSide/`printf '{N\nd\n}'`"                                                                                                                                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 08,$*),,-e '/flexible/d'                                                                                                                                                                                                                    -e "/^MI_OPBC_H/`printf '{N\nd\n}'`" -e "/^MI_OPBC_F/`printf '{N\nd\n}'`"                                               -e "/^boxSide/`printf '{N\nd\n}'`"                                                                                                                                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 09,$*),,-e '/flexible/d'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`" -e "/^PARAM_/`printf '{N\nd\n}'`")\
            $(if $(filter-out 10,$*),,-e '/flexible/d'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`"                                  )\
            $(if $(filter-out 11,$*),,                 -e '/fixed/d'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              -e "/^destroy.*]$$/`printf '{N\nd\n}'`"                                             -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`"                                  )\
            $(if $(filter-out 12,$*),,                 -e '/fixed/d'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 -e "/^destroy.*optional/`printf '{N\nd\n}'`"                                                                                     -e "/^energy.*]$$/`printf '{N\nd\n}'`"                                                                            -e "/^virial/`printf '{N\nd\n}'`" -e "/^particleVirial/`printf '{N\nd\n}'`" -e "/^hessian/`printf '{N\nd\n}'`"                                  )\
                                                                                                              >  $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim
	if test $* -gt 1; then \
         idminus=`expr "$*" - "1"`; \
         diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C/model_Ar_P_Morse_`printf "%02i" $$idminus`C.kim \
              > $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim.numbered 2>&1; \
         diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C/model_Ar_P_Morse_`printf "%02i" $$idminus`C.kim \
              | sed -e "s/^D.*/`printf '\x1b[31m'`&`printf '\x1b[0m'`/" -e "s/^A.*/`printf '\x1b[34m'`&`printf '\x1b[0m'`/" \
              > $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim.colorized-numbered 2>&1; \
              true; \
        fi;
        # make .c file in the directory
	sed -e '/$(VER_REG_EXP_$*)/d' \
            -e 's|[[:space:]]*$(VER_REG_EXP)[[:space:]]*$$||' \
            -e 's/<FILL_model_driver_name>/$@ /g' \
            $(PARAMETER_SED_CMDS) \
               model_driver_P_Morse.c > $(KIM_EX_MODELS_DIR)/$@/$@.c
	printf ',g/^[[:space:]]*[{}][[:space:]]*$$/s/^[[:space:]]*/ /\\\n-,.j\nw\nq\n' | ed $(KIM_EX_MODELS_DIR)/$@/$@.c
	if test $* -gt 1; then \
         idminus=`expr "$*" - "1"`; \
         diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODELS_DIR)/$@/$@.c \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C.c \
              > $(KIM_EX_MODELS_DIR)/$@/$@.c.numbered 2>&1; \
         diff --unchanged-line-format=" %4dn %L" --old-line-format="%c'A'%4dn %L" --new-line-format="D     %L" -d \
              $(KIM_EX_MODELS_DIR)/$@/$@.c \
              $(KIM_EX_MODELS_DIR)/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C/ex_model_Ar_P_Morse_`printf "%02i" $$idminus`C.c \
              | sed -e "s/^D.*/`printf '\x1b[31m'`&`printf '\x1b[0m'`/" -e "s/^A.*/`printf '\x1b[34m'`&`printf '\x1b[0m'`/" \
              > $(KIM_EX_MODELS_DIR)/$@/$@.c.colorized-numbered 2>&1; \
              true; \
        fi;

ex_model_Ar_P_Morse_14C: model_Morse_README \
                         Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_Morse_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                     -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse_13C/g' \
                                     -e 's/SPECIES_NAME_STR/Ar/g'                                             >  $(KIM_EX_MODELS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_Morse_Makefile | sed -e 's/MODEL_NAME_STR/$@/g' \
                                       -e 's/MODEL_DRIVER_NAME_STR/ex_model_driver_P_Morse_13C/g' \
                                       -e 's/SPECIES_NAME_STR/Ar/g'                                           >  $(KIM_EX_MODELS_DIR)/$@/Makefile
        # make .params file in the directory
	echo "8.15"                                                                                           >  $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "-0.0134783698072604"                                                                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "1.545"                                                                                          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "3.786"                                                                                          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo ""                                                                                               >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Parameters taken from: G. E. Jelinek, Phys. Rev. B, Vol 5, no 8, 1972, pp3210--3217"          >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Release: This file is part of the kim-api.git repository."                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Copyright (c) 2013--2014, Regents of the University of Minnesota."                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params

ex_model_driver_P_Morse: model_driver_Morse_README    \
                         model_driver_Makefile        \
                         model_driver_P_Morse.kim.tpl \
                         model_driver_P_Morse.c       \
                         Makefile
        # make directory
	mkdir -p $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	@touch $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODEL_DRIVERS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_driver_Morse_README | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                                 -e 's/MODEL_DRIVER_KIM_NAME_STR/${subst ex_,,$@}/g'          >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_driver_Makefile | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                          -e 's/MODEL_DRIVER_KIM_FILE_TEMPLATE_STR/$(subst ex_,,$@).kim.tpl/' >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_driver_P_Morse.kim.tpl | \
           sed -e '/^@/d' -e $(SED_VER_CMD) \
               -e 's/<FILL_parameter_type_parameter_1>/PARAM_FREE_epsilon         /g' \
               -e 's/<FILL_parameter_type_parameter_2>/PARAM_FREE_C               /g' \
               -e 's/<FILL_parameter_type_parameter_3>/PARAM_FREE_Rzero           /g' \
               -e 's/<FILL_parameter_type_parameter_4>/PARAM_FIXED_shift          /g' \
               -e 's/<FILL_parameter_1_unit>/energy          /g' \
               -e 's/<FILL_parameter_2_unit>/length\^-1       /g' \
               -e 's/<FILL_parameter_3_unit>/length          /g' \
               -e 's/<FILL_parameter_4_unit>/energy          /g' \
               >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl
        # make .c file in the directory
	sed -e '/$(VER_REG_EXP_13)/d' \
            -e 's|[[:space:]]*$(VER_REG_EXP)[[:space:]]*$$||' \
            -e 's/<FILL_model_driver_name>/$@/g' \
            $(PARAMETER_SED_CMDS) \
               model_driver_P_Morse.c > $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.c

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
	echo "# Release: This file is part of the kim-api.git repository."                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Copyright (c) 2013--2014, Regents of the University of Minnesota."                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Release: This file is part of the kim-api.git repository."                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Copyright (c) 2013--2014, Regents of the University of Minnesota."                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# All rights reserved."                                                                         >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "# Contributors:"                                                                                >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ryan S. Elliott"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#    Ellad B. Tadmor"                                                                           >> $(KIM_EX_MODELS_DIR)/$@/$@.params
	echo "#"                                                                                              >> $(KIM_EX_MODELS_DIR)/$@/$@.params


ex_model_driver_P_LJ: model_driver_LJ_README    \
                      model_driver_Makefile     \
                      model_driver_P_LJ.kim.tpl \
                      model_driver_P_LJ.F03     \
                      Makefile
        # make directory
	mkdir -p $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	@touch $(KIM_EX_MODEL_DRIVERS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODEL_DRIVERS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_driver_LJ_README | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                              -e 's/MODEL_DRIVER_KIM_NAME_STR/${subst ex_,,$@}/g'               >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) model_driver_Makefile | sed -e 's/MODEL_DRIVER_NAME_STR/$@/g' \
                                          -e 's/MODEL_DRIVER_KIM_FILE_TEMPLATE_STR/$(subst ex_,,$@).kim.tpl/'   >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) model_driver_P_LJ.kim.tpl | sed -e $(SED_VER_CMD)                                              >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$(subst ex_,,$@).kim.tpl
        # make .F03 file in the directory
	$(F_CAT) model_driver_P_LJ.F03 | $(SED_CMD) \
         | sed -e `echo $@ | tr A-Z a-z | sed -e 's/^\(.*\)/s\/MODEL_DRIVER_NAME_LC_STR\/\1\/g/'` \
               -e 's/MODEL_DRIVER_NAME_STR/$@/g'                                                                >  $(KIM_EX_MODEL_DRIVERS_DIR)/$@/$@.F03

ex_model_Ar_P_LJ: model_LJ_README \
                  Makefile
        # make directory
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	@touch $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
        # make README file in the directory
	$(D_CAT) model_LJ_README | sed -e 's/MODEL_NAME_STR/$@/g' \
                                  -e 's/MODEL_KIM_NAME_STR/${subst ex_,,$@}/g' \
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
	echo "# Release: This file is part of the kim-api.git repository."                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Copyright (c) 2013--2014, Regents of the University of Minnesota."                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
                                  -e 's/MODEL_KIM_NAME_STR/${subst ex_,,$@}/g' \
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
	echo "# Release: This file is part of the kim-api.git repository."                                    >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	echo "# Copyright (c) 2013--2014, Regents of the University of Minnesota."                            >> $(KIM_EX_MODELS_DIR)/$@/$@.params
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
	cp dir_$@/Makefile $(KIM_EX_MODELS_DIR)/$@/Makefile
	cp dir_$@/$@.F03 $(KIM_EX_MODELS_DIR)/$@/$@.F03
	cp dir_$@/README $(KIM_EX_MODELS_DIR)/$@/README
	sed -e $(SED_VER_CMD) \
            -e '/^@/d' \
            dir_$@/$(subst ex_,,$@).kim > $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim

model_El_PF_Template.f.kim: dir_ex_model_Al_PF_ErcolessiAdams/model_Al_PF_ErcolessiAdams.kim \
                            Makefile
	sed -e $(SED_VER_CMD) \
            -e 's/^@//g' \
            -e '/^PARAM/d' \
            dir_ex_model_Al_PF_ErcolessiAdams/model_Al_PF_ErcolessiAdams.kim > $(KIM_TEMPLATES_DIR)/$@

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

ex_model_Ar_P_MLJ_C: dir_model_El_P_MLJ/README             \
                     dir_model_El_P_MLJ/model_El_P_MLJ.c   \
                     dir_model_El_P_MLJ/model_El_P_MLJ.kim \
                     model_Makefile                        \
                     Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	sed -e 's/MODEL_NAME_STR/$@/g' \
            -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/g' model_Makefile > $(KIM_EX_MODELS_DIR)/$@/Makefile
	sed -e 's/MODEL_NAME_STR/$@/g' \
            -e 's/LANGUAGE_STR/c/g' \
            -e 's/MODEL_KIM_NAME_STR/$(subst ex_,,$@)/g' dir_model_El_P_MLJ/README > $(KIM_EX_MODELS_DIR)/$@/README
	sed -e '/\/\*\..\*\/$$/d' \
            -e 's|[[:space:]]*/\*..\*/$$||' \
            -e 's/<FILL_model_name>  /$@/g' \
            -e 's/<FILL_model_reference>/Ashcroft and Mermin   /g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.c > $(KIM_EX_MODELS_DIR)/$@/$@.c
	sed -e $(SED_VER_CMD) \
            -e '/^@/d' \
            -e 's/<FILL_species_name>/Ar                 /g' \
            -e 's/BASED_LIST_STR/ZeroBasedLists/g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.kim > $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim

ex_model_Ar_P_MLJ_F03: dir_model_El_P_MLJ/README             \
                       dir_model_El_P_MLJ/model_El_P_MLJ.F03 \
                       dir_model_El_P_MLJ/model_El_P_MLJ.kim \
                       model_Makefile                        \
                       Makefile
	mkdir -p $(KIM_EX_MODELS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_MODELS_DIR)/$@
	sed -e 's/MODEL_NAME_STR/$@/g' \
            -e 's/MODEL_KIM_FILE_STR/$(subst ex_,,$@).kim/g' model_Makefile > $(KIM_EX_MODELS_DIR)/$@/Makefile
	sed -e 's/MODEL_NAME_STR/$@/g' \
            -e 's/LANGUAGE_STR/F03/g' \
            -e 's/MODEL_KIM_NAME_STR/$(subst ex_,,$@)/g' dir_model_El_P_MLJ/README > $(KIM_EX_MODELS_DIR)/$@/README
	sed -e '/\!\..$$/d' \
            -e 's|[[:space:]]*\!..$$||' \
            -e 's/<FILL_model_name>/$@/g' \
            -e 's/<FILL_model_reference>/Ashcroft and Mermin/g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.F03 > $(KIM_EX_MODELS_DIR)/$@/$@.F03
	sed -e $(SED_VER_CMD) \
            -e '/^@/d' \
            -e 's/<FILL_species_name>/Ar                 /g' \
            -e 's/BASED_LIST_STR/OneBasedLists /g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.kim > $(KIM_EX_MODELS_DIR)/$@/$(subst ex_,,$@).kim

model_El_P_Template.c: dir_model_El_P_MLJ/model_El_P_MLJ.c \
                       Makefile
	sed -e '/\/\*.\.\*\/$$/d' \
            -e 's|[[:space:]]*/\*..\*/$$||' \
            dir_model_El_P_MLJ/model_El_P_MLJ.c > $(KIM_TEMPLATES_DIR)/$@

model_El_P_Template.c.kim: dir_model_El_P_MLJ/model_El_P_MLJ.kim \
                           Makefile
	sed -e $(SED_VER_CMD) \
            -e 's/^@//' \
            -e 's/BASED_LIST_STR/ZeroBasedLists/g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.kim > $(KIM_TEMPLATES_DIR)/model_El_P_Template.c.kim


model_El_P_Template.F03: dir_model_El_P_MLJ/model_El_P_MLJ.F03 \
                         Makefile
	sed -e '/!.\.$$/d' \
            -e 's|[[:space:]]*!..$$||' \
            dir_model_El_P_MLJ/model_El_P_MLJ.F03 > $(KIM_TEMPLATES_DIR)/$@

model_El_P_Template.f.kim: dir_model_El_P_MLJ/model_El_P_MLJ.kim \
                           Makefile
	sed -e $(SED_VER_CMD) \
            -e 's/^@//' \
            -e 's/BASED_LIST_STR/OneBasedLists /g' \
            dir_model_El_P_MLJ/model_El_P_MLJ.kim > $(KIM_TEMPLATES_DIR)/model_El_P_Template.f.kim


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

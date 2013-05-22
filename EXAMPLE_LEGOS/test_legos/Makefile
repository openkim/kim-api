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
#    Stephen M. Whalen
#

#
# Release: This file is part of the openkim-api.git repository.
#


KIM_DIR := ../..
KIM_EX_TESTS_DIR := $(KIM_DIR)/EXAMPLEs/TESTs

VPATH = $(KIM_EX_TESTS_DIR)

SED_CMD = sed -e "s/TEST_NAME_STR/$@/g"

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

COMMON_FRAGMENTS = test_Makefile          \
                   descriptor_file_start  \
                   descriptor_file_input  \
                   descriptor_file_output \
                   Makefile

AUTO_GEN_TESTS = ex_test_Al_free_cluster                        \
                 ex_test_Al_FCCcohesive_MI_OPBC                 \
                 ex_test_ArNe_B2cohesive_NEIGH_RVEC             \
                 ex_test_Ar_FCCcohesiveCutoff_NEIGH_RVEC        \
                 ex_test_Ar_FCCcohesive_MI_OPBC                 \
                 ex_test_Ar_FCCcohesive_NEIGH_PURE              \
                 ex_test_Ar_FCCcohesive_NEIGH_RVEC              \
                 ex_test_Ar_free_cluster_CLUSTER_C              \
                 ex_test_Ar_free_cluster_CLUSTER_F90            \
                 ex_test_Ar_free_cluster_CLUSTER_memory_F90     \
                 ex_test_Ar_free_cluster                        \
                 ex_test_Ar_multiple_models                     \
                 ex_test_Cu_free_cluster                        \
                 ex_test_Ne_free_cluster                        \
                 ex_test_Ne_free_cluster_SI                     \
                 ex_test_Ne_free_cluster_stiff                  \
                 vc_config_ener_forces                          \
                 vc_forces_delta                                \
                 vc_forces_numer_deriv


all: $(AUTO_GEN_TESTS) EXAMPLES.README run_all_ex_tests run_forces_numer_deriv_for_all_models

EXAMPLES.README: test_EXAMPLES.README Makefile
	cp test_EXAMPLES.README $(KIM_EX_TESTS_DIR)/EXAMPLES.README

run_all_ex_tests: test_run_all_ex_tests
	cp test_run_all_ex_tests $(KIM_EX_TESTS_DIR)/run_all_ex_tests

run_forces_numer_deriv_for_all_models: test_run_forces_numer_deriv_for_all_models
	cp test_run_forces_numer_deriv_for_all_models $(KIM_EX_TESTS_DIR)/run_forces_numer_deriv_for_all_models

ex_test_Al_free_cluster: $(COMMON_FRAGMENTS)                    \
                         test_free_cluster_README               \
                         descriptor_file_neighborlist           \
                         program_free_cluster.f.frag            \
                         MI_OPBC_cluster_neighborlist.f.frag    \
                         NEIGH_PURE_cluster_neighborlist.f.frag \
                         NEIGH_RVEC_cluster_neighborlist.f.frag \
                         get_neigh_no_Rij.f.frag                \
                         get_neigh_Rij.f.frag                   \
                         create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
          | sed -e 's/LANGUAGE_STR/F90/g' \
          | sed -e 's/SPECIES_NAME_STR/Al/g'                                                                  >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Al                          spec                    13/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Al/g' \
               -e 's/FCC_SPACING_STR/4.050d0 ! in angstroms/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g'                                             >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Al_FCCcohesive_MI_OPBC: $(COMMON_FRAGMENTS)                          \
                                test_FCCcohesive_README                      \
                                descriptor_file_neighborlist                 \
                                program_FCCcohesive_MI_OPBC.f.frag           \
                                get_model_cutoff_firsttime.f.frag            \
                                setup_KIM_API_object.f.frag                  \
                                setup_neighborlist_no_Rij_KIM_access.f.frag  \
                                MI_OPBC_periodic_neighborlist.f.frag         \
                                get_neigh_no_Rij.f.frag                      \
                                free_KIM_API_object.f.frag                   \
                                create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_FCCcohesive_README  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Al/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Al                          spec                    13/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_FCCcohesive_MI_OPBC.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Al/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
               -e 's/FCC_SPACING_STR/4.050d0 ! in angstroms/g' \
                                                                                                              >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_cutoff_firsttime.f.frag                                                            >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_KIM_API_object.f.frag                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_no_Rij_KIM_access.f.frag                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_periodic_neighborlist.f.frag                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_ArNe_B2cohesive_NEIGH_RVEC: $(COMMON_FRAGMENTS)                        \
                                    test_B2cohesive_README                     \
                                    descriptor_file_neighborlist               \
                                    program_B2cohesive_NEIGH_RVEC.f.frag       \
                                    setup_B2_KIM_API_object.f.frag             \
                                    setup_neighborlist_Rij_KIM_access.f.frag   \
                                    NEIGH_RVEC_periodic_B2_neighborlist.f.frag \
                                    get_neigh_Rij.f.frag                       \
                                    free_KIM_API_object.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_B2cohesive_README  | $(SED_CMD) \
         | sed -e 's/SPECIES1_NAME_STR/Ar/g' \
         | sed -e 's/SPECIES2_NAME_STR/Ne/g'                                                                  >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18\nNe                      spec                    10/g' \
                                                                                                              >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_B2cohesive_NEIGH_RVEC.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES1_NAME_STR/Ar/g' \
               -e 's/SPECIES2_NAME_STR/Ne/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
               -e 's/B2_SPACING_STR/5.260d0 ! in angstroms/g' \
                                                                                                              >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_B2_KIM_API_object.f.frag                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_Rij_KIM_access.f.frag                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_periodic_B2_neighborlist.f.frag                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_FCCcohesiveCutoff_NEIGH_RVEC: $(COMMON_FRAGMENTS)                           \
                                         test_FCCcohesiveCutoff_README                 \
                                         descriptor_file_neighborlist                  \
                                         program_FCCcohesiveCutoff_NEIGH_RVEC.f.frag   \
                                         NEIGH_RVEC_compute_equilibrium_spacing.f.frag \
                                         setup_KIM_API_object.f.frag                   \
                                         setup_neighborlist_Rij_KIM_access.f.frag      \
                                         NEIGH_RVEC_periodic_FCC_neighborlist.f.frag   \
                                         get_neigh_Rij.f.frag                          \
                                         free_KIM_API_object.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_FCCcohesiveCutoff_README  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_FCCcohesiveCutoff_NEIGH_RVEC.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
               -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_compute_equilibrium_spacing.f.frag | sed -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
                                                                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_KIM_API_object.f.frag                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_Rij_KIM_access.f.frag                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_periodic_FCC_neighborlist.f.frag                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_FCCcohesive_MI_OPBC: $(COMMON_FRAGMENTS)                          \
                                test_FCCcohesive_README                      \
                                descriptor_file_neighborlist                 \
                                program_FCCcohesive_MI_OPBC.f.frag           \
                                get_model_cutoff_firsttime.f.frag            \
                                setup_KIM_API_object.f.frag                  \
                                setup_neighborlist_no_Rij_KIM_access.f.frag  \
                                MI_OPBC_periodic_neighborlist.f.frag         \
                                get_neigh_no_Rij.f.frag                      \
                                free_KIM_API_object.f.frag                   \
                                create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_FCCcohesive_README  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_FCCcohesive_MI_OPBC.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
               -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_cutoff_firsttime.f.frag                                                            >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_KIM_API_object.f.frag                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_no_Rij_KIM_access.f.frag                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_periodic_neighborlist.f.frag                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_FCCcohesive_NEIGH_PURE: $(COMMON_FRAGMENTS)                          \
                                   test_FCCcohesive_README                      \
                                   descriptor_file_neighborlist                 \
                                   program_FCCcohesive_NEIGH_PURE.f.frag        \
                                   get_model_cutoff_firsttime.f.frag            \
                                   setup_KIM_API_object.f.frag                  \
                                   setup_neighborlist_no_Rij_KIM_access.f.frag  \
                                   NEIGH_PURE_periodic_neighborlist.f.frag      \
                                   get_neigh_no_Rij.f.frag                      \
                                   free_KIM_API_object.f.frag                   \
                                   create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_FCCcohesive_README  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_FCCcohesive_NEIGH_PURE.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
               -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_cutoff_firsttime.f.frag                                                            >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_KIM_API_object.f.frag                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_no_Rij_KIM_access.f.frag                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_periodic_neighborlist.f.frag                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_FCCcohesive_NEIGH_RVEC: $(COMMON_FRAGMENTS)                           \
                                   test_FCCcohesive_README                       \
                                   descriptor_file_neighborlist                  \
                                   program_FCCcohesive_NEIGH_RVEC.f.frag         \
                                   NEIGH_RVEC_compute_equilibrium_spacing.f.frag \
                                   setup_KIM_API_object.f.frag                   \
                                   setup_neighborlist_Rij_KIM_access.f.frag      \
                                   NEIGH_RVEC_periodic_FCC_neighborlist.f.frag   \
                                   get_neigh_Rij.f.frag                          \
                                   free_KIM_API_object.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_FCCcohesive_README  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "reinit                      method       none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_FCCcohesive_NEIGH_RVEC.f.frag  | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
               -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_compute_equilibrium_spacing.f.frag | sed -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g' \
                                                                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_KIM_API_object.f.frag                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) setup_neighborlist_Rij_KIM_access.f.frag                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) free_KIM_API_object.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_periodic_FCC_neighborlist.f.frag                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_free_cluster_CLUSTER_C: $(COMMON_FRAGMENTS)                 \
                                   test_free_cluster_README            \
                                   program_free_cluster_CLUSTER.c.frag \
                                   create_FCC_configuration.c.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/c/g' \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/OneBasedLists /ZeroBasedLists/g' \
               -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .c file in the directory
	$(C_CAT) program_free_cluster_CLUSTER.c.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
         | sed -e 's/FCC_SPACING_STR/5.26; \/* in angstroms *\//g'                                            >  $(KIM_EX_TESTS_DIR)/$@/$@.c
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.c
	$(C_CAT) create_FCC_configuration.c.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.c

ex_test_Ar_free_cluster_CLUSTER_F90: $(COMMON_FRAGMENTS)                 \
                                     test_free_cluster_README            \
                                     program_free_cluster_CLUSTER.f.frag \
                                     create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster_CLUSTER.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
         | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_free_cluster_CLUSTER_memory_F90: $(COMMON_FRAGMENTS)                        \
                                            test_free_cluster_README                   \
                                            program_free_cluster_CLUSTER_memory.f.frag \
                                            create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster_CLUSTER_memory.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
         | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g'                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ar_free_cluster: $(COMMON_FRAGMENTS)                    \
                         test_free_cluster_README               \
                         descriptor_file_neighborlist           \
                         program_free_cluster.f.frag            \
                         MI_OPBC_cluster_neighborlist.f.frag    \
                         NEIGH_PURE_cluster_neighborlist.f.frag \
                         NEIGH_RVEC_cluster_neighborlist.f.frag \
                         get_neigh_no_Rij.f.frag                \
                         get_neigh_Rij.f.frag                   \
                         create_FCC_configuration.f.frag        \
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Ar/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ar                          spec                    18/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ar/g' \
               -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g'                                             >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Cu_free_cluster: $(COMMON_FRAGMENTS)                    \
                         test_free_cluster_README               \
                         descriptor_file_neighborlist           \
                         program_free_cluster.f.frag            \
                         MI_OPBC_cluster_neighborlist.f.frag    \
                         NEIGH_PURE_cluster_neighborlist.f.frag \
                         NEIGH_RVEC_cluster_neighborlist.f.frag \
                         get_neigh_no_Rij.f.frag                \
                         get_neigh_Rij.f.frag                   \
                         create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Cu/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Cu                          spec                    29/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Cu/g' \
               -e 's/FCC_SPACING_STR/3.610d0 ! in angstroms/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g'                                             >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ne_free_cluster: $(COMMON_FRAGMENTS)                    \
                         test_free_cluster_README               \
                         descriptor_file_neighborlist           \
                         program_free_cluster.f.frag            \
                         MI_OPBC_cluster_neighborlist.f.frag    \
                         NEIGH_PURE_cluster_neighborlist.f.frag \
                         NEIGH_RVEC_cluster_neighborlist.f.frag \
                         get_neigh_no_Rij.f.frag                \
                         get_neigh_Rij.f.frag                   \
                         create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Ne/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ne                          spec                    10/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ne/g' \
               -e 's/FCC_SPACING_STR/4.430d0 ! in angstroms/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g'                                             >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ne_free_cluster_SI: $(COMMON_FRAGMENTS)                    \
                            test_free_cluster_README               \
                            descriptor_file_neighborlist           \
                            program_free_cluster.f.frag            \
                            MI_OPBC_cluster_neighborlist.f.frag    \
                            NEIGH_PURE_cluster_neighborlist.f.frag \
                            NEIGH_RVEC_cluster_neighborlist.f.frag \
                            get_neigh_no_Rij.f.frag                \
                            get_neigh_Rij.f.frag                   \
                            create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_README | $(SED_CMD) \
         | sed -e 's/LANGUAGE_STR/F90/g' \
         | sed -e 's/SPECIES_NAME_STR/Ne/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start_SI \
         | sed -e 's/SPECIES_LINE_STR/Ne                          spec                    10/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ne/g' \
               -e 's/FCC_SPACING_STR/4.430d-10 ! in meters/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d-10 ! in meters/g'                                              >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

ex_test_Ne_free_cluster_stiff: $(COMMON_FRAGMENTS)                    \
                               test_free_cluster_stiff_README         \
                               descriptor_file_neighborlist           \
                               program_free_cluster_stiff.f.frag      \
                               MI_OPBC_cluster_neighborlist.f.frag    \
                               NEIGH_PURE_cluster_neighborlist.f.frag \
                               NEIGH_RVEC_cluster_neighborlist.f.frag \
                               get_neigh_no_Rij.f.frag                \
                               get_neigh_Rij.f.frag                   \
                               create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) test_free_cluster_stiff_README | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ne/g'                                                                   >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .kim file in the directory
	$(D_CAT) descriptor_file_start \
         | sed -e 's/SPECIES_LINE_STR/Ne                          spec                    10/g'               >  $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_H                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_RVEC_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "NEIGH_PURE_F                flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_H                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "MI_OPBC_F                   flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "CLUSTER                     flag"                                                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_input                                                                        >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_neighborlist                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "numberContributingParticles integer      none                []"                                >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "boxSideLengths              real*8       length              [3]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	$(D_CAT) descriptor_file_output                                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "forces                      real*8       force               [numberOfParticles,3]"             >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "hessian                     real*8       pressure            [numberOfParticles,numberOfParticles,3,3]" \
                                                                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
	echo "virial                      real*8       energy              [6]"                               >> $(KIM_EX_TESTS_DIR)/$@/$@.kim
        # make .F90 file in the directory
	$(F_CAT) program_free_cluster_stiff.f.frag | $(SED_CMD) \
         | sed -e 's/SPECIES_NAME_STR/Ne/g' \
               -e 's/FCC_SPACING_STR/4.430d0 ! in angstroms/g' \
               -e 's/CUTOFF_PADDING_STR/0.750d0 ! in angstroms/g'                                             >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90


ex_test_Ar_multiple_models: dir_ex_test_Ar_multiple_models               \
                            $(wildcard dir_ex_test_Ar_multiple_models/*) \
                            Makefile
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_TESTS_DIR)/$@/

ex_test_demo_F03: dir_ex_test_demo_F03               \
                  $(wildcard dir_ex_test_demo_F03/*) \
                  Makefile
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
	cp -rf dir_$@/* $(KIM_EX_TESTS_DIR)/$@/

vc_forces_delta: Makefile                               \
                 test_Makefile                          \
                 vc_forces_delta_README                 \
                 program_vc_forces_delta.f.frag         \
                 get_model_supported_types.f.frag       \
                 get_model_NBC_methods.f.frag           \
                 write_KIM_descriptor.f.frag            \
                 update_neighborlist.f.frag             \
                 MI_OPBC_cluster_neighborlist.f.frag    \
                 NEIGH_PURE_cluster_neighborlist.f.frag \
                 NEIGH_RVEC_cluster_neighborlist.f.frag \
                 NEIGH_RVEC_update_Rij_vectors.f.frag   \
                 get_neigh_no_Rij.f.frag                \
                 get_neigh_Rij.f.frag                   \
                 create_FCC_configuration.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) vc_forces_delta_README | $(SED_CMD)                                                          >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .F90 file in the directory
	$(F_CAT) program_vc_forces_delta.f.frag  | $(SED_CMD)                                                 >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_supported_types.f.frag                                                             >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_NBC_methods.f.frag                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) write_KIM_descriptor.f.frag | $(SED_CMD)                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) update_neighborlist.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_update_Rij_vectors.f.frag                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

vc_forces_numer_deriv: Makefile                               \
                       test_Makefile                          \
                       vc_forces_numer_deriv_README           \
                       program_vc_forces_numer_deriv.f.frag   \
                       get_model_supported_types.f.frag       \
                       get_model_NBC_methods.f.frag           \
                       write_KIM_descriptor.f.frag            \
                       update_neighborlist.f.frag             \
                       MI_OPBC_cluster_neighborlist.f.frag    \
                       NEIGH_PURE_cluster_neighborlist.f.frag \
                       NEIGH_RVEC_cluster_neighborlist.f.frag \
                       NEIGH_RVEC_update_Rij_vectors.f.frag   \
                       get_neigh_no_Rij.f.frag                \
                       get_neigh_Rij.f.frag                   \
                       create_FCC_configuration.f.frag        \
                       compute_numer_deriv.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) vc_forces_numer_deriv_README | $(SED_CMD)                                                    >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .F90 file in the directory
	$(F_CAT) program_vc_forces_numer_deriv.f.frag  | $(SED_CMD)                                           >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_supported_types.f.frag                                                             >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_NBC_methods.f.frag                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) write_KIM_descriptor.f.frag | $(SED_CMD)                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) update_neighborlist.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_update_Rij_vectors.f.frag                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) create_FCC_configuration.f.frag                                                              >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) compute_numer_deriv.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

vc_config_ener_forces: Makefile                                 \
                       test_Makefile                            \
                       vc_config_ener_forces_README             \
                       program_vc_config_ener_forces.f.frag     \
                       get_model_NBC_methods.f.frag             \
                       write_KIM_descriptor.f.frag              \
                       update_neighborlist.f.frag               \
                       MI_OPBC_cluster_neighborlist.f.frag      \
                       NEIGH_PURE_cluster_neighborlist.f.frag   \
                       NEIGH_RVEC_cluster_neighborlist.f.frag   \
                       NEIGH_RVEC_update_Rij_vectors.f.frag     \
                       get_neigh_no_Rij.f.frag                  \
                       get_neigh_Rij.f.frag
        # make directory
	mkdir -p $(KIM_EX_TESTS_DIR)/$@
	@touch $(KIM_EX_TESTS_DIR)/$@
	cp $(KIM_DIR)/LICENSE.CDDL $(KIM_EX_TESTS_DIR)/$@
        # make README file in the directory
	$(D_CAT) vc_config_ener_forces_README | $(SED_CMD)                                                    >  $(KIM_EX_TESTS_DIR)/$@/README
        # make Makefile in the directory
	$(D_CAT) test_Makefile | $(SED_CMD)                                                                   >  $(KIM_EX_TESTS_DIR)/$@/Makefile
        # make .F90 file in the directory
	$(F_CAT) program_vc_config_ener_forces.f.frag  | $(SED_CMD)                                           >  $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_model_NBC_methods.f.frag                                                                 >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) write_KIM_descriptor.f.frag | $(SED_CMD)                                                     >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) update_neighborlist.f.frag                                                                   >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) MI_OPBC_cluster_neighborlist.f.frag                                                          >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_PURE_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_cluster_neighborlist.f.frag                                                       >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) NEIGH_RVEC_update_Rij_vectors.f.frag                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_no_Rij.f.frag                                                                      >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	$(F_CAT) get_neigh_Rij.f.frag                                                                         >> $(KIM_EX_TESTS_DIR)/$@/$@.F90
	echo                                                                                                  >> $(KIM_EX_TESTS_DIR)/$@/$@.F90

.PHONY: clean

clean:

#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                    
# Author: Ryan S. Elliott
#


KIM_DIR := ../../
KIM_TESTS_DIR := $(KIM_DIR)TESTs/

VPATH = $(KIM_TESTS_DIR)

SED_CMD = sed -e "s/TEST_NAME_STR/$@/g"

COMMON_FRAGMENTS = test_Makefile          \
                   descriptor_file_start  \
                   descriptor_file_input  \
                   descriptor_file_output \
                   report_error.frag      \
                   Makefile

COMMON_COHESSIVE_FRAGMENTS = test_FCCcohesive_README   \
                             setup_KIM_API_object.frag \
                             free_KIM_API_object.frag

AUTO_GEN_TESTS = test_Al_free_cluster                 \
                 test_Al_periodic_FCC_MI_OPBC         \
                 test_ArNe_periodic_B2_NEIGH_RVEC     \
                 test_Ar_FCCcohesive_MI_OPBC          \
                 test_Ar_FCCcohesive_NEIGH_PURE       \
                 test_Ar_free_cluster_CLUSTER         \
                 test_Ar_free_cluster                 \
                 test_Ar_multiple_models              \
                 test_Ar_periodic_FCC_MI_OPBC         \
                 test_Ar_periodic_FCC_NEIGH_PURE      \
                 test_Ar_periodic_FCC_NEIGH_RVEC      \
                 test_Ar_periodic_cut_FCC_NEIGH_RVEC  \
                 test_Ne_periodic_cut_FCC_NEIGH_RVEC


all: $(AUTO_GEN_TESTS) EXAMPLES.README

EXAMPLES.README: test_EXAMPLES.README Makefile
	cp test_EXAMPLES.README $(KIM_TESTS_DIR)EXAMPLES.README

test_ArNe_periodic_B2_NEIGH_RVEC: $(COMMON_FRAGMENTS)                        \
                                  test_periodic_B2_README                    \
                                  descriptor_file_neighborlist               \
                                  program_periodic_B2_NEIGH_RVEC.frag        \
                                  NEIGH_RVEC_F_periodic_B2_neighborlist.frag \
                                  get_neigh_Rij.frag

        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_B2_README | $(SED_CMD) \
                                    | sed -e 's/SPECIES1_NAME_STR/Ar/g' \
                                    | sed -e 's/SPECIES2_NAME_STR/Ne/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18\n\nNe                      spec                    10/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_B2_NEIGH_RVEC.frag | $(SED_CMD) \
                                                 | sed -e 's/SPECIES1_NAME_STR/Ar/g' \
                                                 | sed -e 's/SPECIES2_NAME_STR/Ne/g' \
                                                 | sed -e 's/B2_SPACING_STR/5.260d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_periodic_B2_neighborlist.frag    >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_free_cluster_CLUSTER: $(COMMON_FRAGMENTS)               \
                              test_free_cluster_README          \
                              program_free_cluster_CLUSTER.frag \
                              create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_free_cluster_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "CLUSTER                 dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "forces                  real*8       force      standard                [numberOfAtoms,3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_free_cluster_CLUSTER.frag | $(SED_CMD) \
                                              | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                              | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Al_free_cluster: $(COMMON_FRAGMENTS)                    \
                      test_free_cluster_README               \
                      descriptor_file_neighborlist           \
                      program_free_cluster.frag              \
                      MI_OPBC_neighborlist.frag              \
                      NEIGH_PURE_cluster_neighborlist.frag   \
                      NEIGH_RVEC_F_cluster_neighborlist.frag \
                      get_neigh_no_Rij.frag                  \
                      get_neigh_Rij.frag                     \
                      create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_free_cluster_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Al/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Al                      spec                    13/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "forces                  real*8       force      standard                [numberOfAtoms,3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_free_cluster.frag | $(SED_CMD) \
                                      | sed -e 's/SPECIES_NAME_STR/Al/g' \
                                      | sed -e 's/FCC_SPACING_STR/4.050d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat MI_OPBC_neighborlist.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_PURE_cluster_neighborlist.frag          >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_cluster_neighborlist.frag        >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_free_cluster: $(COMMON_FRAGMENTS)                    \
                      test_free_cluster_README               \
                      descriptor_file_neighborlist           \
                      program_free_cluster.frag              \
                      MI_OPBC_neighborlist.frag              \
                      NEIGH_PURE_cluster_neighborlist.frag   \
                      NEIGH_RVEC_F_cluster_neighborlist.frag \
                      get_neigh_no_Rij.frag                  \
                      get_neigh_Rij.frag                     \
                      create_FCC_configuration.frag          \
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_free_cluster_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "forces                  real*8       force      standard                [numberOfAtoms,3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_free_cluster.frag | $(SED_CMD) \
                                      | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                      | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat MI_OPBC_neighborlist.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_PURE_cluster_neighborlist.frag          >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_cluster_neighborlist.frag        >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Al_periodic_FCC_MI_OPBC: $(COMMON_FRAGMENTS)               \
                              test_periodic_FCC_README          \
                              descriptor_file_neighborlist      \
                              program_periodic_FCC_MI_OPBC.frag \
                              MI_OPBC_neighborlist.frag         \
                              get_neigh_no_Rij.frag             \
                              create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Al/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Al                      spec                    13/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_FCC_MI_OPBC.frag | $(SED_CMD) \
                                              | sed -e 's/SPECIES_NAME_STR/Al/g' \
                                              | sed -e 's/FCC_SPACING_STR/4.050d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat MI_OPBC_neighborlist.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_FCCcohesive_MI_OPBC: $(COMMON_FRAGMENTS)                        \
                             $(COMMON_COHESIVE_FRAGMENTS)               \
                             descriptor_file_neighborlist               \
                             program_FCCcohesive_MI_OPBC.frag           \
                             get_model_cutoff_firsttime.frag            \
                             setup_neighborlist_no_Rij_KIM_access.frag  \
                             MI_OPBC_neighborlist.frag                  \
                             get_neigh_no_Rij.frag                      \
                             create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_FCCcohesive_README  | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_FCCcohesive_MI_OPBC.frag  | $(SED_CMD) \
                                              | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                              | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_model_cutoff_firsttime.frag               >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat setup_KIM_API_object.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat setup_neighborlist_no_Rij_KIM_access.frag     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat free_KIM_API_object.frag                      >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat MI_OPBC_neighborlist.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_FCCcohesive_NEIGH_PURE: $(COMMON_FRAGMENTS)                        \
                                $(COMMON_COHESIVE_FRAGMENTS)               \
                                descriptor_file_neighborlist               \
                                program_FCCcohesive_NEIGH_PURE.frag        \
                                get_model_cutoff_firsttime.frag            \
                                setup_neighborlist_no_Rij_KIM_access.frag  \
                                NEIGH_PURE_periodic_neighborlist.frag      \
                                get_neigh_no_Rij.frag                      \
                                create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_FCCcohesive_README  | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_FCCcohesive_NEIGH_PURE.frag  | $(SED_CMD) \
                                                 | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                 | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_model_cutoff_firsttime.frag               >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat setup_KIM_API_object.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat setup_neighborlist_no_Rij_KIM_access.frag     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat free_KIM_API_object.frag                      >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_PURE_periodic_neighborlist.frag         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_periodic_FCC_MI_OPBC: $(COMMON_FRAGMENTS)               \
                              test_periodic_FCC_README          \
                              descriptor_file_neighborlist      \
                              program_periodic_FCC_MI_OPBC.frag \
                              MI_OPBC_neighborlist.frag         \
                              get_neigh_no_Rij.frag             \
                              create_FCC_configuration.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-H               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "MI-OPBC-F               dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "boxlength               real*8       length     standard                [3]" \
                                                          >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_FCC_MI_OPBC.frag | $(SED_CMD) \
                                              | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                              | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat MI_OPBC_neighborlist.frag                     >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_periodic_FCC_NEIGH_PURE: $(COMMON_FRAGMENTS)                   \
                                 test_periodic_FCC_README              \
                                 descriptor_file_neighborlist          \
                                 program_periodic_FCC_NEIGH_PURE.frag  \
                                 NEIGH_PURE_periodic_neighborlist.frag \
                                 get_neigh_no_Rij.frag                 \
                                 create_FCC_configuration.frag         \
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-H            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-PURE-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_FCC_NEIGH_PURE.frag | $(SED_CMD) \
                                                 | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                 | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_PURE_periodic_neighborlist.frag         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_no_Rij.frag                         >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat create_FCC_configuration.frag                 >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_periodic_FCC_NEIGH_RVEC: $(COMMON_FRAGMENTS)                         \
                                 test_periodic_FCC_README                    \
                                 descriptor_file_neighborlist                \
                                 program_periodic_FCC_NEIGH_RVEC.frag        \
                                 NEIGH_RVEC_F_periodic_FCC_neighborlist.frag \
                                 get_neigh_Rij.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_FCC_NEIGH_RVEC.frag | $(SED_CMD) \
                                                 | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                 | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_periodic_FCC_neighborlist.frag   >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ar_periodic_cut_FCC_NEIGH_RVEC: $(COMMON_FRAGMENTS)                          \
                                     test_periodic_FCC_README                     \
                                     descriptor_file_neighborlist                 \
                                     program_periodic_cut_FCC_NEIGH_RVEC.frag     \
                                     NEIGH_RVEC_F_periodic_FCC_neighborlist.frag  \
                                     get_neigh_Rij.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ar                      spec                    18/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_cut_FCC_NEIGH_RVEC.frag | $(SED_CMD) \
                                                     | sed -e 's/SPECIES_NAME_STR/Ar/g' \
                                                     | sed -e 's/FCC_SPACING_STR/5.26d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_periodic_FCC_neighborlist.frag   >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90

test_Ne_periodic_cut_FCC_NEIGH_RVEC: $(COMMON_FRAGMENTS)                         \
                                     test_periodic_FCC_README                    \
                                     descriptor_file_neighborlist                \
                                     program_periodic_cut_FCC_NEIGH_RVEC.frag    \
                                     NEIGH_RVEC_F_periodic_FCC_neighborlist.frag \
                                     get_neigh_Rij.frag
        # make directory
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
        # make README file in the directory
	cat test_periodic_FCC_README | $(SED_CMD) \
                                     | sed -e 's/SPECIES_NAME_STR/Ne/g' \
                                                          >  $(KIM_TESTS_DIR)$@/README
        # make Makefile in the directory
	cat test_Makefile | $(SED_CMD)                    >  $(KIM_TESTS_DIR)$@/Makefile
        # make .kim file in the directory
	cat descriptor_file_start    | $(SED_CMD) \
                                     | sed -e 's/SPECIES_LINE_STR/Ne                      spec                    10/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo "NEIGH-RVEC-F            dummy"              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_input                         >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_neighborlist                  >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.kim
	cat descriptor_file_output                        >> $(KIM_TESTS_DIR)$@/$@.kim
        # make .F90 file in the directory
	cat program_periodic_cut_FCC_NEIGH_RVEC.frag | $(SED_CMD) \
                                                     | sed -e 's/SPECIES_NAME_STR/Ne/g' \
                                                     | sed -e 's/FCC_SPACING_STR/4.430d0 ! in angstroms/g' \
                                                          >  $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat NEIGH_RVEC_F_periodic_FCC_neighborlist.frag   >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat get_neigh_Rij.frag                            >> $(KIM_TESTS_DIR)$@/$@.F90
	echo                                              >> $(KIM_TESTS_DIR)$@/$@.F90
	cat report_error.frag                             >> $(KIM_TESTS_DIR)$@/$@.F90


test_demo_F03: example_test_demo_F03                               \
                               $(wildcard example_test_demo_F03/*) \
                               Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

test_Ar_multiple_models: example_test_Ar_multiple_models               \
                         $(wildcard example_test_Ar_multiple_models/*) \
                         Makefile
	mkdir -p $(KIM_TESTS_DIR)$@
	@touch $(KIM_TESTS_DIR)$@
	cp -rf example_$@/* $(KIM_TESTS_DIR)$@/

.PHONY: clean

clean: 

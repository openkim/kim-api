
# KIM API alpha release, 11-Feb-2011
# 
# this make file builds/cleans everything 
#
#

include KIM_API/Include.mk

all: api models tests

api:
	cd KIM_API/ && make && cd ..

models: fortran_models
	cd MODELs/Sample_01_lj_cutoff_c && make && cd ..
	cd MODELs/Sample_01_lj_cutoff_cpp && make && cd ..

tests: fortran_tests
	cd TESTs/Sample_01_compute_example_c && make && cd ..

ifdef NO_FORTRAN
fortran_models:
	echo Skip fotran parts
fortran_tests:
	echo Skip fotran parts
else
fortran_models:
	cd MODELs/Sample_01_lj_cutoff && make && cd ..
fortran_tests:
	cd TESTs/Sample_01_compute_example_f && make && cd ..
endif

clean:
	cd KIM_API/ && make clean && cd ..
	cd MODELs/Sample_01_lj_cutoff && make clean && cd ..
	cd MODELs/Sample_01_lj_cutoff_c && make clean && cd ..
	cd MODELs/Sample_01_lj_cutoff_cpp && make clean && cd ..
	cd TESTs/Sample_01_compute_example_c && make clean && cd ..
	cd TESTs/Sample_01_compute_example_f && make clean && cd ..


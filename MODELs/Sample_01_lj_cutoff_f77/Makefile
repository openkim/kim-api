MODEL_BUILD_TARGET:= Sample_01_lj_cutoff_f77.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

Sample_01_lj_cutoff_f77.a:  LJ_mod_f77.o LJ_mod.o
	@ar rcs  $@ *.o

clean:
	rm -f *.o *.mod *.a *.so

LJ_mod_f77.F90:LJ_mod.F

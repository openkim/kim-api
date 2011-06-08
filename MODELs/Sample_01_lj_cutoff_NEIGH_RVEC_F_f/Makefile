MODEL_BUILD_TARGET:= Sample_01_lj_cutoff_NEIGH_RVEC_F_f.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif


all: $(MODEL_BUILD_TARGET)

Sample_01_lj_cutoff_NEIGH_RVEC_F_f.a: LJ_rvec_F_mod.o
	@ar rcs  $@ *.o

clean:
	rm -f *.o *.mod *.a *.so



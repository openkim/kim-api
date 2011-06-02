MODEL_BUILD_TARGET:= Sample_01_lj_cutoff_c.a

ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

all: $(MODEL_BUILD_TARGET)

Sample_01_lj_cutoff_c.a:  LJ_sample1.o
	@ar rcs  $@ *.o


clean:
	rm -f *.o *.a *.so

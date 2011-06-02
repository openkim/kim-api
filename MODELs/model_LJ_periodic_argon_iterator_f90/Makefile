ifndef KIM_API_DIR
include ../../KIM_API/Include.mk
else
include $(KIM_API_DIR)Include.mk
endif

all: model_LJ_periodic_argon_iterator_f90.a

model_LJ_periodic_argon_iterator_f90.a: model_LJ_periodic_argon_iterator.o
	@ar rcs $@ *.o

clean:
	rm -f *.o *.mod *.a


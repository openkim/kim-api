 # Define GNU compiler switches
   OBJONLY=-c
   OUTPUTIN=-o
   FORTRANFLAG = -fcray-pointer -O3 -I$(KIM_API_DIR) -D $(MACHINESYSTEM)                     \
                                                     -D KIM_DIR_API=\"$(KIM_API_DIR)\"       \
                                                     -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\" \
                                                     -D KIM_DIR_TESTS=\"$(KIM_TESTS_DIR)\"   \
                                                     -D KIM_DIR_MODEL_DRIVERS=\"$(KIM_MODEL_DRIVERS_DIR)\"
   #CCOMPILER   = gcc
   #CPPCOMPILER = g++
   CCOMPILER   = mpicc
   CPPCOMPILER = mpiCC
   #CCOMPILER   = gcc-fsf-4.4  # for OS X using fink compilers
   #CPPCOMPILER = g++-fsf-4.4  # for OS X using fink compilers
   #CCOMPILER   = gcc-fsf-4.6  # for OS X using fink compilers                                  
   #CPPCOMPILER = g++-fsf-4.6  # for OS X using fink compilers
   CPPFLAG = -O3 -I$(KIM_API_DIR) -Wno-write-strings -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\" \
                                                     -D KIM_DIR_API=\"$(KIM_API_DIR)\"       \
                                                     -D KIM_DIR_TESTS=\"$(KIM_TESTS_DIR)\"   \
                                                     -D KIM_DIR_MODEL_DRIVERS=\"$(KIM_MODEL_DRIVERS_DIR)\"
   FORTRANCOMPILER = gfortran
   #FORTRANCOMPILER = mpif90
   CPPLIBFLAG = -lgfortran           #if GNU version 4.5 and up. tested on suse 
   LINKCOMPILER = $(CPPCOMPILER)     #if GNU version 4.5 and up. tested on suse
   #CPPLIBFLAG = -lstdc++             #if GNU version 4.4.1. tested on suse
   #LINKCOMPILER = $(FORTRANCOMPILER) #if GNU version 4.4.1. tested on suse
   ifdef KIM_DYNAMIC
      CPPLIBFLAG += -ldl
   endif

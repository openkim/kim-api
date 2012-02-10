  # Define Intel compiler switches
   OBJONLY=-c
   OUTPUTIN=-o
   FORTRANFLAG =-I $(KIM_API_DIR) -D $(MACHINESYSTEM)                     \
                                  -D KIM_DIR_API=\"$(KIM_API_DIR)\"       \
                                  -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\" \
                                  -D KIM_DIR_TESTS=\"$(KIM_TESTS_DIR)\"   \
                                  -D KIM_DIR_MODEL_DRIVERS=\"$(KIM_MODEL_DRIVERS_DIR)\"
   CCOMPILER = icc
   CPPCOMPILER = icpc
   CPPFLAG = -O3 -I$(KIM_API_DIR) -D KIM_DIR_API=\"$(KIM_API_DIR)\"       \
                                  -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\" \
                                  -D KIM_DIR_TESTS=\"$(KIM_TESTS_DIR)\"   \
                                  -D KIM_DIR_MODEL_DRIVERS=\"$(KIM_MODEL_DRIVERS_DIR)\"
   CPPLIBFLAG = -nofor_main -cxxlib
   FORTRANLIBFLAG = -cxxlib
   FORTRANCOMPILER = ifort
   LINKCOMPILER = $(FORTRANCOMPILER)


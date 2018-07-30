set(C_SOURCES KIM_ChargeUnit_c.cpp
              KIM_ComputeArgumentName_c.cpp
              KIM_ComputeArguments_c.cpp
              KIM_ComputeCallbackName_c.cpp
              KIM_DataType_c.cpp
              KIM_EnergyUnit_c.cpp
              KIM_LanguageName_c.cpp
              KIM_LengthUnit_c.cpp
              KIM_Log_c.cpp
              KIM_LogVerbosity_c.cpp
              KIM_Model_c.cpp
              KIM_ModelCompute_c.cpp
              KIM_ModelComputeArguments_c.cpp
              KIM_ModelComputeArgumentsCreate_c.cpp
              KIM_ModelComputeArgumentsDestroy_c.cpp
              KIM_ModelCreate_c.cpp
              KIM_ModelDestroy_c.cpp
              KIM_ModelDriverCreate_c.cpp
              KIM_ModelRefresh_c.cpp
              KIM_Numbering_c.cpp
              KIM_SemVer_c.cpp
              KIM_SpeciesName_c.cpp
              KIM_SupportStatus_c.cpp
              KIM_TemperatureUnit_c.cpp
              KIM_TimeUnit_c.cpp
              )

add_library(c OBJECT ${C_SOURCES})

target_include_directories(c PRIVATE ../include)
target_include_directories(c PRIVATE ../../cpp/include)
target_include_directories(c PRIVATE ${CMAKE_BINARY_DIR})

#target_compile_definitions(c PRIVATE
#    PACKAGEDIR="${PACKAGE_DIR}"
#    KIMDIR="${KIM_DIR}"
#    KIMLIBBUILD="${KIM_LIB_BUILD}.so"
#    MODELDRIVERSDIR="${MODELDRIVERSDIR}"
#    MODELSDIR="${MODELSDIR}"
#    PACKAGENAME="${PACKAGE_NAME}"
#    USERROOT=1
#    USERCONFIGFILEROOTNAME="${USER_CONFIG_FILE_ROOT_NAME}"
#    USERCONFIGFILEDIRNAME="${USER_CONFIG_FILE_DIR_NAME}"
#    VERSION_MAJOR="${KIM_VERSION_MAJOR}"
#    MODELLIBFILE="lib${MODEL_LIB}"
#    MODELDRIVERLIBFILE="lib${MODEL_DRIVER_LIB}"
#)

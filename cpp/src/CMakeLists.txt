set(CPP_SOURCES KIM_ChargeUnit.cpp
                KIM_ComputeArgumentName.cpp
                KIM_ComputeArguments.cpp
                KIM_ComputeCallbackName.cpp
                KIM_DataType.cpp
                KIM_EnergyUnit.cpp
                KIM_LanguageName.cpp
                KIM_LengthUnit.cpp
                KIM_Log.cpp
                KIM_LogVerbosity.cpp
                KIM_Model.cpp
                KIM_ModelCompute.cpp
                KIM_ModelComputeArguments.cpp
                KIM_ModelComputeArgumentsCreate.cpp
                KIM_ModelComputeArgumentsDestroy.cpp
                KIM_ModelCreate.cpp
                KIM_ModelDestroy.cpp
                KIM_ModelDriverCreate.cpp
                KIM_ModelRefresh.cpp
                KIM_Numbering.cpp
                KIM_SemVer.cpp
                KIM_SpeciesName.cpp
                KIM_SupportStatus.cpp
                KIM_TemperatureUnit.cpp
                KIM_TimeUnit.cpp
                KIM_ComputeArgumentsImplementation.cpp
                KIM_LogImplementation.cpp
                KIM_ModelImplementation.cpp
                KIM_ModelLibrary.cpp
                old_KIM_API_DIRS.cpp)

add_library(cpp OBJECT ${CPP_SOURCES})

target_include_directories(cpp PRIVATE ../include)
target_include_directories(cpp PRIVATE ../../c/include)
target_include_directories(cpp PRIVATE ${CMAKE_BINARY_DIR})

target_compile_definitions(cpp PRIVATE
    PACKAGEDIR="${PACKAGE_DIR}"
    KIMDIR="${KIM_DIR}"
    KIMLIBBUILD="${KIM_LIB_BUILD}.so"
    MODELDRIVERSDIR="${MODELDRIVERSDIR}"
    MODELSDIR="${MODELSDIR}"
    PACKAGENAME="${PACKAGE_NAME}"
    USERROOT=1
    USERCONFIGFILEROOTNAME="${USER_CONFIG_FILE_ROOT_NAME}"
    USERCONFIGFILEDIRNAME="${USER_CONFIG_FILE_DIR_NAME}"
    VERSION_MAJOR="${KIM_VERSION_MAJOR}"
    MODELLIBFILE="lib${MODEL_LIB}"
    MODELDRIVERLIBFILE="lib${MODEL_DRIVER_LIB}"
)

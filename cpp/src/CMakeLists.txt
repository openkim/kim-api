set(CPP_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ChargeUnit.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentName.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArguments.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeCallbackName.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_DataType.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_EnergyUnit.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LanguageName.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LengthUnit.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Log.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogVerbosity.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Model.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCompute.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArguments.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsCreate.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsDestroy.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCreate.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDestroy.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDriverCreate.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRefresh.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Numbering.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SemVer.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SpeciesName.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SupportStatus.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TemperatureUnit.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TimeUnit.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentsImplementation.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogImplementation.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelImplementation.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelLibrary.cpp
                ${CMAKE_CURRENT_SOURCE_DIR}/old_KIM_API_DIRS.cpp)

target_sources(kim-api PRIVATE ${CPP_SOURCES})

target_include_directories(kim-api PUBLIC ../include)
target_include_directories(kim-api PUBLIC ../../c/include)
target_include_directories(kim-api PUBLIC ${CMAKE_BINARY_DIR})
target_include_directories(kim-api PUBLIC ../src)

target_compile_definitions(kim-api PUBLIC
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

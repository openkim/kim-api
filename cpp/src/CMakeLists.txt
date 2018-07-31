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

target_sources(${FULL_PACKAGE_NAME} PRIVATE ${CPP_SOURCES})

target_include_directories(${FULL_PACKAGE_NAME} PUBLIC ../include)
target_include_directories(${FULL_PACKAGE_NAME} PUBLIC ../../c/include)
target_include_directories(${FULL_PACKAGE_NAME} PUBLIC ${CMAKE_BINARY_DIR})
target_include_directories(${FULL_PACKAGE_NAME} PUBLIC ../src)

target_compile_definitions(${FULL_PACKAGE_NAME} PUBLIC
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

set(CPP_HEADERS
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ChargeUnit.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ChargeUnit.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeArgumentName.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeArgumentName.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeArguments.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeCallbackName.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeCallbackName.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_DataType.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_DataType.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_EnergyUnit.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_EnergyUnit.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_func.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LanguageName.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LanguageName.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LengthUnit.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LengthUnit.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Log.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LogVerbosity.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsCreate.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsCreateLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsDestroy.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsDestroyLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArguments.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCompute.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCreate.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCreateLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDestroy.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDestroyLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverCreate.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverCreateLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverHeaders.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelHeaders.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Model.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelRefresh.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelRefreshLogMacros.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Numbering.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Numbering.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SemVer.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SimulatorHeaders.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SpeciesName.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SpeciesName.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SupportStatus.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SupportStatus.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TemperatureUnit.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TemperatureUnit.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TimeUnit.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TimeUnit.inc
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_UnitSystem.hpp
    ${CMAKE_BINARY_DIR}/KIM_LOG_DEFINES.inc
    ${CMAKE_BINARY_DIR}/KIM_Version.hpp
)

install(FILES ${CPP_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${FULL_PACKAGE_NAME})

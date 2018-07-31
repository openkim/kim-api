set(C_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ChargeUnit_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentName_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArguments_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeCallbackName_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_DataType_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_EnergyUnit_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LanguageName_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LengthUnit_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Log_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogVerbosity_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Model_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCompute_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArguments_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsCreate_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsDestroy_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCreate_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDestroy_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDriverCreate_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRefresh_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Numbering_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SemVer_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SpeciesName_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SupportStatus_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TemperatureUnit_c.cpp
              ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TimeUnit_c.cpp
              )

target_sources(${FULL_PACKAGE_NAME} PRIVATE ${C_SOURCES})

target_include_directories(${FULL_PACKAGE_NAME} PRIVATE ../include)
target_include_directories(${FULL_PACKAGE_NAME} PRIVATE ../../cpp/include)
target_include_directories(${FULL_PACKAGE_NAME} PRIVATE ${CMAKE_BINARY_DIR})

set(C_HEADERS
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ChargeUnit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeArgumentName.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeArguments.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ComputeCallbackName.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_DataType.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_EnergyUnit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_func.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LanguageName.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LengthUnit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Log.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_LogVerbosity.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsCreate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsCreateLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsDestroy.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsDestroyLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArguments.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeArgumentsLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCompute.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelComputeLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCreate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelCreateLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDestroy.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDestroyLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverCreate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverCreateLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelDriverHeaders.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Model.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelHeaders.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelRefresh.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_ModelRefreshLogMacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_Numbering.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SemVer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SimulatorHeaders.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SpeciesName.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_SupportStatus.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TemperatureUnit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_TimeUnit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/KIM_UnitSystem.h
    ${CMAKE_BINARY_DIR}/KIM_Version.h
)

install(FILES ${C_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${FULL_PACKAGE_NAME})

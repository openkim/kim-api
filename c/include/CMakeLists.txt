target_include_directories(kim-api PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

set(C_HEADERS
    KIM_ChargeUnit.h
    KIM_ComputeArgumentName.h
    KIM_ComputeArguments.h
    KIM_ComputeCallbackName.h
    KIM_DataType.h
    KIM_EnergyUnit.h
    KIM_func.h
    KIM_LanguageName.h
    KIM_LengthUnit.h
    KIM_Log.h
    KIM_LogVerbosity.h
    KIM_ModelComputeArgumentsCreate.h
    KIM_ModelComputeArgumentsCreateLogMacros.h
    KIM_ModelComputeArgumentsDestroy.h
    KIM_ModelComputeArgumentsDestroyLogMacros.h
    KIM_ModelComputeArguments.h
    KIM_ModelComputeArgumentsLogMacros.h
    KIM_ModelCompute.h
    KIM_ModelComputeLogMacros.h
    KIM_ModelCreate.h
    KIM_ModelCreateLogMacros.h
    KIM_ModelDestroy.h
    KIM_ModelDestroyLogMacros.h
    KIM_ModelDriverCreate.h
    KIM_ModelDriverCreateLogMacros.h
    KIM_ModelDriverHeaders.h
    KIM_ModelHeaders.h
    KIM_Model.h
    KIM_ModelRefresh.h
    KIM_ModelRefreshLogMacros.h
    KIM_Numbering.h
    KIM_SemVer.h
    KIM_SimulatorHeaders.h
    KIM_SpeciesName.h
    KIM_SupportStatus.h
    KIM_TemperatureUnit.h
    KIM_TimeUnit.h
    KIM_UnitSystem.h
    ${CMAKE_BINARY_DIR}/KIM_Version.h
)

install(FILES ${C_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${FULL_PACKAGE_NAME})

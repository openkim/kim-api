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

target_sources(kim-api PRIVATE ${C_SOURCES})

target_include_directories(kim-api PRIVATE ../include)
target_include_directories(kim-api PRIVATE ../../cpp/include)
target_include_directories(kim-api PRIVATE ${CMAKE_BINARY_DIR})

cmake_minimum_required(VERSION 3.0)
find_package(PkgConfig)
pkg_check_modules(KIM-API REQUIRED libkim-api-v2)

set(MODEL_DRIVER_NAME "LennardJones612__MD_414112407348_003")
set(MODEL_DRIVER_CREATE_FUNCTION_NAME "model_driver_create")
set(MODEL_DRIVER_CREATE_FUNCTION_LANG "cpp")

configure_file(driver_init_wrapper.cpp.in driver_init_wrapper.cpp @ONLY)

set(SOURCES
  LennardJones612.cpp
  LennardJones612Implementation.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/driver_init_wrapper.cpp
  )

add_library(model-driver MODULE ${SOURCES})
set_target_properties(model-driver PROPERTIES OUTPUT_NAME
  "kim-api-model-driver-v2")
link_directories(${KIM-API_LIBRARY_DIRS})

target_include_directories(model-driver PRIVATE ${KIM-API_INCLUDE_DIRS})
target_link_libraries(model-driver ${KIM-API_LDFLAGS})
target_compile_options(model-driver PUBLIC ${KIM-API_CFLAGS})
target_compile_definitions(model-driver PUBLIC ${KIM-API_DEFINITIONS})

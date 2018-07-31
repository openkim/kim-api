cmake_minimum_required(VERSION 3.0)
find_package(PkgConfig)
pkg_check_modules(KIM-API REQUIRED libkim-api-v2)

set(MODEL_NAME "LennardJones_Ar")
set(MODEL_CREATE_FUNCTION_NAME "model_create")
set(MODEL_CREATE_FUNCTION_LANG "cpp")

configure_file(init_wrapper.cpp.in init_wrapper.cpp @ONLY)

set(SOURCES
  LennardJones_Ar.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp
  )

add_library(model MODULE ${SOURCES})
set_target_properties(model PROPERTIES OUTPUT_NAME
  "kim-api-model-v2")
link_directories(${KIM-API_LIBRARY_DIRS})

target_include_directories(model PRIVATE ${KIM-API_INCLUDE_DIRS})
target_link_libraries(model ${KIM-API_LDFLAGS})
target_compile_options(model PUBLIC ${KIM-API_CFLAGS})
target_compile_definitions(model PUBLIC ${KIM-API_DEFINITIONS})

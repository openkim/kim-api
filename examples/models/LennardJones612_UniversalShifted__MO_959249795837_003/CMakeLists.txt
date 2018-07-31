cmake_minimum_required(VERSION 3.0)
find_package(PkgConfig)
if(TARGET kim-api)
  set(KIM-API_LDFLAGS kim-api)
else()
  pkg_check_modules(KIM-API REQUIRED libkim-api-v2)
endif()

set(MODEL_DRIVER_NAME "LennardJones612__MD_414112407348_003")
set(MODEL_NAME "LennardJones612_UniversalShifted__MO_959249795837_003")

set(PARAMETER_FILES
  LennardJones612_UniversalShifted.params
  )

list(LENGTH PARAMETER_FILES NUMBER_OF_PARAMETER_FILES)

configure_file(init_wrapper.cpp.in init_wrapper.cpp @ONLY)

set(SOURCES
  ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp
  )

set(IDX 1)
foreach(FNAME ${PARAMETER_FILES})
  set(PARAM_FILE_IN "${CMAKE_CURRENT_SOURCE_DIR}/${FNAME}")
  set(PARAM_FILE_XXD_IN "${CMAKE_CURRENT_BINARY_DIR}/parameter_file_${IDX}")
  set(PARAM_FILE "${PARAM_FILE_XXD_IN}.c")
  list(APPEND SOURCES ${PARAM_FILE})
  add_custom_command(OUTPUT ${PARAM_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${PARAM_FILE_IN}" "${PARAM_FILE_XXD_IN}"
    COMMAND xxd -i "parameter_file_${IDX}" "${PARAM_FILE}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
  MATH(EXPR IDX "${IDX}+1")
endforeach()

add_library(model MODULE ${SOURCES})
set_target_properties(model PROPERTIES OUTPUT_NAME
  "kim-api-model-v2")
link_directories(${KIM-API_LIBRARY_DIRS})

target_include_directories(model PRIVATE ${KIM-API_INCLUDE_DIRS})
target_link_libraries(model ${KIM-API_LDFLAGS})
target_compile_options(model PUBLIC ${KIM-API_CFLAGS})
target_compile_definitions(model PUBLIC ${KIM-API_DEFINITIONS})

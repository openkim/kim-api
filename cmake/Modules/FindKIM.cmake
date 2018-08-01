# - Find KIM
# Find the KIM headers and libraries.
#
#  KIM_INCLUDE_DIRS - where to find KIM headers, etc.
#  KIM_LIBRARIES    - List of libraries when using KIM.
#  KIM_API_FOUND    - True if KIM found.
#

find_package(PkgConfig)

if(TARGET kim-api)
    set(KIM_LDFLAGS kim-api)
    set(KIM_CMAKE_DIR ${CMAKE_SOURCE_DIR}/cmake)
else()
    pkg_check_modules(KIM REQUIRED libkim-api-v2)

    include(FindPackageHandleStandardArgs)
    # handle the QUIETLY and REQUIRED arguments and set KIM_FOUND to TRUE
    # if all listed variables are TRUE

    find_package_handle_standard_args(KIM DEFAULT_MSG KIM_LIBRARIES KIM_INCLUDE_DIRS)

    mark_as_advanced(KIM_LIBRARIES KIM_INCLUDE_DIRS)

    set(KIM_CMAKE_DIR ${KIM_LIBDIR}/kim-api-v2/cmake)
endif()

set(KIM_VERSION_FULL ${KIM_VERSION})

function(kim_add_standalone_model)
    set(options "")
    set(oneValueArgs NAME CREATE_FUNCTION_NAME CREATE_FUNCTION_LANG)
    set(multiValueArgs SOURCES)
    cmake_parse_arguments(MODEL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    configure_file(${KIM_CMAKE_DIR}/stand-alone-model_init_wrapper.cpp.in
                   ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp @ONLY)

    list(APPEND MODEL_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp)

    add_library(${MODEL_NAME} MODULE ${MODEL_SOURCES})
    set_target_properties(${MODEL_NAME} PROPERTIES OUTPUT_NAME "kim-api-model-v2"
                                                   LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/models/${MODEL_NAME})
    link_directories(${KIM_LIBRARY_DIRS})

    target_include_directories(${MODEL_NAME} PRIVATE ${KIM_INCLUDE_DIRS})
    target_link_libraries(${MODEL_NAME} ${KIM_LDFLAGS})
    target_compile_options(${MODEL_NAME} PUBLIC ${KIM_CFLAGS})
    target_compile_definitions(${MODEL_NAME} PUBLIC ${KIM_DEFINITIONS})
endfunction(kim_add_standalone_model)

function(kim_add_parameterized_model)
    set(options "")
    set(oneValueArgs NAME DRIVER_NAME)
    set(multiValueArgs PARAMETER_FILES)
    cmake_parse_arguments(MODEL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    list(LENGTH MODEL_PARAMETER_FILES NUMBER_OF_PARAMETER_FILES)

    configure_file(${KIM_CMAKE_DIR}/parameterized-model_init_wrapper.cpp.in
                   ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp @ONLY)

    set(MODEL_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp)

    set(IDX 1)
    foreach(FNAME ${MODEL_PARAMETER_FILES})
      set(PARAM_FILE_IN "${CMAKE_CURRENT_SOURCE_DIR}/${FNAME}")
      set(PARAM_FILE_XXD_IN "${CMAKE_CURRENT_BINARY_DIR}/parameter_file_${IDX}")
      set(PARAM_FILE "${PARAM_FILE_XXD_IN}.c")
      list(APPEND MODEL_SOURCES ${PARAM_FILE})
      add_custom_command(OUTPUT ${PARAM_FILE}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${PARAM_FILE_IN}" "${PARAM_FILE_XXD_IN}"
        COMMAND xxd -i "parameter_file_${IDX}" "${PARAM_FILE}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
      MATH(EXPR IDX "${IDX}+1")
    endforeach()

    add_library(${MODEL_NAME} MODULE ${MODEL_SOURCES})
    set_target_properties(${MODEL_NAME} PROPERTIES OUTPUT_NAME "kim-api-model-v2"
                                                   LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/models/${MODEL_NAME})
    link_directories(${KIM_LIBRARY_DIRS})

    target_include_directories(${MODEL_NAME} PRIVATE ${KIM_INCLUDE_DIRS})
    target_link_libraries(${MODEL_NAME} ${KIM_LDFLAGS})
    target_compile_options(${MODEL_NAME} PUBLIC ${KIM_CFLAGS})
    target_compile_definitions(${MODEL_NAME} PUBLIC ${KIM_DEFINITIONS})
endfunction(kim_add_parameterized_model)

function(kim_add_model_driver)
    set(options "")
    set(oneValueArgs NAME CREATE_FUNCTION_NAME CREATE_FUNCTION_LANG)
    set(multiValueArgs SOURCES)
    cmake_parse_arguments(MODEL_DRIVER "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    configure_file(${KIM_CMAKE_DIR}/driver_init_wrapper.cpp.in
                   ${CMAKE_CURRENT_BINARY_DIR}/driver_init_wrapper.cpp @ONLY)

    list(APPEND MODEL_DRIVER_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/driver_init_wrapper.cpp)

    add_library(${MODEL_DRIVER_NAME} MODULE ${MODEL_DRIVER_SOURCES})
    set_target_properties(${MODEL_DRIVER_NAME} PROPERTIES OUTPUT_NAME "kim-api-model-driver-v2"
                                                          LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/model_drivers/${MODEL_DRIVER_NAME})
    link_directories(${KIM_LIBRARY_DIRS})

    target_include_directories(${MODEL_DRIVER_NAME} PRIVATE ${KIM_INCLUDE_DIRS})
    target_link_libraries(${MODEL_DRIVER_NAME} ${KIM_LDFLAGS})
    target_compile_options(${MODEL_DRIVER_NAME} PUBLIC ${KIM_CFLAGS})
    target_compile_definitions(${MODEL_DRIVER_NAME} PUBLIC ${KIM_DEFINITIONS})
endfunction(kim_add_model_driver)

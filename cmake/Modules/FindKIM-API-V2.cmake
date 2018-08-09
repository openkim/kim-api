# - Find KIM-API-V2
# Find the KIM-API-V2 headers and libraries.
#
#  KIM-API-V2_TARGET                      - the target name for the kim-api library
#  KIM-API-V2_INCLUDE_DIRS                - where to find KIM-API-V2 headers, etc.
#  KIM-API-V2_LIBRARIES                   - List of libraries when using KIM-API-V2.
#  KIM-API-V2_CMAKE_DIR                   - directory containing KIM-API-V2 cmake files.
#  KIM-API-V2_COLLECTIONS_INFO_EXECUTABLE - executable full path.
#  KIM-API-V2_MODEL_INSTALL_PREFIX        - where to install models.
#  KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX - where to install model drivers.
#  KIM-API-V2_FOUND                       - TRUE if KIM-API-V2 found.
#
#  add_kim_api_v2_model_library()         - set up kim-api model target
#  add_kim_api_v2_model_driver_library()  - set up kim-api model driver target
#


find_package(PkgConfig)

if(TARGET kim-api)
    set(KIM-API-V2_CMAKE_DIR ${CMAKE_SOURCE_DIR}/cmake)
    set(KIM-API-V2_COLLECTION_INFO_EXECUTABLE "") # invalid in build directory
    set(KIM-API-V2_TARGET kim-api)
else()
    pkg_check_modules(KIM-API-INTERNAL libkim-api-v2 QUIET)
    if(KIM-API-INTERNAL_FOUND)
      set(KIM-API-V2_LIBDIR ${KIM-API-INTERNAL_LIBDIR})
      set(KIM-API-V2_INCLUDE_DIRS ${KIM-API-INTERNAL_INCLUDE_DIRS})
      set(KIM-API-V2_LIBRARIES ${KIM-API-INTERNAL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${KIM-API-INTERNAL_LIBRARIES}${CMAKE_SHARED_LIBRARY_SUFFIX})
      set(KIM-API-V2_CMAKE_DIR ${KIM-API-INTERNAL_LIBDIR}/kim-api-v2/cmake)
      string(REGEX REPLACE "^([0-9]+\\.[0-9]+\\.[0-9]+).*" "\\1" KIM-API-INTERNAL_VERSION "${KIM-API-INTERNAL_VERSION}")
    else()
      find_path(KIM-API-V2_INCLUDE_DIRS KIM_Version.h PATH_SUFFIXES kim-api-v2)
      find_library(KIM-API-V2_LIBRARIES NAMES kim-api-v2)
      find_path(KIM-API-V2_CMAKE_DIR parameterized-model_init_wrapper.cpp.in PATH_SUFFIXES lib/kim-api-v2/cmake lib64/kim-api-v2/cmake)
      set(KIM-API-INTERNAL_VERSION 2)
    endif()
    project(kim-api-v2 VERSION ${KIM-API-INTERNAL_VERSION} LANGUAGES CXX C Fortran)
    find_program(KIM-API-V2_COLLECTION_INFO_EXECUTABLE kim-api-v2-collections-info
      PATH_SUFFIXES libexec/kim-api-v2 PATHS ${KIM-API-INTERNAL_PREFIX})
    execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} system models        OUTPUT_VARIABLE KIM_API_MODEL_IDENTIFIER        OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REGEX REPLACE "^.*/([^/]+)" "\\1" KIM_API_MODEL_IDENTIFIER "${KIM_API_MODEL_IDENTIFIER}")
    execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} system model_drivers OUTPUT_VARIABLE KIM_API_MODEL_DRIVER_IDENTIFIER OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REGEX REPLACE "^.*/([^/]+)" "\\1" KIM_API_MODEL_DRIVER_IDENTIFIER "${KIM_API_MODEL_DRIVER_IDENTIFIER}")

    add_library(KIM-API-V2::kim-api UNKNOWN IMPORTED)
    set_target_properties(KIM-API-V2::kim-api PROPERTIES IMPORTED_LOCATION ${KIM-API-V2_LIBRARIES}
      INTERFACE_INCLUDE_DIRECTORIES ${KIM-API-V2_INCLUDE_DIRS})
    set(KIM-API-V2_TARGET KIM-API-V2::kim-api)

    set(KIM-API-V2_VERSION 2.0)
    include(FindPackageHandleStandardArgs)

    # handle the QUIETLY and REQUIRED arguments and set KIM-API-V2_FOUND to TRUE
    # if all listed variables are TRUE
    find_package_handle_standard_args(KIM-API-V2
        REQUIRED_VARS KIM-API-V2_INCLUDE_DIRS KIM-API-V2_LIBRARIES KIM-API-V2_CMAKE_DIR KIM-API-V2_COLLECTION_INFO_EXECUTABLE
        VERSION_VAR KIM-API-V2_VERSION)
    mark_as_advanced(KIM-API-V2_INCLUDE_DIRS KIM-API-V2_LIBRARIES KIM-API-V2_CMAKE_DIR KIM-API-V2_COLLECTION_INFO_EXECUTABLE)
endif()

if(TARGET kim-api)
    set(KIM-API-V2_MODEL_INSTALL_PREFIX ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_MODEL_IDENTIFIER})
    set(KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_MODEL_DRIVER_IDENTIFIER})
else()
    set(KIM-API-V2_INSTALL_TYPE "SYSTEM" CACHE STRING "TODO add description here")
    set_property(CACHE KIM-API-V2_INSTALL_TYPE PROPERTY STRINGS SYSTEM USER ENVIRONMENT)

    if(KIM-API-V2_INSTALL_TYPE STREQUAL "SYSTEM")
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} system models        OUTPUT_VARIABLE KIM-API-V2_MODEL_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} system model_drivers OUTPUT_VARIABLE KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
    elseif(KIM-API-V2_INSTALL_TYPE STREQUAL "USER")
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} config_file models        OUTPUT_VARIABLE KIM-API-V2_MODEL_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} config_file model_drivers OUTPUT_VARIABLE KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
    elseif(KIM-API-V2_INSTALL_TYPE STREQUAL "ENVIRONMENT")
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} env models        OUTPUT_VARIABLE KIM-API-V2_MODEL_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
        execute_process(COMMAND ${KIM-API-V2_COLLECTION_INFO_EXECUTABLE} env model_drivers OUTPUT_VARIABLE KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
        # TODO error if empty
        # TODO split env var if necessary and select first element
    else()
        # TODO handle unknown value
    endif()
endif()


function(add_kim_api_v2_model_library)
    set(options "")
    set(oneValueArgs NAME DRIVER_NAME CREATE_FUNCTION_NAME CREATE_FUNCTION_LANGUAGE)
    set(multiValueArgs PARAMETER_FILES)
    cmake_parse_arguments(MODEL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    # TODO sanity checks to ensure all arguments are passed

    set(MODEL_SOURCES "")

    find_program(XXD_EXECUTABLE "xxd")
    find_package_handle_standard_args(XXD REQUIRED_VARS XXD_EXECUTABLE)
    if(NOT XXD_FOUND)
      message(FATAL_ERROR "TODO Ryan will put an error message here")
    endif()
    if(MODEL_PARAMETER_FILES)
        list(LENGTH MODEL_PARAMETER_FILES NUMBER_OF_PARAMETER_FILES)
        set(INIT_WRAPPER_FILE "${KIM-API-V2_CMAKE_DIR}/parameterized-model_init_wrapper.cpp.in")

        set(IDX 1)
        foreach(FNAME ${MODEL_PARAMETER_FILES})
          set(PARAM_FILE_IN "${CMAKE_CURRENT_SOURCE_DIR}/${FNAME}")
          set(PARAM_FILE_XXD_IN "${CMAKE_CURRENT_BINARY_DIR}/parameter_file_${IDX}")
          set(PARAM_FILE "${PARAM_FILE_XXD_IN}.c")
          list(APPEND MODEL_SOURCES ${PARAM_FILE})
          add_custom_command(OUTPUT ${PARAM_FILE}
            COMMAND ${CMAKE_COMMAND} -E copy_if_different "${PARAM_FILE_IN}" "${PARAM_FILE_XXD_IN}"
            COMMAND ${XXD_EXECUTABLE} -i "parameter_file_${IDX}" "${PARAM_FILE}"
            DEPENDS ${PARAM_FILE_IN}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            )
          MATH(EXPR IDX "${IDX}+1")
        endforeach()
    else()
        set(INIT_WRAPPER_FILE "${KIM-API-V2_CMAKE_DIR}/stand-alone-model_init_wrapper.cpp.in")
        # TODO add simulator model case
    endif()

    configure_file(${INIT_WRAPPER_FILE} ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp @ONLY)

    list(APPEND MODEL_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/init_wrapper.cpp)

    add_library(${MODEL_NAME} MODULE ${MODEL_SOURCES})
    set_target_properties(${MODEL_NAME} PROPERTIES OUTPUT_NAME "${PROJECT_NAME}-${KIM_API_MODEL_IDENTIFIER}"
                                                   LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${MODEL_NAME})

    target_link_libraries(${MODEL_NAME} ${KIM-API-V2_TARGET})

    install(TARGETS ${MODEL_NAME} LIBRARY DESTINATION "${KIM-API-V2_MODEL_INSTALL_PREFIX}/${MODEL_NAME}")
endfunction(add_kim_api_v2_model_library)

function(add_kim_api_v2_model_driver_library)
    set(options "")
    set(oneValueArgs NAME CREATE_FUNCTION_NAME CREATE_FUNCTION_LANGUAGE)
    set(multiValueArgs "")
    cmake_parse_arguments(MODEL_DRIVER "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    configure_file(${KIM-API-V2_CMAKE_DIR}/driver_init_wrapper.cpp.in
                   ${CMAKE_CURRENT_BINARY_DIR}/driver_init_wrapper.cpp @ONLY)

    set(MODEL_DRIVER_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/driver_init_wrapper.cpp)

    add_library(${MODEL_DRIVER_NAME} MODULE ${MODEL_DRIVER_SOURCES})
    set_target_properties(${MODEL_DRIVER_NAME} PROPERTIES OUTPUT_NAME "${PROJECT_NAME}-${KIM_API_MODEL_DRIVER_IDENTIFIER}"
                                                          LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${MODEL_DRIVER_NAME})

    target_link_libraries(${MODEL_DRIVER_NAME} ${KIM-API-V2_TARGET})

    install(TARGETS ${MODEL_DRIVER_NAME} LIBRARY DESTINATION "${KIM-API-V2_MODEL_DRIVER_INSTALL_PREFIX}/${MODEL_DRIVER_NAME}")
endfunction(add_kim_api_v2_model_driver_library)

cmake_minimum_required(VERSION 3.0)

project(kim-api VERSION 2.0.0 LANGUAGES C CXX Fortran)

include(GNUInstallDirs)

set(KIM_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(KIM_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(KIM_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(KIM_VERSION_PRERELEASE)
set(KIM_VERSION_BUILD_METADATA)

set(KIM_VERSION_STRING "${KIM_VERSION_MAJOR}.${KIM_VERSION_MINOR}.${KIM_VERSION_PATCH}")

if(KIM_VERSION_PRERELEASE)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}-${KIM_VERSION_PRERELEASE}")
endif()

if(KIM_VERSION_BUILD_METADATA)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}+${KIM_VERSION_BUILD_METADATA}")
endif()

set(PACKAGE_NAME "kim-api" CACHE STRING "TODO add description here")
set(FULL_PACKAGE_NAME "${PACKAGE_NAME}-v${KIM_VERSION_MAJOR}")
set(PACKAGE_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/${FULL_PACKAGE_NAME}")

set(LIB_ROOT       ${PACKAGE_NAME})
set(KIM_DIR        "${CMAKE_SOURCE_DIR}")
set(KIM_LIB        "${LIB_ROOT}-v${KIM_VERSION_MAJOR}")
set(KIM_LIB_BUILD  "${LIB_ROOT}-v${KIM_VERSION_STRING}")
set(MODEL_LIB       "${LIB_ROOT}-model-v${KIM_VERSION_MAJOR}")
set(MODEL_DRIVER_LIB "${LIB_ROOT}-model-driver-v${KIM_VERSION_MAJOR}")

set(MODELSDIR "models")
set(MODELDRIVERSDIR "model_drivers")

set(USER_CONFIG_FILE_ROOT_NAME "$ENV{HOME}" CACHE STRING "TODO add description")
set(USER_CONFIG_FILE_DIR_NAME ".${PACKAGE_NAME}" CACHE STRING "TODO add description")
mark_as_advanced(USER_CONFIG_FILE_ROOT_NAME)
mark_as_advanced(USER_CONFIG_FILE_DIR_NAME)

set(KIM_LOG_MAXIMUM_LEVEL "ERROR" CACHE STRING "TODO add description here")
set_property(CACHE KIM_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)

configure_file(cpp/include/KIM_LOG_DEFINES.inc.in KIM_LOG_DEFINES.inc)
configure_file(cpp/include/KIM_Version.hpp.in KIM_Version.hpp)
configure_file(c/include/KIM_Version.h.in KIM_Version.h)
configure_file(fortran/include/kim_version.fd.in kim_version.fd)


add_library(kim-api SHARED)
target_link_libraries(kim-api ${CMAKE_DL_LIBS})

add_subdirectory(c/src)
add_subdirectory(cpp/src)
add_subdirectory(fortran/src)
add_subdirectory(fortran/include)

add_subdirectory(cpp/src/utils)

install(TARGETS kim-api DESTINATION ${CMAKE_INSTALL_LIBDIR})

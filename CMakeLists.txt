cmake_minimum_required(VERSION 3.0)

set(PACKAGE_NAME "kim-api" CACHE STRING "TODO add description here")

project(${PACKAGE_NAME} VERSION 2.0.0 LANGUAGES C CXX Fortran)
include(GNUInstallDirs)

set(KIM_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(KIM_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(KIM_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(KIM_VERSION_PRERELEASE git)
set(KIM_VERSION_BUILD_METADATA)
# TODO : translate next line for metadata
# $(shell git rev-parse --short HEAD)$(shell if ! (git diff --quiet && git diff --cached --quiet); then printf ".dirty"; fi).$(KIM_COMPILERSUITE).$(KIM_SYSTEMLINKER).$(KIM_SYSTEMARCH).$(KIM_LINK)

set(KIM_VERSION_STRING "${KIM_VERSION_MAJOR}.${KIM_VERSION_MINOR}.${KIM_VERSION_PATCH}")

if(KIM_VERSION_PRERELEASE)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}-${KIM_VERSION_PRERELEASE}")
endif()

if(KIM_VERSION_BUILD_METADATA)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}+${KIM_VERSION_BUILD_METADATA}")
endif()

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

configure_file(Makefile.KIM_Config.in Makefile.KIM_Config)
configure_file(Makefile.Version.in Makefile.Version)

add_library(${FULL_PACKAGE_NAME} SHARED)
target_link_libraries(${FULL_PACKAGE_NAME} ${CMAKE_DL_LIBS})

add_subdirectory(c/src)
add_subdirectory(cpp/src)
add_subdirectory(fortran/src)
add_subdirectory(fortran/include)

add_subdirectory(cpp/src/utils)

install(TARGETS ${FULL_PACKAGE_NAME} DESTINATION ${CMAKE_INSTALL_LIBDIR})

install(FILES ${CMAKE_BINARY_DIR}/Makefile.KIM_Config DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME})
install(FILES ${CMAKE_BINARY_DIR}/Makefile.Version DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME})

set(BUILD_SYSTEM
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.Generic
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.LoadDefaults
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.ModelDriver
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.ParameterizedModel
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.SanityCheck
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.SimulatorModel
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.StandAloneModel
  )
install(FILES ${BUILD_SYSTEM} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system)

set(BUILD_COMPILER
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/compiler_defaults/Makefile.GCC
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/compiler_defaults/Makefile.INTEL
  )
install(FILES ${BUILD_COMPILER} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system/compiler_defaults)

set(BUILD_LINKER
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.DARWIN
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.FREEBSD
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.LINUX
  )
install(FILES ${BUILD_LINKER} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system/linker_defaults)

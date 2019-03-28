#
# Required preamble
#

cmake_minimum_required(VERSION 3.4)

list(APPEND CMAKE_PREFIX_PATH $ENV{KIM_API_CMAKE_PREFIX_DIR})
find_package(KIM-API 2.0 REQUIRED CONFIG)
if(NOT TARGET kim-api)
  enable_testing()
  project("${KIM_API_PROJECT_NAME}" VERSION "${KIM_API_VERSION}"
    LANGUAGES CXX C Fortran)
endif()

# End preamble


add_kim_api_model_library(
  NAME            "ex_sim_model_Si_mod_tersoff"
  METADATA_FILE   "kim-simulator-model.json"
  PARAMETER_FILES "Si.tersoff.mod"
  )

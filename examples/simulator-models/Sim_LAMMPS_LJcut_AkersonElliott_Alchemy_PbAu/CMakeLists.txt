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
  NAME            "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu"
  METADATA_FILE   "kim-simulator-model.edn"
  PARAMETER_FILES "Alchemy_PbAu.in"
  )

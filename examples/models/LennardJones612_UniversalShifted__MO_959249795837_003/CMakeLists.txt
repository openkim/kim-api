cmake_minimum_required(VERSION 3.0)
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    $ENV{KIM_DIR}/cmake/Modules
)

find_package(KIM REQUIRED)

kim_add_model(
    NAME "LennardJones612_UniversalShifted__MO_959249795837_003"
    DRIVER_NAME "LennardJones612__MD_414112407348_003"
    PARAMETER_FILES LennardJones612_UniversalShifted.params
)

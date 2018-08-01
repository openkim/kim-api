cmake_minimum_required(VERSION 3.0)
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    $ENV{KIM_DIR}/cmake/Modules
)

find_package(KIM REQUIRED)

kim_add_model_driver(
    NAME "LennardJones612__MD_414112407348_003"
    CREATE_FUNCTION_NAME "model_driver_create"
    CREATE_FUNCTION_LANG "cpp"
    SOURCES LennardJones612.cpp LennardJones612Implementation.cpp
)

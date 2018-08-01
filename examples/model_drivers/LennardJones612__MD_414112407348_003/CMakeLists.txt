cmake_minimum_required(VERSION 3.0)

list(APPEND CMAKE_MODULE_PATH $ENV{KIM_CMAKE_MODULES_DIR})
find_package(KIM 2 REQUIRED)

kim_add_model_driver(
    NAME "LennardJones612__MD_414112407348_003"
    CREATE_FUNCTION_NAME "model_driver_create"
    CREATE_FUNCTION_LANG "cpp"
    SOURCES LennardJones612.cpp LennardJones612Implementation.cpp
)

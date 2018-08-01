cmake_minimum_required(VERSION 3.0)

list(APPEND CMAKE_MODULE_PATH $ENV{KIM_CMAKE_MODULES_DIR})
find_package(KIM 2 REQUIRED)

kim_add_model(
    NAME "LennardJones_Ar"
    CREATE_FUNCTION_NAME "model_create"
    CREATE_FUNCTION_LANG "cpp"
    SOURCES LennardJones_Ar.cpp
)

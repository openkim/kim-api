cmake_minimum_required(VERSION 3.0)

list(APPEND CMAKE_MODULE_PATH $ENV{KIM_CMAKE_DIR})
find_package(KIM REQUIRED)

kim_add_model(
    NAME "ex_model_Ar_P_LJ"
    DRIVER_NAME "ex_model_driver_P_LJ"
    PARAMETER_FILES ex_model_Ar_P_LJ.params
)

cmake_minimum_required(VERSION 3.0)

list(APPEND CMAKE_MODULE_PATH $ENV{KIM_CMAKE_MODULES_DIR})
find_package(KIM 2 REQUIRED)

kim_add_model(
    NAME "ex_model_Ar_P_Morse"
    DRIVER_NAME "ex_model_driver_P_Morse"
    PARAMETER_FILES ex_model_Ar_P_Morse.params
)

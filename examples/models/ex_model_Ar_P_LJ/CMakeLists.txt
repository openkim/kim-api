cmake_minimum_required(VERSION 3.0)
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    $ENV{KIM_DIR}/cmake/Modules
)

find_package(KIM REQUIRED)

kim_add_parameterized_model(
    NAME "ex_model_Ar_P_LJ"
    DRIVER_NAME "ex_model_driver_P_LJ"
    PARAMETER_FILES ex_model_Ar_P_LJ.params
)

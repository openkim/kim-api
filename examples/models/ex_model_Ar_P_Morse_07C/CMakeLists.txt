cmake_minimum_required(VERSION 3.0)
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    $ENV{KIM_DIR}/cmake/Modules
)

find_package(KIM REQUIRED)

kim_add_standalone_model(
    NAME "ex_model_Ar_P_Morse_07C"
    CREATE_FUNCTION_NAME "model_create"
    CREATE_FUNCTION_LANG "c"
    SOURCES ex_model_Ar_P_Morse_07C.c
)

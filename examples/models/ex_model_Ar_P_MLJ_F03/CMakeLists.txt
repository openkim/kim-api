cmake_minimum_required(VERSION 3.0)
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    $ENV{KIM_DIR}/cmake/Modules
)

find_package(KIM REQUIRED)
enable_language(Fortran)
# workaround for F03 files, use F90 instead if possible
set_source_files_properties(ex_model_Ar_P_MLJ_F03.F03 PROPERTIES LANGUAGE Fortran)

kim_add_model(
    NAME "ex_model_Ar_P_MLJ_F03"
    CREATE_FUNCTION_NAME "model_create_routine"
    CREATE_FUNCTION_LANG "fortran"
    SOURCES ex_model_Ar_P_MLJ_F03.F03
)

cmake_minimum_required(VERSION 3.0)
find_package(PkgConfig)
pkg_check_modules(KIM-API REQUIRED libkim-api-v2)

set(TEST_NAME "ex_test_Ar_fcc_cluster_cpp")

link_directories(${KIM-API_LIBRARY_DIRS})

add_executable(${TEST_NAME} ${TEST_NAME}.cpp)
target_include_directories(${TEST_NAME} PRIVATE ${KIM-API_INCLUDE_DIRS})
target_link_libraries(${TEST_NAME} ${KIM-API_LIBRARIES})
target_compile_options(${TEST_NAME} PUBLIC ${KIM-API_CFLAGS})
target_compile_definitions(${TEST_NAME} PUBLIC ${KIM-API_DEFINITIONS})

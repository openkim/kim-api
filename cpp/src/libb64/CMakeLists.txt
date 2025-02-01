cmake_minimum_required(VERSION 3.13)
project(base64-encoder LANGUAGES CXX C)
set (CMAKE_CXX_STANDARD 98)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-std=c++98)

add_compile_options(-g)
add_compile_options(-O0)

add_subdirectory(libb64)
add_executable(${PROJECT_NAME} main.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC libb64/include/)
target_link_libraries(${PROJECT_NAME} PUBLIC b64)

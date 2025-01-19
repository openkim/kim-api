cmake_minimum_required(VERSION 3.10)

project(base64-encode VERSION 1.0)

set(CMAKE_CXX_STANDARD 98)
set(CMAKE_CXX_STANDARD_REQUIRED True)
add_compile_options("-std=c++98")  # Sometime the CMAKE_CXX_STANDARD does not work

set(SOURCE_FILES base64-encode.cpp)

# Add executable target
add_executable(${PROJECT_NAME} ${SOURCE_FILES})

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/utils
)

install(TARGETS ${PROJECT_NAME} DESTINATION bin)

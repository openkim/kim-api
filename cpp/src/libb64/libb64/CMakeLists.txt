message("BUILDING LIB-BASE64")

if(NOT DEFINED BUFFERSIZE) #  override this option with -DBUFFERSIZE=XXXX
    # set(BUFFERSIZE 4096)
    # set(BUFFERSIZE 65536)
    set(BUFFERSIZE 16777216)
endif()
message("LIB-BASE64 BUFFERSIZE = ${BUFFERSIZE}")

# Define the archiveQueue library
add_library(b64 STATIC EXCLUDE_FROM_ALL 
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cencode.c 
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cdecode.c
)
target_compile_definitions(b64 PUBLIC 
    -DBUFFERSIZE=${BUFFERSIZE}
)

# Include directories for archiveQueue and libarchive
target_include_directories(b64 PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include/
)

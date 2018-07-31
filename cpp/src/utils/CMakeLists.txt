set(BUILD_CONFIG           ${FULL_PACKAGE_NAME}-build-config)
set(COLLECTIONS_MANAGEMENT ${FULL_PACKAGE_NAME}-collections-management)
set(COLLECTIONS_INFO       ${FULL_PACKAGE_NAME}-collections-info)
set(ACTIVATE_SCRIPT        ${FULL_PACKAGE_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${FULL_PACKAGE_NAME}-deactivate)
set(SIMULATOR_MODEL        ${FULL_PACKAGE_NAME}-simulator-model)

configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT} @ONLY)
configure_file(activate.in ${ACTIVATE_SCRIPT} @ONLY)
configure_file(deactivate.in ${DEACTIVATE_SCRIPT} @ONLY)

add_executable(simulator-model simulator-model.cpp)
target_link_libraries(simulator-model kim-api)
set_target_properties(simulator-model PROPERTIES OUTPUT_NAME ${SIMULATOR_MODEL})

add_executable(collections-info collections-info.cpp)
target_link_libraries(collections-info kim-api)
set_target_properties(collections-info PROPERTIES OUTPUT_NAME ${COLLECTIONS_INFO})

add_executable(build-config build-config.cpp)
target_compile_definitions(build-config PRIVATE
  CC_STRING="${CMAKE_C_COMPILER}"
  CXX_STRING="${CMAKE_CXX_COMPILER}"
  FC_STRING="${CMAKE_Fortran_COMPILER}"
  LD_STRING="UNKNOWN-ld"
  INCLUDES_STRING="-I${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FULL_PACKAGE_NAME}"
  CFLAGS_STRING="${CMAKE_C_FLAGS}"
  CXXFLAGS_STRING="${CMAKE_CXX_FLAGS}"
  FFLAGS_STRING="${CMAKE_Fortran_FLAGS}"
  LDFLAGS_STRING="UNKNOWN-ldflags"
  LDLIBS_STRING="UNKNOWN-ldlibs"
  XLANGLDLIBS_STRING="UNKNOWN-xlangldlibs"
  FNOMAINFLAG_STRING="UNKNOWN-fnomainflag"
  OBJONLYFLAG_STRING="UNKNOWN-objonlyflag"
  OUTPUTINFLAG_STRING="UNKNOWN-outputinflag"
  MAKEFILEKIMCONFIG_STRING="${CMAKE_INSTALL_FULL_LIBDIR}/${FULL_PACKAGE_NAME}/Makefile.KIM_Config"
  LIBEXECPATH_STRING="${CMAKE_INSTALL_FULL_LIBEXECDIR}/${FULL_PACKAGE_NAME}"
  VERSION_STRING="${KIM_VERSION_STRING}"
  )
set_target_properties(build-config PROPERTIES OUTPUT_NAME ${BUILD_CONFIG})

install(TARGETS build-config DESTINATION  ${CMAKE_INSTALL_BINDIR})
install(TARGETS collections-info simulator-model DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/${FULL_PACKAGE_NAME})

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${ACTIVATE_SCRIPT}
           ${CMAKE_CURRENT_BINARY_DIR}/${COLLECTIONS_MANAGEMENT}
           ${CMAKE_CURRENT_BINARY_DIR}/${DEACTIVATE_SCRIPT}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
)

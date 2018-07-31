set(BUILD_CONFIG           ${FULL_PACKAGE_NAME}-build-config)
set(COLLECTIONS_MANAGEMENT ${FULL_PACKAGE_NAME}-collections-management)
set(COLLECTIONS_INFO       ${FULL_PACKAGE_NAME}-collections-info)
set(ACTIVATE_SCRIPT        ${FULL_PACKAGE_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${FULL_PACKAGE_NAME}-deactivate)
set(SIMULATOR_MODEL        ${FULL_PACKAGE_NAME}-simulator-model)
set(PKG_CONFIG_FILE        "lib${FULL_PACKAGE_NAME}.pc")

configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT} @ONLY)
configure_file(activate.in ${ACTIVATE_SCRIPT} @ONLY)
configure_file(deactivate.in ${DEACTIVATE_SCRIPT} @ONLY)
configure_file(libkim-api.pc.in ${PKG_CONFIG_FILE} @ONLY)

add_executable(simulator-model simulator-model.cpp)
target_link_libraries(simulator-model kim-api)
set_target_properties(simulator-model PROPERTIES OUTPUT_NAME ${SIMULATOR_MODEL})

add_executable(collections-info collections-info.cpp)
target_link_libraries(collections-info kim-api)
set_target_properties(collections-info PROPERTIES OUTPUT_NAME ${COLLECTIONS_INFO})

add_executable(build-config build-config.cpp)

set(LD_FORTRAN_LIB_STRING "")
foreach(impl_lib ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
    set(LD_FORTRAN_LIB_STRING "${LD_FORTRAN_LIB_STRING} -l${impl_lib}")
endforeach()

target_compile_definitions(build-config PRIVATE
  CC_STRING="${CMAKE_C_COMPILER}"
  CXX_STRING="${CMAKE_CXX_COMPILER}"
  FC_STRING="${CMAKE_Fortran_COMPILER}"
  LD_STRING="${CMAKE_CXX_COMPILER}"
  INCLUDES_STRING="-I${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FULL_PACKAGE_NAME}"
  CFLAGS_STRING="${CMAKE_C_FLAGS}"
  CXXFLAGS_STRING="${CMAKE_CXX_FLAGS}"
  FFLAGS_STRING="${CMAKE_Fortran_FLAGS}"
  LDFLAGS_STRING="-L${CMAKE_INSTALL_FULL_LIBDIR}"
  LDLIBS_STRING="-l${FULL_PACKAGE_NAME} ${LD_FORTRAN_LIB_STRING}"
  XLANGLDLIBS_STRING=""
  FNOMAINFLAG_STRING=""
  OBJONLYFLAG_STRING="-c"
  OUTPUTINFLAG_STRING="-o"
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

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PKG_CONFIG_FILE} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

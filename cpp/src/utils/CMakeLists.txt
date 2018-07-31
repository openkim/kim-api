set(COLLECTIONS_MANAGEMENT ${FULL_PACKAGE_NAME}-collections-management)
set(COLLECTIONS_INFO       ${FULL_PACKAGE_NAME}-collections-info)
set(ACTIVATE_SCRIPT        ${FULL_PACKAGE_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${FULL_PACKAGE_NAME}-deactivate)
set(SIMULATOR_MODEL        ${FULL_PACKAGE_NAME}-simulator-model)

configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT})
configure_file(activate.in ${ACTIVATE_SCRIPT})
configure_file(deactivate.in ${DEACTIVATE_SCRIPT})

add_executable(${SIMULATOR_MODEL} simulator-model.cpp)
target_link_libraries(${SIMULATOR_MODEL} ${FULL_PACKAGE_NAME})

add_executable(${COLLECTIONS_INFO} collections-info.cpp)
target_link_libraries(${COLLECTIONS_INFO} ${FULL_PACKAGE_NAME})

install(TARGETS ${COLLECTIONS_INFO} ${SIMULATOR_MODEL} DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/${FULL_PACKAGE_NAME})

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${ACTIVATE_SCRIPT}
        ${CMAKE_CURRENT_BINARY_DIR}/${COLLECTIONS_MANAGEMENT}
        ${CMAKE_CURRENT_BINARY_DIR}/${DEACTIVATE_SCRIPT}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
)

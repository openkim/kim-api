set(COLLECTIONS_MANAGEMENT ${FULL_PACKAGE_NAME}-collections-management)
set(ACTIVATE_SCRIPT        ${FULL_PACKAGE_NAME}-activate.sh)
set(DEACTIVATE_SCRIPT      ${FULL_PACKAGE_NAME}-deactivate.sh)
set(SIMULATOR_MODEL        ${FULL_PACKAGE_NAME}-simulator-model)

configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT})
configure_file(activate.sh.in ${ACTIVATE_SCRIPT})
configure_file(deactivate.sh.in ${DEACTIVATE_SCRIPT})

add_executable(${SIMULATOR_MODEL} simulator-model.cpp)
target_link_libraries(${SIMULATOR_MODEL} kim-api)

install(
  FILES ${COLLECTIONS_MANAGEMENT} ${ACTIVATE_SCRIPT} ${DEACTIVATE_SCRIPT}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
)

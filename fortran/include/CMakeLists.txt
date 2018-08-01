set(F_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_charge_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_argument_name_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_arguments_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_callback_name_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_data_type_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_energy_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_language_name_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_length_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_verbosity_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_headers_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_headers_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_simulator_headers_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_unit_system_module.f90)

target_sources(kim-api PRIVATE ${F_SOURCES})

set(FORTRAN_INCLUDES
    ${CMAKE_BINARY_DIR}/kim_charge_unit_module.mod
    ${CMAKE_BINARY_DIR}/kim_compute_argument_name_module.mod
    ${CMAKE_BINARY_DIR}/kim_compute_arguments_module.mod
    ${CMAKE_BINARY_DIR}/kim_compute_callback_name_module.mod
    ${CMAKE_BINARY_DIR}/kim_data_type_module.mod
    ${CMAKE_BINARY_DIR}/kim_energy_unit_module.mod
    ${CMAKE_BINARY_DIR}/kim_language_name_module.mod
    ${CMAKE_BINARY_DIR}/kim_length_unit_module.mod
    ${CMAKE_BINARY_DIR}/kim_log_module.mod
    ${CMAKE_BINARY_DIR}/kim_log_verbosity_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_compute_arguments_create_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_compute_arguments_destroy_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_compute_arguments_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_compute_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_create_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_destroy_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_driver_create_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_driver_headers_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_headers_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_module.mod
    ${CMAKE_BINARY_DIR}/kim_model_refresh_module.mod
    ${CMAKE_BINARY_DIR}/kim_numbering_module.mod
    ${CMAKE_BINARY_DIR}/kim_sem_ver_module.mod
    ${CMAKE_BINARY_DIR}/kim_simulator_headers_module.mod
    ${CMAKE_BINARY_DIR}/kim_species_name_module.mod
    ${CMAKE_BINARY_DIR}/kim_support_status_module.mod
    ${CMAKE_BINARY_DIR}/kim_temperature_unit_module.mod
    ${CMAKE_BINARY_DIR}/kim_time_unit_module.mod
    ${CMAKE_BINARY_DIR}/kim_unit_system_module.mod
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_log_macros.fd
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_log_macros.fd
    ${CMAKE_BINARY_DIR}/kim_version.fd
)

target_include_directories(kim-api PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

install(FILES ${FORTRAN_INCLUDES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${FULL_PACKAGE_NAME})

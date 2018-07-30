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
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_module.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kim_unit_system_module.f90)

target_sources(kim-api PRIVATE ${F_SOURCES})

configure_file(kim_version.fd.in kim_version.fd)

set(F_SOURCES
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_charge_unit_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_argument_name_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_arguments_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_callback_name_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_data_type_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_energy_unit_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_language_name_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_length_unit_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_verbosity_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_f.f90
   ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_f.f90)

target_sources(kim-api PRIVATE ${F_SOURCES})

target_include_directories(kim-api PRIVATE ../include)

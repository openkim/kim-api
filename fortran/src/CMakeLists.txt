set(F_SOURCES
   kim_charge_unit_f.f90
   kim_compute_argument_name_f.f90
   kim_compute_arguments_f.f90
   kim_compute_callback_name_f.f90
   kim_data_type_f.f90
   kim_energy_unit_f.f90
   kim_language_name_f.f90
   kim_length_unit_f.f90
   kim_log_f.f90
   kim_log_verbosity_f.f90
   kim_model_compute_arguments_create_f.f90
   kim_model_compute_arguments_destroy_f.f90
   kim_model_compute_arguments_f.f90
   kim_model_compute_f.f90
   kim_model_create_f.f90
   kim_model_destroy_f.f90
   kim_model_driver_create_f.f90
   kim_model_f.f90
   kim_model_refresh_f.f90
   kim_numbering_f.f90
   kim_sem_ver_f.f90
   kim_species_name_f.f90
   kim_support_status_f.f90
   kim_temperature_unit_f.f90
   kim_time_unit_f.f90)

add_library(fortran_impl OBJECT ${F_SOURCES})

target_include_directories(fortran_impl PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../include)
add_dependencies(fortran_impl fortran_mods)

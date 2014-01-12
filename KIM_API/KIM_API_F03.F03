#include "KIM_API_status.h"

#define DP 8

module kim_api_f03
  implicit none

  integer, parameter :: KIM_KEY_STRING_LENGTH = 64

  interface


    integer(c_int) function kim_api_init(kimmdl, testname, mdlname) bind(c, name="KIM_API_init")
      use, intrinsic :: iso_c_binding

      type(c_ptr)       :: kimmdl
      character(c_char) :: testname(*)
      character(c_char) :: mdlname(*)
    end function kim_api_init


    integer(c_int) function kim_api_string_init(kimmdl, testdescriptor, modelname) bind(c, name="KIM_API_string_init")  ! untested
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: testdescriptor(*)
      character(c_char)       :: modelname(*)
    end function kim_api_string_init


    integer(c_int) function kim_api_model_info(kimmdl, modelname) bind(c, name="KIM_API_model_info")  ! untested
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: modelname(*)
    end function kim_api_model_info


    subroutine kim_api_allocate_f03(kimmdl, natoms, ntypes, error) bind(c, name="KIM_API_allocate")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: natoms
      integer(c_int), value :: ntypes
      integer(c_int)        :: error
    end subroutine kim_api_allocate_f03


    subroutine kim_api_free_f03(kimmdl, error) bind(c, name="KIM_API_free")
      use, intrinsic :: iso_c_binding

      type(c_ptr)    :: kimmdl
      integer(c_int) :: error
    end subroutine kim_api_free_f03


    subroutine kim_api_print_f03(kimmdl, error) bind(c, name="KIM_API_print")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end subroutine kim_api_print_f03


    integer(c_int) function kim_api_model_compute_f03(kimmdl) bind(c, name="KIM_API_model_compute")
      use, intrinsic :: iso_c_binding

      type(c_ptr), value :: kimmdl
    end function kim_api_model_compute_f03


    integer(c_int) function kim_api_model_destroy_f03(kimmdl) bind(c, name="KIM_API_model_destroy")
      use, intrinsic :: iso_c_binding

      type(c_ptr), value :: kimmdl
    end function kim_api_model_destroy_f03


    integer(c_int) function kim_api_get_model_index_shift_f03(kimmdl) bind(c, name="KIM_API_get_model_index_shift")
      use, intrinsic :: iso_c_binding

      type(c_ptr), value :: kimmdl
    end function kim_api_get_model_index_shift_f03


    subroutine kim_api_set_model_buffer_f03(kimmdl, ob, ier) bind(c, name="KIM_API_set_model_buffer")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_model_buffer_f03


    subroutine kim_api_set_test_buffer_f03(kimmdl, ob, ier) bind(c, name="KIM_API_set_test_buffer")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_test_buffer_f03


    type(c_ptr) function kim_api_get_model_buffer_f03(kimmdl, ier) bind(c, name="KIM_API_get_model_buffer")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_model_buffer_f03


    type(c_ptr) function kim_api_get_test_buffer_f03(kimmdl, ier) bind(c, name="KIM_API_get_test_buffer")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_test_buffer_f03


    integer(c_int) function kim_api_is_half_neighbors_f03(kimmdl, error) bind(c, name="KIM_API_is_half_neighbors")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_is_half_neighbors_f03


    integer(c_int) function kim_api_set_data(kimmdl, nm, size, dt) bind(c, name="KIM_API_set_data")
      use, intrinsic :: iso_c_binding

      type(c_ptr),         value :: kimmdl
      character(c_char)          :: nm(*)
      integer(c_intptr_t), value :: size
      type(c_ptr),         value :: dt
    end function kim_api_set_data


    integer(c_int) function kim_api_set_method(kimmdl, nm, size, dt) bind(c, name="KIM_API_set_method")
      use, intrinsic :: iso_c_binding

      type(c_ptr),         value :: kimmdl
      character(c_char)          :: nm(*)
      integer(c_intptr_t), value :: size
      type(c_funptr),      value :: dt
    end function kim_api_set_method


    type(c_ptr) function kim_api_get_data(kimmdl, nm, error) bind(c, name="KIM_API_get_data")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_data


    type(c_funptr) function kim_api_get_method(kimmdl, nm, error) bind(c, name="KIM_API_get_method")  ! untested
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_method


    integer(c_intptr_t) function kim_api_get_size(kimmdl, nm, error) bind(c, name="KIM_API_get_size")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_size


    integer(c_intptr_t) function kim_api_get_rank(kimmdl, nm, error) bind(c, name="KIM_API_get_rank")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_rank


    integer(c_intptr_t) function kim_api_get_shape(kimmdl, nm, shape, error) bind(c, name="KIM_API_get_shape")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: shape(*)
      integer(c_int)          :: error
    end function kim_api_get_shape


    subroutine kim_api_set_shape(kimmdl, nm, shape, rank, error) bind(c, name="KIM_API_set_shape")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: shape(*)
      integer(c_int),   value :: rank
      integer(c_int)          :: error
    end subroutine kim_api_set_shape


    subroutine kim_api_set_compute(kimmdl, nm, flag, error) bind(c, name="KIM_API_set_compute")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int),   value :: flag
      integer(c_int)          :: error
    end subroutine kim_api_set_compute


    integer(c_int) function kim_api_get_compute(kimmdl, nm, error) bind(c, name="KIM_API_get_compute")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_compute


    integer(c_int) function kim_api_get_index(kimmdl, nm, error) bind(c, name="KIM_API_get_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_index


    integer(c_int) function kim_api_model_init_f03(kimmdl) bind(c, name="KIM_API_model_init")
      use, intrinsic :: iso_c_binding

      type(c_ptr), value :: kimmdl
    end function kim_api_model_init_f03


    integer(c_int) function kim_api_model_reinit_f03(kimmdl) bind(c, name="KIM_API_model_reinit")
      use, intrinsic :: iso_c_binding

      type(c_ptr), value :: kimmdl
    end function kim_api_model_reinit_f03


    type(c_ptr) function kim_api_get_model_partcl_typs_f03(kimmdl, nptypes, error) bind(c, name="KIM_API_get_model_partcl_typs")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nptypes
      integer(c_int)       :: error
    end function kim_api_get_model_partcl_typs_f03


    type(c_ptr) function kim_api_get_test_partcl_typs_f03(kimmdl, nptypes, error) bind(c, name="KIM_API_get_test_partcl_typs")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nptypes
      integer(c_int)       :: error
    end function kim_api_get_test_partcl_typs_f03


    type(c_ptr) function kim_api_get_params_f03(kimmdl, nvpar, error) bind(c, name="KIM_API_get_params")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_params_f03


    type(c_ptr) function kim_api_get_free_params_f03(kimmdl, nvpar, error) bind(c, name="KIM_API_get_free_params")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_free_params_f03


    type(c_ptr) function kim_api_get_fixed_params_f03(kimmdl, nvpar, error) bind(c, name="KIM_API_get_fixed_params")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_fixed_params_f03


    type(c_ptr) function kim_api_get_nbc_method_f03(kimmdl, error) bind(c, name="KIM_API_get_NBC_method")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_nbc_method_f03


    integer(c_int) function kim_api_get_partcl_type_code(kimmdl, particle, error) bind(c, name="KIM_API_get_partcl_type_code")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: particle(*)
      integer(c_int)          :: error
    end function kim_api_get_partcl_type_code


    subroutine kim_api_set_partcl_type_code(kimmdl, particle, code, error) bind(c, name="KIM_API_set_partcl_type_code")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: particle(*)
      integer(c_int),   value :: code
      integer(c_int)          :: error
    end subroutine kim_api_set_partcl_type_code


    type(c_ptr) function kim_api_get_model_kim_str(modelname, error) bind(c, name="KIM_API_get_model_kim_str")  ! untested
      use, intrinsic :: iso_c_binding

      character(c_char) :: modelname(*)
      integer(c_int)    :: error
    end function kim_api_get_model_kim_str


    integer(c_int) function kim_api_get_neigh_mode_f03(kimmdl, error) bind(c, name="KIM_API_get_neigh_mode")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_neigh_mode_f03


    integer(c_int) function kim_api_get_neigh_f03(kimmdl, mode, request, atom, numnei, nei1atom, rij) bind(c, name="KIM_API_get_neigh")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: mode
      integer(c_int), value :: request
      integer(c_int)        :: atom
      integer(c_int)        :: numnei
      type(c_ptr)           :: nei1atom
      type(c_ptr)           :: rij
    end function kim_api_get_neigh_f03


    type(c_ptr) function kim_api_get_status_msg_f03(status_code) bind(c, name="KIM_API_get_status_msg")
      use, intrinsic :: iso_c_binding

      integer(c_int), value :: status_code
    end function kim_api_get_status_msg_f03


    integer(c_int) function kim_api_report_error(ln, fl, usermsg, status_code) bind(c, name="KIM_API_report_error")
      use, intrinsic :: iso_c_binding

      integer(c_int),   value :: ln
      character(c_char)       :: fl(*)
      character(c_char)       :: usermsg(*)
      integer(c_int),   value :: status_code
    end function kim_api_report_error


    real(c_double) function kim_api_get_scale_conversion(u_from, u_to, error) bind(c, name="KIM_API_get_scale_conversion")
      use, intrinsic :: iso_c_binding

      character(c_char) :: u_from(*)
      character(c_char) :: u_to(*)
      integer(c_int)    :: error
    end function kim_api_get_scale_conversion


    integer(c_int) function kim_api_get_unit_handling_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_handling")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_handling_f03


    type(c_ptr) function kim_api_get_unit_length_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_length")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_length_f03


    type(c_ptr) function kim_api_get_unit_energy_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_energy")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_energy_f03


    type(c_ptr) function kim_api_get_unit_charge_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_charge")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_charge_f03


    type(c_ptr) function kim_api_get_unit_temperature_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_temperature")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_temperature_f03


    type(c_ptr) function kim_api_get_unit_time_f03(kimmdl, error) bind(c, name="KIM_API_get_unit_time")
      use, intrinsic :: iso_c_binding

      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_time_f03


    real(c_double) function kim_api_convert_to_act_unit(kimmdl, &
                                                        length, &
                                                        energy, &
                                                        charge, &
                                                        temperature, &
                                                        time, &
                                                        length_exponent, &
                                                        energy_exponent, &
                                                        charge_exponent, &
                                                        temperature_exponent, &
                                                        time_exponent, &
                                                        error) &
                                                        bind(c, name="KIM_API_convert_to_act_unit")
      use, intrinsic :: iso_c_binding

      type(c_ptr),      value :: kimmdl
      character(c_char)       :: length(*)
      character(c_char)       :: energy(*)
      character(c_char)       :: charge(*)
      character(c_char)       :: temperature(*)
      character(c_char)       :: time(*)
      real(c_double),   value :: length_exponent
      real(c_double),   value :: energy_exponent
      real(c_double),   value :: charge_exponent
      real(c_double),   value :: temperature_exponent
      real(c_double),   value :: time_exponent
      integer(c_int)          :: error
    end function kim_api_convert_to_act_unit


    integer(c_int) function kim_api_set_data_by_index(kimmdl, I, size, dt) bind(c, name="KIM_API_set_data_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),         value :: kimmdl
      integer(c_int),      value :: I
      integer(c_intptr_t), value :: size
      type(c_ptr),         value :: dt
    end function kim_api_set_data_by_index


    integer(c_int) function kim_api_set_method_by_index(kimmdl, I, size, dt) bind(c, name="KIM_API_set_method_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),         value :: kimmdl
      integer(c_int),      value :: I
      integer(c_intptr_t), value :: size
      type(c_funptr),      value :: dt
    end function kim_api_set_method_by_index


    type(c_ptr) function kim_api_get_data_by_index_f03(kimmdl, I, error) bind(c, name="KIM_API_get_data_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_data_by_index_f03


    type(c_funptr) function kim_api_get_method_by_index_f03(kimmdl, I, error) bind(c, name="KIM_API_get_method_by_index")  ! untested
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_method_by_index_f03


    integer(c_intptr_t) function kim_api_get_size_by_index_f03(kimmdl, I, error) bind(c, name="KIM_API_get_size_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_size_by_index_f03


    integer(c_intptr_t) function kim_api_get_shape_by_index(kimmdl, I, shape, error) bind(c, name="KIM_API_get_shape_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: shape(*)
      integer(c_int)        :: error
    end function kim_api_get_shape_by_index


    integer(c_intptr_t) function kim_api_get_rank_by_index_f03(kimmdl, I, error) bind(c, name="KIM_API_get_rank_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_rank_by_index_f03


    subroutine kim_api_set_compute_by_index_f03(kimmdl, I, flag, error) bind(c, name="KIM_API_set_compute_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int), value :: flag
      integer(c_int)        :: error
    end subroutine kim_api_set_compute_by_index_f03


    integer(c_int) function kim_api_get_compute_by_index_f03(kimmdl, I, error) bind(c, name="KIM_API_get_compute_by_index")
      use, intrinsic :: iso_c_binding

      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_compute_by_index_f03


  end interface



  contains



  integer function kim_api_init_f03(kimmdl, testname, mdlname)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: testname
    character(len=*), intent(in) :: mdlname

    kim_api_init_f03 = kim_api_init(kimmdl, trim(testname)//c_null_char, trim(mdlname)//c_null_char)

  end function kim_api_init_f03


  integer function kim_api_string_init_f03(kimmdl, testdescriptor, modelname)  ! untested
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: testdescriptor
    character(len=*), intent(in) :: modelname

    kim_api_string_init_f03 = kim_api_string_init(kimmdl, trim(testdescriptor)//c_null_char, trim(modelname)//c_null_char)

  end function kim_api_string_init_f03


  integer function kim_api_model_info_f03(kimmdl, modelname)  ! untested
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: modelname

    kim_api_model_info_f03 = kim_api_model_info(kimmdl, trim(modelname)//c_null_char)

  end function kim_api_model_info_f03


  integer function kim_api_set_data_f03(kimmdl, nm, size, dt)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer,          intent(in) :: size
    type(c_ptr),      intent(in) :: dt

    integer(c_intptr_t) :: size_int


    size_int = size

    kim_api_set_data_f03 = kim_api_set_data(kimmdl, trim(nm)//c_null_char, size_int, dt)

  end function kim_api_set_data_f03


  integer function kim_api_set_method_f03(kimmdl, nm, size, dt)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer,          intent(in) :: size
    type(c_funptr),   intent(in) :: dt

    integer(c_intptr_t) :: size_int


    size_int = size

    kim_api_set_method_f03 = kim_api_set_method(kimmdl, trim(nm)//c_null_char, size_int, dt)

  end function kim_api_set_method_f03


  type(c_ptr) function kim_api_get_data_f03(kimmdl, nm, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(out) :: error

    kim_api_get_data_f03 = kim_api_get_data(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_data_f03


  type(c_funptr) function kim_api_get_method_f03(kimmdl, nm, error)  ! untested
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer,          intent(in) :: error

    kim_api_get_method_f03 = kim_api_get_method(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_method_f03


  integer function kim_api_get_size_f03(kimmdl, nm, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer,          intent(in) :: error

    kim_api_get_size_f03 = kim_api_get_size(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_size_f03


  integer function kim_api_get_rank_f03(kimmdl, nm, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer,          intent(in) :: error

    kim_api_get_rank_f03 = kim_api_get_rank(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_rank_f03


  subroutine kim_api_get_shape_f03(kimmdl, nm, shape, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(out) :: shape(*)
    integer,          intent(out) :: error


    integer              :: rank
    integer, allocatable :: shape_tmp(:)
    integer              :: i


    rank = kim_api_get_shape(kimmdl, trim(nm)//c_null_char, shape, error)


    allocate(shape_tmp(rank))

    do i=1,rank
      shape_tmp(i) = shape(rank - i + 1)  ! transpose shape
    end do

    shape(1:rank) = shape_tmp(1:rank)

    deallocate(shape_tmp)

  end subroutine kim_api_get_shape_f03


  subroutine kim_api_set_shape_f03(kimmdl, nm, shape, rank, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(in)  :: shape(*)
    integer,          intent(in)  :: rank
    integer,          intent(out) :: error

    integer :: shape_t(rank)
    integer :: i

    if (rank .gt. 1) then
      do i=1,rank
        shape_t(i) = shape(rank - i + 1)  ! transpose shape
      end do
    end if

    call kim_api_set_shape(kimmdl, trim(nm)//c_null_char, shape_t, rank, error)

  end subroutine kim_api_set_shape_f03


  subroutine kim_api_set_compute_f03(kimmdl, nm, flag, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(in)  :: flag
    integer,          intent(out) :: error

    call  kim_api_set_compute(kimmdl, trim(nm)//c_null_char, flag, error)

  end subroutine kim_api_set_compute_f03


  integer function kim_api_get_compute_f03(kimmdl, nm, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(out) :: error

    kim_api_get_compute_f03 = kim_api_get_compute(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_compute_f03


  integer function kim_api_get_index_f03(kimmdl, nm, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer,          intent(out) :: error

    kim_api_get_index_f03 = kim_api_get_index(kimmdl, trim(nm)//c_null_char, error)

  end function kim_api_get_index_f03


  integer function kim_api_get_partcl_type_code_f03(kimmdl, particle, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer,          intent(out) :: error

    kim_api_get_partcl_type_code_f03 = kim_api_get_partcl_type_code(kimmdl, trim(particle)//c_null_char, error)
  end function kim_api_get_partcl_type_code_f03


  subroutine kim_api_set_partcl_type_code_f03(kimmdl, particle, code, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer,          intent(in)  :: code
    integer,          intent(out) :: error

    call kim_api_set_partcl_type_code(kimmdl, trim(particle)//c_null_char, code, error)

  end subroutine kim_api_set_partcl_type_code_f03

  
  type(c_ptr) function kim_api_get_model_kim_str_f03(modelname, error)  ! untested
    use, intrinsic :: iso_c_binding

    implicit none

    character(len=*), intent(in)  :: modelname
    integer,          intent(out) :: error

    kim_api_get_model_kim_str_f03 = kim_api_get_model_kim_str(trim(modelname)//c_null_char, error)

  end function kim_api_get_model_kim_str_f03


  integer function kim_api_report_error_f03(ln, fl, usermsg, status_code)
    use, intrinsic :: iso_c_binding

    implicit none

    integer,          intent(in) :: ln
    character(len=*), intent(in) :: fl
    character(len=*), intent(in) :: usermsg
    integer,          intent(in) :: status_code


    kim_api_report_error_f03 = kim_api_report_error(ln, trim(fl)//c_null_char, trim(usermsg)//c_null_char, status_code)

  end function kim_api_report_error_f03


  real(DP) function kim_api_get_scale_conversion_f03(u_from, u_to, error)
    use, intrinsic :: iso_c_binding

    implicit none

    character(len=*), intent(in)  :: u_from
    character(len=*), intent(in)  :: u_to
    integer,          intent(out) :: error

    kim_api_get_scale_conversion_f03 = kim_api_get_scale_conversion(trim(u_from)//c_null_char, trim(u_to)//c_null_char, error)

  end function kim_api_get_scale_conversion_f03


  real(DP) function kim_api_convert_to_act_unit_f03(kimmdl, &
                                                    length, &
                                                    energy, &
                                                    charge, &
                                                    temperature, &
                                                    time, &
                                                    length_exponent, &
                                                    energy_exponent, &
                                                    charge_exponent, &
                                                    temperature_exponent, &
                                                    time_exponent, &
                                                    error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: length
    character(len=*), intent(in)  :: energy
    character(len=*), intent(in)  :: charge
    character(len=*), intent(in)  :: temperature
    character(len=*), intent(in)  :: time
    real(DP),         intent(in)  :: length_exponent
    real(DP),         intent(in)  :: energy_exponent
    real(DP),         intent(in)  :: charge_exponent
    real(DP),         intent(in)  :: temperature_exponent
    real(DP),         intent(in)  :: time_exponent
    integer,          intent(out) :: error

    kim_api_convert_to_act_unit_f03 = &
      kim_api_convert_to_act_unit( &
        kimmdl, &
        trim(length)//c_null_char, &
        trim(energy)//c_null_char, &
        trim(charge)//c_null_char, &
        trim(temperature)//c_null_char, &
        trim(time)//c_null_char, &
        length_exponent, &
        energy_exponent, &
        charge_exponent, &
        temperature_exponent, &
        time_exponent, &
        error &
      )

  end function kim_api_convert_to_act_unit_f03


  integer function kim_api_set_data_by_index_f03(kimmdl, I, size, dt)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr), intent(in) :: kimmdl
    integer,     intent(in) :: I
    integer,     intent(in) :: size
    type(c_ptr), intent(in) :: dt
    
    integer(c_intptr_t) :: size_int


    size_int = size

    kim_api_set_data_by_index_f03 = kim_api_set_data_by_index(kimmdl, I, size_int, dt)

  end function kim_api_set_data_by_index_f03


  integer function kim_api_set_method_by_index_f03(kimmdl, I, size, dt)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr),    intent(in) :: kimmdl
    integer,        intent(in) :: I
    integer,        intent(in) :: size
    type(c_funptr), intent(in) :: dt

    integer(c_intptr_t) :: size_int


    size_int = size

    kim_api_set_method_by_index_f03 = kim_api_set_method_by_index(kimmdl, I, size_int, dt)

  end function kim_api_set_method_by_index_f03


  subroutine kim_api_get_shape_by_index_f03(kimmdl, I, shape, error)
    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr), intent(in)  :: kimmdl
    integer,     intent(in)  :: I
    integer,     intent(out) :: shape(*)
    integer,     intent(out) :: error

    integer              :: rank
    integer, allocatable :: shape_tmp(:)
    integer              :: j


    rank = kim_api_get_shape_by_index(kimmdl, I, shape, error)


    allocate(shape_tmp(rank))

    do j=1,rank
      shape_tmp(j) = shape(rank - j + 1)  ! transpose shape
    end do

    shape(1:rank) = shape_tmp(1:rank)

    deallocate(shape_tmp)

  end subroutine kim_api_get_shape_by_index_f03


  integer function errcheck_mltpl_f03(error, msgfrom, grarg, nm, ind)
    use, intrinsic :: iso_c_binding

    implicit none

    integer                     :: error
    character(len=*)            :: msgfrom
    integer                     :: grarg
    character(c_char), optional :: nm(*)
    integer,           optional :: ind

    errcheck_mltpl_f03 = KIM_STATUS_OK

    if (error .ge. KIM_STATUS_OK) return

    errcheck_mltpl_f03 = KIM_STATUS_FAIL

    if (present(nm) .and. present(ind)) then
      print*, "failed:", msgfrom, ", for group argument ", grarg, " and kim_name "!, nm, ", kim_index", ind
    else if (present(nm)) then
      print*, "failed:", msgfrom, ", for group argument ", grarg, " and kim_name "!, nm
    else if (present(ind)) then
      print*, "failed:", msgfrom, ", for group argument ", grarg, " and kim_index"!, ind
    else
      print*, "failed:", msgfrom, ", for group argument ", grarg
    endif

    errcheck_mltpl_f03 = KIM_STATUS_FAIL

  end function errcheck_mltpl_f03


  subroutine kim_api_setm_method_f03(kimmdl, error, &
                                     nm1,  sz1,  dt1,  k1, &
                                     nm2,  sz2,  dt2,  k2, &
                                     nm3,  sz3,  dt3,  k3, &
                                     nm4,  sz4,  dt4,  k4, &
                                     nm5,  sz5,  dt5,  k5, &
                                     nm6,  sz6,  dt6,  k6, &
                                     nm7,  sz7,  dt7,  k7, &
                                     nm8,  sz8,  dt8,  k8, &
                                     nm9,  sz9,  dt9,  k9, &
                                     nm10, sz10, dt10, k10, &
                                     nm11, sz11, dt11, k11, &
                                     nm12, sz12, dt12, k12, &
                                     nm13, sz13, dt13, k13, &
                                     nm14, sz14, dt14, k14, &
                                     nm15, sz15, dt15, k15 &
                                     )

    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr) :: kimmdl
    integer     :: error

    character(len=*)           :: nm1
    character(len=*), optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10,nm11,nm12,nm13,nm14,nm15

    integer                    :: sz1
    integer,          optional :: sz2,sz3,sz4,sz5,sz6,sz7,sz8,sz9,sz10,sz11,sz12,sz13,sz14,sz15

    type(c_funptr)             :: dt1
    type(c_funptr),   optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10,dt11,dt12,dt13,dt14,dt15

    integer                    :: k1
    integer,          optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
    
    character(len=40) :: msg = "kim_api_setm_method_f03"


    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl_f03(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1,nm1) .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      error = kim_api_set_method_f03(kimmdl, nm1, sz1, dt1)
      if (errcheck_mltpl_f03(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. (.not.present(sz2) .or. .not. present(dt2) .or. .not.present(k2))) then
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. (.not.present(sz3) .or. .not.present(dt3) .or. .not.present(k3))) then
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. (.not.present(sz4) .or. .not.present(dt4) .or. .not.present(k4))) then
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. (.not.present(sz5) .or. .not.present(dt5) .or. .not.present(k5))) then
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if( present(nm6) .and. (.not.present(sz6) .or. .not.present(dt6) .or. .not.present(k6))) then
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if( present(nm7) .and. (.not.present(sz7) .or. .not.present(dt7) .or. .not.present(k7))) then
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if( present(nm8) .and. (.not.present(sz8) .or. .not.present(dt8) .or. .not.present(k8))) then
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if( present(nm9) .and. (.not.present(sz9) .or. .not.present(dt9) .or. .not.present(k9))) then
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if( present(nm10) .and. (.not.present(sz10) .or. .not.present(dt10) .or. .not.present(k10))) then
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if( present(nm11) .and. (.not.present(sz11) .or. .not.present(dt11) .or. .not.present(k11))) then
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if( present(nm12) .and. (.not.present(sz12) .or. .not.present(dt12) .or. .not.present(k12))) then
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if( present(nm13) .and. (.not.present(sz13) .or. .not.present(dt13) .or. .not.present(k13))) then
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if( present(nm14) .and. (.not.present(sz14) .or. .not.present(dt14) .or. .not.present(k14))) then
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if( present(nm15) .and. (.not.present(sz15) .or. .not.present(dt15) .or. .not.present(k15))) then
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1)))then
      if (errcheck_mltpl_f03(error, msg, 2,  nm2) .lt. KIM_STATUS_OK) return
    else if(present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 3,  nm3) .lt. KIM_STATUS_OK) return
    else if(present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 4,  nm4) .lt. KIM_STATUS_OK) return
    else if(present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 5,  nm5) .lt. KIM_STATUS_OK) return
    else if(present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 6,  nm6) .lt. KIM_STATUS_OK) return
    else if(present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 7,  nm7) .lt. KIM_STATUS_OK) return
    else if(present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 8,  nm8) .lt. KIM_STATUS_OK) return
    else if(present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 9,  nm9) .lt. KIM_STATUS_OK) return
    else if(present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 10,  nm10) .lt. KIM_STATUS_OK) return
    else if(present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 11,  nm11) .lt. KIM_STATUS_OK) return
    else if(present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 12,  nm12) .lt. KIM_STATUS_OK) return
    else if(present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 13,  nm13) .lt. KIM_STATUS_OK) return
    else if(present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1)))then
      if  (errcheck_mltpl_f03(error, msg, 14,  nm14) .lt. KIM_STATUS_OK) return
    else if(present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1)))then
      if (errcheck_mltpl_f03(error, msg, 15,  nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error = KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm2, sz2, dt2);
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm3, sz3, dt3);
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm4, sz4, dt4);
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm5, sz5, dt5);
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm6, sz6, dt6);
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm7, sz7, dt7);
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm8, sz8, dt8);
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm9, sz9, dt9);
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm10, sz10, dt10);
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm11, sz11, dt11);
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm12, sz12, dt12);
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm13, sz13, dt13);
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm14, sz14, dt14);
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) error=kim_api_set_method_f03(kimmdl, nm15, sz15, dt15);
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return

  end subroutine kim_api_setm_method_f03


  subroutine kim_api_getm_compute_f03(kimmdl, error, &
                                      nm1,  dt1,  k1, &
                                      nm2,  dt2,  k2, &
                                      nm3,  dt3,  k3, &
                                      nm4,  dt4,  k4, &
                                      nm5,  dt5,  k5, &
                                      nm6,  dt6,  k6, &
                                      nm7,  dt7,  k7, &
                                      nm8,  dt8,  k8, &
                                      nm9,  dt9,  k9, &
                                      nm10, dt10, k10, &
                                      nm11, dt11, k11, &
                                      nm12, dt12, k12, &
                                      nm13, dt13, k13, &
                                      nm14, dt14, k14, &
                                      nm15, dt15, k15 &
                                      )

    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr) :: kimmdl
    integer     :: error

    character(len=*)              :: nm1
    character(len=*),    optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10,nm11,nm12,nm13,nm14,nm15

    integer                       :: dt1
    integer,             optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10,dt11,dt12,dt13,dt14,dt15

    integer                       :: k1
    integer,             optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15

    character(len=40) :: msg = "kim_api_getm_compute_f03"
    

    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl_f03(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      dt1 = kim_api_get_compute_f03(kimmdl, nm1, error);
      if (errcheck_mltpl_f03(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. (.not.present(dt2))) then
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. (.not.present(dt3))) then
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. (.not.present(dt4))) then
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. (.not.present(dt5))) then
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(nm6) .and. (.not.present(dt6))) then
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(nm7) .and. (.not.present(dt7))) then
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(nm8) .and. (.not.present(dt8))) then
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(nm9) .and. (.not.present(dt9))) then
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(nm10) .and. (.not.present(dt10))) then
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(nm11) .and. (.not.present(dt11))) then
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(nm12) .and. (.not.present(dt12))) then
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(nm13) .and. (.not.present(dt13))) then
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(nm14) .and. (.not.present(dt14))) then
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(nm15) .and. (.not.present(dt15))) then
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1))) then
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error = KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) dt2 = kim_api_get_compute_f03(kimmdl, nm2, error);
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) dt3 = kim_api_get_compute_f03(kimmdl, nm3, error);
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) dt4 = kim_api_get_compute_f03(kimmdl, nm4, error);
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) dt5 = kim_api_get_compute_f03(kimmdl, nm5, error);
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) dt6 = kim_api_get_compute_f03(kimmdl, nm6, error);
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) dt7 = kim_api_get_compute_f03(kimmdl, nm7, error);
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) dt8 = kim_api_get_compute_f03(kimmdl, nm8, error);
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) dt9 = kim_api_get_compute_f03(kimmdl, nm9, error);
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) dt10 = kim_api_get_compute_f03(kimmdl, nm10, error);
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) dt11 = kim_api_get_compute_f03(kimmdl, nm11, error);
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) dt12 = kim_api_get_compute_f03(kimmdl, nm12, error);
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) dt13 = kim_api_get_compute_f03(kimmdl, nm13, error);
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) dt14 = kim_api_get_compute_f03(kimmdl, nm14, error);
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) dt15 = kim_api_get_compute_f03(kimmdl, nm15, error);
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
  end subroutine kim_api_getm_compute_f03


  subroutine kim_api_getm_data_f03(kimmdl, error, &
                                   nm1,  dt1,  k1, &
                                   nm2,  dt2,  k2, &
                                   nm3,  dt3,  k3, &
                                   nm4,  dt4,  k4, &
                                   nm5,  dt5,  k5, &
                                   nm6,  dt6,  k6, &
                                   nm7,  dt7,  k7, &
                                   nm8,  dt8,  k8, &
                                   nm9,  dt9,  k9, &
                                   nm10, dt10, k10, &
                                   nm11, dt11, k11, &
                                   nm12, dt12, k12, &
                                   nm13, dt13, k13, &
                                   nm14, dt14, k14, &
                                   nm15, dt15, k15 &
                                   )

    use, intrinsic :: iso_c_binding

    implicit none

    type(c_ptr) :: kimmdl
    integer     :: error

    character(len=*)              :: nm1
    character(len=*),    optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10,nm11,nm12,nm13,nm14,nm15

    type(c_ptr)                   :: dt1
    type(c_ptr),         optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10,dt11,dt12,dt13,dt14,dt15

    integer                       :: k1
    integer,             optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15

    character(len=40) ::msg="kim_api_getm_data_f03"

    
    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl_f03(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      dt1 = kim_api_get_data_f03(kimmdl, nm1, error);
      if (errcheck_mltpl_f03(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. (.not.present(dt2))) then
        if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. (.not.present(dt3))) then
        if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. (.not.present(dt4))) then
        if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. (.not.present(dt5))) then
        if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(nm6) .and. (.not.present(dt6))) then
        if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(nm7) .and. (.not.present(dt7))) then
        if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(nm8) .and. (.not.present(dt8))) then
        if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(nm9) .and. (.not.present(dt9))) then
        if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(nm10) .and. (.not.present(dt10))) then
        if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(nm11) .and. (.not.present(dt11))) then
        if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(nm12) .and. (.not.present(dt12))) then
        if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(nm13) .and. (.not.present(dt13))) then
        if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(nm14) .and. (.not.present(dt14))) then
        if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(nm15) .and. (.not.present(dt15))) then
        if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1))) then
        if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error=KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) dt2 = kim_api_get_data_f03(kimmdl, nm2, error);
      if (errcheck_mltpl_f03(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) dt3 = kim_api_get_data_f03(kimmdl, nm3, error);
      if (errcheck_mltpl_f03(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) dt4 = kim_api_get_data_f03(kimmdl, nm4, error);
      if (errcheck_mltpl_f03(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) dt5 = kim_api_get_data_f03(kimmdl, nm5, error);
      if (errcheck_mltpl_f03(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) dt6 = kim_api_get_data_f03(kimmdl, nm6, error);
      if (errcheck_mltpl_f03(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) dt7 = kim_api_get_data_f03(kimmdl, nm7, error);
      if (errcheck_mltpl_f03(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) dt8 = kim_api_get_data_f03(kimmdl, nm8, error);
      if (errcheck_mltpl_f03(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) dt9 = kim_api_get_data_f03(kimmdl, nm9, error);
      if (errcheck_mltpl_f03(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) dt10 = kim_api_get_data_f03(kimmdl, nm10, error);
      if (errcheck_mltpl_f03(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) dt11 = kim_api_get_data_f03(kimmdl, nm11, error);
      if (errcheck_mltpl_f03(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) dt12 = kim_api_get_data_f03(kimmdl, nm12, error);
      if (errcheck_mltpl_f03(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) dt13 = kim_api_get_data_f03(kimmdl, nm13, error);
      if (errcheck_mltpl_f03(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) dt14 = kim_api_get_data_f03(kimmdl, nm14, error);
      if (errcheck_mltpl_f03(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) dt15 = kim_api_get_data_f03(kimmdl, nm15, error);
      if (errcheck_mltpl_f03(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
  end subroutine kim_api_getm_data_f03

end module kim_api_f03


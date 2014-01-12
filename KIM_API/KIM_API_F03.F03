!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2014, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Andreas Klemenz
!    Lars Pastewka
!    Ryan S. Elliott
!

!
! Release: This file is part of the openkim-api.git repository.
!


#include "KIM_API_status.h"

module kim_api_f03
  implicit none
  integer, parameter :: KIM_KEY_STRING_LENGTH = 128
  interface
    subroutine kim_api_c_free(cp) bind(c, name="free")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: cp
    end subroutine kim_api_c_free

    subroutine kim_api_allocate(kimmdl, natoms, ntypes, error) &
      bind(c, name="KIM_API_allocate")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: natoms
      integer(c_int), value :: ntypes
      integer(c_int)        :: error
    end subroutine kim_api_allocate

    subroutine kim_api_free(kimmdl, error) bind(c, name="KIM_API_free")
      use, intrinsic :: iso_c_binding
      type(c_ptr)    :: kimmdl
      integer(c_int) :: error
    end subroutine kim_api_free

    subroutine kim_api_print(kimmdl, error) bind(c, name="KIM_API_print")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end subroutine kim_api_print

    integer(c_int) function kim_api_model_compute(kimmdl) &
      bind(c, name="KIM_API_model_compute")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_compute

    integer(c_int) function kim_api_model_destroy(kimmdl) &
      bind(c, name="KIM_API_model_destroy")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_destroy

    integer(c_int) function kim_api_get_model_index_shift(kimmdl) &
      bind(c, name="KIM_API_get_model_index_shift")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_get_model_index_shift

    subroutine kim_api_set_model_buffer(kimmdl, ob, ier) &
      bind(c, name="KIM_API_set_model_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_model_buffer

    subroutine kim_api_set_test_buffer(kimmdl, ob, ier) &
      bind(c, name="KIM_API_set_test_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_test_buffer

    type(c_ptr) function kim_api_get_model_buffer(kimmdl, ier) &
      bind(c, name="KIM_API_get_model_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_model_buffer

    type(c_ptr) function kim_api_get_test_buffer(kimmdl, ier) &
      bind(c, name="KIM_API_get_test_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_test_buffer

    integer(c_int) function kim_api_is_half_neighbors(kimmdl, error) &
      bind(c, name="KIM_API_is_half_neighbors")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_is_half_neighbors

    integer(c_int) function kim_api_model_init(kimmdl) &
      bind(c, name="KIM_API_model_init")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_init

    integer(c_int) function kim_api_model_reinit(kimmdl) &
      bind(c, name="KIM_API_model_reinit")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_reinit

    type(c_ptr) function kim_api_get_model_partcl_typs(kimmdl, nptypes, &
      error) &
      bind(c, name="KIM_API_get_model_partcl_typs")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nptypes
      integer(c_int)       :: error
    end function kim_api_get_model_partcl_typs

    type(c_ptr) function kim_api_get_test_partcl_typs(kimmdl, nptypes, &
      error) &
      bind(c, name="KIM_API_get_test_partcl_typs")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nptypes
      integer(c_int)       :: error
    end function kim_api_get_test_partcl_typs

    type(c_ptr) function kim_api_get_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_params

    type(c_ptr) function kim_api_get_free_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_free_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_free_params

    type(c_ptr) function kim_api_get_fixed_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_fixed_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_fixed_params

    type(c_ptr) function kim_api_get_nbc_method(kimmdl, error) &
      bind(c, name="KIM_API_get_NBC_method")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_nbc_method

    integer(c_int) function kim_api_get_neigh_mode(kimmdl, error) &
      bind(c, name="KIM_API_get_neigh_mode")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_neigh_mode

    integer(c_int) function kim_api_get_neigh(kimmdl, mode, request, &
      atom, numnei, nei1atom, rij) &
      bind(c, name="KIM_API_get_neigh")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: mode
      integer(c_int), value :: request
      integer(c_int)        :: atom
      integer(c_int)        :: numnei
      type(c_ptr)           :: nei1atom
      type(c_ptr)           :: rij
    end function kim_api_get_neigh

    type(c_ptr) function kim_api_get_status_msg(status_code) &
      bind(c, name="KIM_API_get_status_msg")
      use, intrinsic :: iso_c_binding
      integer(c_int), value :: status_code
    end function kim_api_get_status_msg

    integer(c_int) function kim_api_get_unit_handling(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_handling")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_handling

    type(c_ptr) function kim_api_get_unit_length(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_length")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_length

    type(c_ptr) function kim_api_get_unit_energy(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_energy")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_energy

    type(c_ptr) function kim_api_get_unit_charge(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_charge")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_charge

    type(c_ptr) function kim_api_get_unit_temperature(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_temperature")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_temperature

    type(c_ptr) function kim_api_get_unit_time(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_time")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_time

    type(c_ptr) function kim_api_get_data_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_data_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_data_by_index

    type(c_funptr) function kim_api_get_method_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_method_by_index")  ! untested
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_method_by_index

    integer(c_intptr_t) function kim_api_get_size_by_index(kimmdl, I, &
      error) &
      bind(c, name="KIM_API_get_size_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_size_by_index

    integer(c_intptr_t) function kim_api_get_rank_by_index(kimmdl, I, &
      error) &
      bind(c, name="KIM_API_get_rank_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_rank_by_index

    subroutine kim_api_set_compute_by_index(kimmdl, I, flag, error) &
      bind(c, name="KIM_API_set_compute_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int), value :: flag
      integer(c_int)        :: error
    end subroutine kim_api_set_compute_by_index

    integer(c_int) function kim_api_get_compute_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_compute_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_compute_by_index
  end interface

contains
  integer(c_int) function kim_api_init(kimmdl, testname, mdlname)
    use :: kim_api_f03_helper, only : c_call => kim_api_init
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: testname
    character(len=*), intent(in) :: mdlname

    kim_api_init = c_call(kimmdl, trim(testname)//c_null_char, &
      trim(mdlname)//c_null_char)
  end function kim_api_init

  integer(c_int) function kim_api_string_init(kimmdl, testdescriptor, modelname)
    ! untested
    use :: kim_api_f03_helper, only : c_call => kim_api_string_init
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: testdescriptor
    character(len=*), intent(in) :: modelname

    kim_api_string_init = c_call(kimmdl, &
      trim(testdescriptor)//c_null_char, trim(modelname)//c_null_char)
  end function kim_api_string_init

  integer(c_int) function kim_api_model_info(kimmdl, modelname)
    ! untested
    use :: kim_api_f03_helper, only : c_call => kim_api_model_info
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: modelname

    kim_api_model_info = c_call(kimmdl, &
      trim(modelname)//c_null_char)
  end function kim_api_model_info

  integer(c_int) function kim_api_set_data(kimmdl, nm, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_data
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: size
    type(c_ptr),      intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_data = c_call(kimmdl, trim(nm)//c_null_char, &
      size_int, dt)
  end function kim_api_set_data

  integer(c_int) function kim_api_set_method(kimmdl, nm, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_method
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: size
    type(c_funptr),   intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_method = c_call(kimmdl, &
      trim(nm)//c_null_char, size_int, dt)
  end function kim_api_set_method

  type(c_ptr) function kim_api_get_data(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_data
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_data = c_call(kimmdl, trim(nm)//c_null_char, &
      error)
  end function kim_api_get_data

  type(c_funptr) function kim_api_get_method(kimmdl, nm, error)
    ! untested
    use :: kim_api_f03_helper, only : c_call => kim_api_get_method
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_method = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_method

  integer(c_intptr_t) function kim_api_get_size(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_size
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_size = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_size

  integer(c_intptr_t) function kim_api_get_rank(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_rank
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_rank = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_rank

  subroutine kim_api_get_shape(kimmdl, nm, shape, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_shape
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: shape(*)
    integer(c_int),   intent(out) :: error
    integer(c_intptr_t)           :: rank
    integer(c_int),   allocatable :: shape_tmp(:)
    integer(c_intptr_t)           :: i

    rank = c_call(kimmdl, trim(nm)//c_null_char, shape, error)

    allocate(shape_tmp(rank))
    do i=1,rank
      shape_tmp(i) = shape(rank - i + 1)  ! transpose shape
    end do
    shape(1:rank) = shape_tmp(1:rank)
    deallocate(shape_tmp)
  end subroutine kim_api_get_shape

  subroutine kim_api_set_shape(kimmdl, nm, shape, rank, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_shape
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(in)  :: shape(*)
    integer(c_int),   intent(in)  :: rank
    integer(c_int),   intent(out) :: error
    integer(c_int) :: shape_t(rank)
    integer(c_int) :: i

    if (rank .gt. 1) then
      do i=1,rank
        shape_t(i) = shape(rank - i + 1)  ! transpose shape
      end do
    end if

    call c_call(kimmdl, trim(nm)//c_null_char, shape_t, rank, error)
  end subroutine kim_api_set_shape

  subroutine kim_api_set_compute(kimmdl, nm, flag, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_compute
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(in)  :: flag
    integer(c_int),   intent(out) :: error

    call c_call(kimmdl, trim(nm)//c_null_char, flag, error)
  end subroutine kim_api_set_compute

  integer(c_int) function kim_api_get_compute(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_compute
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_compute = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_compute

  integer(c_int) function kim_api_get_index(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_index
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_index = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_index

  integer(c_int) function kim_api_get_partcl_type_code(kimmdl, particle, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_partcl_type_code
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer(c_int),   intent(out) :: error

    kim_api_get_partcl_type_code = c_call(kimmdl, trim(particle)//c_null_char, &
      error)
  end function kim_api_get_partcl_type_code

  subroutine kim_api_set_partcl_type_code(kimmdl, particle, code, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_partcl_type_code
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer(c_int),   intent(in)  :: code
    integer(c_int),   intent(out) :: error

    call c_call(kimmdl, trim(particle)//c_null_char, code, error)
  end subroutine kim_api_set_partcl_type_code

  type(c_ptr) function kim_api_get_model_kim_str(modelname, error)
    ! untested
    use :: kim_api_f03_helper, only : c_call => kim_api_get_model_kim_str
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=*), intent(in)  :: modelname
    integer(c_int),   intent(out) :: error

    kim_api_get_model_kim_str = c_call(trim(modelname)//c_null_char, error)
  end function kim_api_get_model_kim_str

  integer(c_int) function kim_api_report_error(ln, fl, usermsg, status_code)
    use :: kim_api_f03_helper, only : c_call => kim_api_report_error
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int),   intent(in) :: ln
    character(len=*), intent(in) :: fl
    character(len=*), intent(in) :: usermsg
    integer(c_int),   intent(in) :: status_code

    kim_api_report_error = c_call(ln, trim(fl)//c_null_char, &
      trim(usermsg)//c_null_char, status_code)
  end function kim_api_report_error

  real(c_double) function kim_api_get_scale_conversion(u_from, u_to, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_scale_conversion
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=*), intent(in)  :: u_from
    character(len=*), intent(in)  :: u_to
    integer(c_int),   intent(out) :: error

    kim_api_get_scale_conversion = &
      c_call(trim(u_from)//c_null_char, trim(u_to)//c_null_char, error)
  end function kim_api_get_scale_conversion

  real(c_double) function kim_api_convert_to_act_unit( &
    kimmdl, &
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
    use :: kim_api_f03_helper, only : c_call => kim_api_convert_to_act_unit
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: length
    character(len=*), intent(in)  :: energy
    character(len=*), intent(in)  :: charge
    character(len=*), intent(in)  :: temperature
    character(len=*), intent(in)  :: time
    real(c_double),   intent(in)  :: length_exponent
    real(c_double),   intent(in)  :: energy_exponent
    real(c_double),   intent(in)  :: charge_exponent
    real(c_double),   intent(in)  :: temperature_exponent
    real(c_double),   intent(in)  :: time_exponent
    integer(c_int),   intent(out) :: error

    kim_api_convert_to_act_unit = c_call( &
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
  end function kim_api_convert_to_act_unit

  integer(c_int) function kim_api_set_data_by_index(kimmdl, I, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_data_by_index
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),    intent(in) :: kimmdl
    integer(c_int), intent(in) :: I
    integer(c_int), intent(in) :: size
    type(c_ptr),    intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_data_by_index = c_call(kimmdl, I, size_int, dt)
  end function kim_api_set_data_by_index

  integer(c_int) function kim_api_set_method_by_index(kimmdl, I, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_method_by_index
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),    intent(in) :: kimmdl
    integer(c_int), intent(in) :: I
    integer(c_int), intent(in) :: size
    type(c_funptr), intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_method_by_index = c_call(kimmdl, I, size_int, dt)
  end function kim_api_set_method_by_index


  subroutine kim_api_get_shape_by_index(kimmdl, I, shape, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_shape_by_index
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr),        intent(in)  :: kimmdl
    integer(c_int),     intent(in)  :: I
    integer(c_int),     intent(out) :: shape(*)
    integer(c_int),     intent(out) :: error
    integer(c_intptr_t)             :: rank
    integer, allocatable            :: shape_tmp(:)
    integer(c_intptr_t)             :: j

    rank = c_call(kimmdl, I, shape, error)

    allocate(shape_tmp(rank))
    do j=1,rank
      shape_tmp(j) = shape(rank - j + 1)  ! transpose shape
    end do
    shape(1:rank) = shape_tmp(1:rank)
    deallocate(shape_tmp)
  end subroutine kim_api_get_shape_by_index

  subroutine kim_api_setm_method( &
    kimmdl, error, &
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
    use :: kim_api_f03_helper, only : errcheck_mltpl
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr)                :: kimmdl
    integer(c_int)             :: error
    character(len=*)           :: nm1
    character(len=*), optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10,nm11, &
      nm12,nm13,nm14,nm15
    integer(c_int)             :: sz1
    integer(c_int),   optional :: sz2,sz3,sz4,sz5,sz6,sz7,sz8,sz9,sz10,sz11, &
      sz12,sz13,sz14,sz15
    type(c_funptr)             :: dt1
    type(c_funptr),   optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10,dt11, &
      dt12,dt13,dt14,dt15
    integer(c_int)             :: k1
    integer(c_int),   optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,&
      k14,k15
    character(len=40) :: msg = "kim_api_setm_method"

    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1,nm1) &
        .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      error = kim_api_set_method(kimmdl, nm1, sz1, dt1)
      if (errcheck_mltpl(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. &
      (.not.present(sz2) .or. .not. present(dt2) .or. .not.present(k2))) then
      if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. &
      (.not.present(sz3) .or. .not.present(dt3) .or. .not.present(k3))) then
      if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. &
      (.not.present(sz4) .or. .not.present(dt4) .or. .not.present(k4))) then
      if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. &
      (.not.present(sz5) .or. .not.present(dt5) .or. .not.present(k5))) then
      if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if( present(nm6) .and. &
      (.not.present(sz6) .or. .not.present(dt6) .or. .not.present(k6))) then
      if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if( present(nm7) .and. &
      (.not.present(sz7) .or. .not.present(dt7) .or. .not.present(k7))) then
      if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if( present(nm8) .and. &
      (.not.present(sz8) .or. .not.present(dt8) .or. .not.present(k8))) then
      if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if( present(nm9) .and. &
      (.not.present(sz9) .or. .not.present(dt9) .or. .not.present(k9))) then
      if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if( present(nm10) .and. &
      (.not.present(sz10) .or. .not.present(dt10) .or. .not.present(k10))) then
      if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if( present(nm11) .and. &
      (.not.present(sz11) .or. .not.present(dt11) .or. .not.present(k11))) then
      if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if( present(nm12) .and. &
      (.not.present(sz12) .or. .not.present(dt12) .or. .not.present(k12))) then
      if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if( present(nm13) .and. &
      (.not.present(sz13) .or. .not.present(dt13) .or. .not.present(k13))) then
      if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if( present(nm14) .and. &
      (.not.present(sz14) .or. .not.present(dt14) .or. .not.present(k14))) then
      if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if( present(nm15) .and. &
      (.not.present(sz15) .or. .not.present(dt15) .or. .not.present(k15))) then
      if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1)))then
      if (errcheck_mltpl(error, msg, 2,  nm2) .lt. KIM_STATUS_OK) return
    else if(present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 3,  nm3) .lt. KIM_STATUS_OK) return
    else if(present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 4,  nm4) .lt. KIM_STATUS_OK) return
    else if(present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 5,  nm5) .lt. KIM_STATUS_OK) return
    else if(present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 6,  nm6) .lt. KIM_STATUS_OK) return
    else if(present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 7,  nm7) .lt. KIM_STATUS_OK) return
    else if(present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 8,  nm8) .lt. KIM_STATUS_OK) return
    else if(present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 9,  nm9) .lt. KIM_STATUS_OK) return
    else if(present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 10,  nm10) .lt. KIM_STATUS_OK) return
    else if(present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 11,  nm11) .lt. KIM_STATUS_OK) return
    else if(present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 12,  nm12) .lt. KIM_STATUS_OK) return
    else if(present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 13,  nm13) .lt. KIM_STATUS_OK) return
    else if(present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1)))then
      if  (errcheck_mltpl(error, msg, 14,  nm14) .lt. KIM_STATUS_OK) return
    else if(present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1)))then
      if (errcheck_mltpl(error, msg, 15,  nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error = KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm2, sz2, dt2);
    end if
    if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm3, sz3, dt3);
    end if
    if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm4, sz4, dt4);
    end if
    if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm5, sz5, dt5);
    end if
    if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm6, sz6, dt6);
    end if
    if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm7, sz7, dt7);
    end if
    if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm8, sz8, dt8);
    end if
    if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm9, sz9, dt9);
    end if
    if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm10, sz10, dt10);
    end if
    if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm11, sz11, dt11);
    end if
    if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm12, sz12, dt12);
    end if
    if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm13, sz13, dt13);
    end if
    if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm14, sz14, dt14);
    end if
    if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) then
      error=kim_api_set_method(kimmdl, nm15, sz15, dt15);
    end if
    if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
  end subroutine kim_api_setm_method

  subroutine kim_api_getm_compute( &
    kimmdl, error, &
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
    use :: kim_api_f03_helper, only : errcheck_mltpl
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr) :: kimmdl
    integer(c_int)                :: error
    character(len=*)              :: nm1
    character(len=*),    optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10, &
      nm11,nm12,nm13,nm14,nm15
    integer(c_int)                :: dt1
    integer(c_int),      optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10, &
      dt11,dt12,dt13,dt14,dt15
    integer(c_int)                :: k1
    integer(c_int),      optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12, &
      k13,k14,k15
    character(len=40) :: msg = "kim_api_getm_compute"

    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1, nm1) &
        .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      dt1 = kim_api_get_compute(kimmdl, nm1, error);
      if (errcheck_mltpl(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. (.not.present(dt2))) then
      if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. (.not.present(dt3))) then
      if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. (.not.present(dt4))) then
      if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. (.not.present(dt5))) then
      if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(nm6) .and. (.not.present(dt6))) then
      if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(nm7) .and. (.not.present(dt7))) then
      if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(nm8) .and. (.not.present(dt8))) then
      if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(nm9) .and. (.not.present(dt9))) then
      if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(nm10) .and. (.not.present(dt10))) then
      if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(nm11) .and. (.not.present(dt11))) then
      if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(nm12) .and. (.not.present(dt12))) then
      if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(nm13) .and. (.not.present(dt13))) then
      if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(nm14) .and. (.not.present(dt14))) then
      if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(nm15) .and. (.not.present(dt15))) then
      if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error = KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) then
      dt2 = kim_api_get_compute(kimmdl, nm2, error);
    end if
    if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) then
      dt3 = kim_api_get_compute(kimmdl, nm3, error);
    end if
    if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) then
      dt4 = kim_api_get_compute(kimmdl, nm4, error);
    end if
    if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) then
      dt5 = kim_api_get_compute(kimmdl, nm5, error);
    end if
    if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) then
      dt6 = kim_api_get_compute(kimmdl, nm6, error);
    end if
    if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) then
      dt7 = kim_api_get_compute(kimmdl, nm7, error);
    end if
    if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) then
      dt8 = kim_api_get_compute(kimmdl, nm8, error);
    end if
    if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) then
      dt9 = kim_api_get_compute(kimmdl, nm9, error);
    end if
    if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) then
      dt10 = kim_api_get_compute(kimmdl, nm10, error);
    end if
    if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) then
      dt11 = kim_api_get_compute(kimmdl, nm11, error);
    end if
    if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) then
      dt12 = kim_api_get_compute(kimmdl, nm12, error);
    end if
    if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) then
      dt13 = kim_api_get_compute(kimmdl, nm13, error);
    end if
    if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) then
      dt14 = kim_api_get_compute(kimmdl, nm14, error);
    end if
    if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) then
      dt15 = kim_api_get_compute(kimmdl, nm15, error);
    end if
    if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
  end subroutine kim_api_getm_compute

  subroutine kim_api_getm_data( &
    kimmdl, error, &
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
    use :: kim_api_f03_helper, only : errcheck_mltpl
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr)                   :: kimmdl
    integer(c_int)                :: error
    character(len=*)              :: nm1
    character(len=*),    optional :: nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10, &
      nm11,nm12,nm13,nm14,nm15
    type(c_ptr)                   :: dt1
    type(c_ptr),         optional :: dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10, &
      dt11,dt12,dt13,dt14,dt15
    integer(c_int)                :: k1
    integer(c_int),      optional :: k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12, &
      k13,k14,k15
    character(len=40) ::msg="kim_api_getm_data"

    if ((k1 .ne. 0) .and. (k1 .ne. 1)) then
      error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
      if (errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1, nm1) &
        .lt. KIM_STATUS_OK) return
    end if
    if (k1 .eq. 1) then
      dt1 = kim_api_get_data(kimmdl, nm1, error);
      if (errcheck_mltpl(error, msg, 1, nm1) .lt. KIM_STATUS_OK) return
    end if

    !check rest of the arguments
    error = KIM_STATUS_WRONG_MULTIPLE_ARGS
    if (present(nm2) .and. (.not.present(dt2))) then
      if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(nm3) .and. (.not.present(dt3))) then
      if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(nm4) .and. (.not.present(dt4))) then
      if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(nm5) .and. (.not.present(dt5))) then
      if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(nm6) .and. (.not.present(dt6))) then
      if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(nm7) .and. (.not.present(dt7))) then
      if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(nm8) .and. (.not.present(dt8))) then
      if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(nm9) .and. (.not.present(dt9))) then
      if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(nm10) .and. (.not.present(dt10))) then
      if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(nm11) .and. (.not.present(dt11))) then
      if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(nm12) .and. (.not.present(dt12))) then
      if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(nm13) .and. (.not.present(dt13))) then
      if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(nm14) .and. (.not.present(dt14))) then
      if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(nm15) .and. (.not.present(dt15))) then
      if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
    if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    else if (present(k3) .and. ((k3 .ne. 0) .and. (k3 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    else if (present(k4) .and. ((k4 .ne. 0) .and. (k4 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    else if (present(k5) .and. ((k5 .ne. 0) .and. (k5 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    else if (present(k6) .and. ((k6 .ne. 0) .and. (k6 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    else if (present(k7) .and. ((k7 .ne. 0) .and. (k7 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    else if (present(k8) .and. ((k8 .ne. 0) .and. (k8 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    else if (present(k9) .and. ((k9 .ne. 0) .and. (k9 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    else if (present(k10) .and. ((k10 .ne. 0) .and. (k10 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    else if (present(k11) .and. ((k11 .ne. 0) .and. (k11 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    else if (present(k12) .and. ((k12 .ne. 0) .and. (k12 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    else if (present(k13) .and. ((k13 .ne. 0) .and. (k13 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    else if (present(k14) .and. ((k14 .ne. 0) .and. (k14 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    else if (present(k15) .and. ((k15 .ne. 0) .and. (k15 .ne. 1))) then
      if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
    end if

    !process arguments
    error=KIM_STATUS_OK
    if (present(nm2) .and. (k2 .eq. 1)) then
      dt2 = kim_api_get_data(kimmdl, nm2, error);
    end if
    if (errcheck_mltpl(error, msg, 2, nm2) .lt. KIM_STATUS_OK) return
    if (present(nm3) .and. (k3 .eq. 1)) then
      dt3 = kim_api_get_data(kimmdl, nm3, error);
    end if
    if (errcheck_mltpl(error, msg, 3, nm3) .lt. KIM_STATUS_OK) return
    if (present(nm4) .and. (k4 .eq. 1)) then
      dt4 = kim_api_get_data(kimmdl, nm4, error);
    end if
    if (errcheck_mltpl(error, msg, 4, nm4) .lt. KIM_STATUS_OK) return
    if (present(nm5) .and. (k5 .eq. 1)) then
      dt5 = kim_api_get_data(kimmdl, nm5, error);
    end if
    if (errcheck_mltpl(error, msg, 5, nm5) .lt. KIM_STATUS_OK) return
    if (present(nm6) .and. (k6 .eq. 1)) then
      dt6 = kim_api_get_data(kimmdl, nm6, error);
    end if
    if (errcheck_mltpl(error, msg, 6, nm6) .lt. KIM_STATUS_OK) return
    if (present(nm7) .and. (k7 .eq. 1)) then
      dt7 = kim_api_get_data(kimmdl, nm7, error);
    end if
    if (errcheck_mltpl(error, msg, 7, nm7) .lt. KIM_STATUS_OK) return
    if (present(nm8) .and. (k8 .eq. 1)) then
      dt8 = kim_api_get_data(kimmdl, nm8, error);
    end if
    if (errcheck_mltpl(error, msg, 8, nm8) .lt. KIM_STATUS_OK) return
    if (present(nm9) .and. (k9 .eq. 1)) then
      dt9 = kim_api_get_data(kimmdl, nm9, error);
    end if
    if (errcheck_mltpl(error, msg, 9, nm9) .lt. KIM_STATUS_OK) return
    if (present(nm10) .and. (k10 .eq. 1)) then
      dt10 = kim_api_get_data(kimmdl, nm10, error);
    end if
    if (errcheck_mltpl(error, msg, 10, nm10) .lt. KIM_STATUS_OK) return
    if (present(nm11) .and. (k11 .eq. 1)) then
      dt11 = kim_api_get_data(kimmdl, nm11, error);
    end if
    if (errcheck_mltpl(error, msg, 11, nm11) .lt. KIM_STATUS_OK) return
    if (present(nm12) .and. (k12 .eq. 1)) then
      dt12 = kim_api_get_data(kimmdl, nm12, error);
    end if
    if (errcheck_mltpl(error, msg, 12, nm12) .lt. KIM_STATUS_OK) return
    if (present(nm13) .and. (k13 .eq. 1)) then
      dt13 = kim_api_get_data(kimmdl, nm13, error);
    end if
    if (errcheck_mltpl(error, msg, 13, nm13) .lt. KIM_STATUS_OK) return
    if (present(nm14) .and. (k14 .eq. 1)) then
      dt14 = kim_api_get_data(kimmdl, nm14, error);
    end if
    if (errcheck_mltpl(error, msg, 14, nm14) .lt. KIM_STATUS_OK) return
    if (present(nm15) .and. (k15 .eq. 1)) then
      dt15 = kim_api_get_data(kimmdl, nm15, error);
    end if
    if (errcheck_mltpl(error, msg, 15, nm15) .lt. KIM_STATUS_OK) return
  end subroutine kim_api_getm_data
end module kim_api_f03

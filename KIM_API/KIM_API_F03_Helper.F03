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

module kim_api_f03_helper
  use, intrinsic :: iso_c_binding
  implicit none
  interface
    integer(c_int) function kim_api_init(kimmdl, testname, mdlname) &
      bind(c, name="KIM_API_init")
      use, intrinsic :: iso_c_binding
      type(c_ptr)       :: kimmdl
      character(c_char) :: testname(*)
      character(c_char) :: mdlname(*)
    end function kim_api_init

    integer(c_int) function kim_api_string_init(kimmdl, testdescriptor, &
      modelname) &
      bind(c, name="KIM_API_string_init")
      use, intrinsic :: iso_c_binding
      type(c_ptr)       :: kimmdl
      character(c_char) :: testdescriptor(*)
      character(c_char) :: modelname(*)
    end function kim_api_string_init

    integer(c_int) function kim_api_model_info(kimmdl, modelname) &
      bind(c, name="KIM_API_model_info")
      use, intrinsic :: iso_c_binding
      type(c_ptr)       :: kimmdl
      character(c_char) :: modelname(*)
    end function kim_api_model_info

    integer(c_int) function kim_api_set_data(kimmdl, nm, size, dt) &
      bind(c, name="KIM_API_set_data")
      use, intrinsic :: iso_c_binding
      type(c_ptr),         value :: kimmdl
      character(c_char)          :: nm(*)
      integer(c_intptr_t), value :: size
      type(c_ptr),         value :: dt
    end function kim_api_set_data

    integer(c_int) function kim_api_set_method(kimmdl, nm, size, dt) &
      bind(c, name="KIM_API_set_method")
      use, intrinsic :: iso_c_binding
      type(c_ptr),         value :: kimmdl
      character(c_char)          :: nm(*)
      integer(c_intptr_t), value :: size
      type(c_funptr),      value :: dt
    end function kim_api_set_method

    type(c_ptr) function kim_api_get_data(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_data")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_data

    type(c_funptr) function kim_api_get_method(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_method")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_method

    integer(c_intptr_t) function kim_api_get_size(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_size")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_size

    integer(c_intptr_t) function kim_api_get_rank(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_rank")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_rank

    integer(c_intptr_t) function kim_api_get_shape(kimmdl, nm, shape, error) &
      bind(c, name="KIM_API_get_shape")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: shape(*)
      integer(c_int)          :: error
    end function kim_api_get_shape

    subroutine kim_api_set_shape(kimmdl, nm, shape, rank, error) &
      bind(c, name="KIM_API_set_shape")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: shape(*)
      integer(c_int),   value :: rank
      integer(c_int)          :: error
    end subroutine kim_api_set_shape

    subroutine kim_api_set_compute(kimmdl, nm, flag, error) &
      bind(c, name="KIM_API_set_compute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int),   value :: flag
      integer(c_int)          :: error
    end subroutine kim_api_set_compute

    integer(c_int) function kim_api_get_compute(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_compute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_compute

    integer(c_int) function kim_api_get_index(kimmdl, nm, error) &
      bind(c, name="KIM_API_get_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: nm(*)
      integer(c_int)          :: error
    end function kim_api_get_index

    integer(c_int) function kim_api_get_partcl_type_code(kimmdl, particle, &
      error) &
      bind(c, name="KIM_API_get_partcl_type_code")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: particle(*)
      integer(c_int)          :: error
    end function kim_api_get_partcl_type_code

    subroutine kim_api_set_partcl_type_code(kimmdl, particle, code, error) &
      bind(c, name="KIM_API_set_partcl_type_code")
      use, intrinsic :: iso_c_binding
      type(c_ptr),      value :: kimmdl
      character(c_char)       :: particle(*)
      integer(c_int),   value :: code
      integer(c_int)          :: error
    end subroutine kim_api_set_partcl_type_code

    type(c_ptr) function kim_api_get_model_kim_str(modelname, error) &
      bind(c, name="KIM_API_get_model_kim_str")
      use, intrinsic :: iso_c_binding
      character(c_char) :: modelname(*)
      integer(c_int)    :: error
    end function kim_api_get_model_kim_str

    integer(c_int) function kim_api_report_error(ln, fl, usermsg, status_code) &
      bind(c, name="KIM_API_report_error")
      use, intrinsic :: iso_c_binding
      integer(c_int),   value :: ln
      character(c_char)       :: fl(*)
      character(c_char)       :: usermsg(*)
      integer(c_int),   value :: status_code
    end function kim_api_report_error

    real(c_double) function kim_api_get_scale_conversion(u_from, u_to, error) &
      bind(c, name="KIM_API_get_scale_conversion")
      use, intrinsic :: iso_c_binding
      character(c_char) :: u_from(*)
      character(c_char) :: u_to(*)
      integer(c_int)    :: error
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

    integer(c_int) function kim_api_set_data_by_index(kimmdl, I, size, dt) &
      bind(c, name="KIM_API_set_data_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),         value :: kimmdl
      integer(c_int),      value :: I
      integer(c_intptr_t), value :: size
      type(c_ptr),         value :: dt
    end function kim_api_set_data_by_index

    integer(c_int) function kim_api_set_method_by_index(kimmdl, I, size, dt) &
      bind(c, name="KIM_API_set_method_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),         value :: kimmdl
      integer(c_int),      value :: I
      integer(c_intptr_t), value :: size
      type(c_funptr),      value :: dt
    end function kim_api_set_method_by_index

    integer(c_intptr_t) function kim_api_get_shape_by_index(kimmdl, I, shape, &
      error) &
      bind(c, name="KIM_API_get_shape_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: shape(*)
      integer(c_int)        :: error
    end function kim_api_get_shape_by_index
  end interface

contains

  integer(c_int) function errcheck_mltpl_integer(error, msgfrom, grarg, ind)
    implicit none
    integer(c_int),   intent(in) :: error
    character(len=*), intent(in) :: msgfrom
    integer(c_int),   intent(in) :: grarg
    integer(c_int),   intent(in) :: ind

    errcheck_mltpl_integer = KIM_STATUS_OK

    if (error .ge. KIM_STATUS_OK) return

    errcheck_mltpl_integer = KIM_STATUS_FAIL

    print *, "failed:", trim(msgfrom), ", for group argument ", grarg, &
      " and index", ind

    errcheck_mltpl_integer = KIM_STATUS_FAIL
  end function errcheck_mltpl_integer

  integer(c_int) function errcheck_mltpl_character(error, msgfrom, grarg, nm)
    implicit none
    integer(c_int),    intent(in) :: error
    character(len=*),  intent(in) :: msgfrom
    integer(c_int),    intent(in) :: grarg
    character(len=*),  intent(in) :: nm

    errcheck_mltpl_character = KIM_STATUS_OK

    if (error .ge. KIM_STATUS_OK) return

    errcheck_mltpl_character = KIM_STATUS_FAIL

    print *, "failed:", trim(msgfrom), ", for group argument ", grarg, &
      " and name", trim(nm)

    errcheck_mltpl_character = KIM_STATUS_FAIL
  end function errcheck_mltpl_character
end module kim_api_f03_helper

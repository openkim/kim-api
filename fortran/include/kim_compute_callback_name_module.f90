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
! Copyright (c) 2016--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_compute_callback_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_compute_callback_name_type, &
    kim_compute_callback_name_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_compute_callback_name_string, &

    kim_compute_callback_name_get_neighbor_list, &
    kim_compute_callback_name_process_dedr_term, &
    kim_compute_callback_name_process_d2edr2_term, &

    kim_compute_callback_name_get_number_of_compute_callback_names, &
    kim_compute_callback_name_get_compute_callback_name


  type, bind(c) :: kim_compute_callback_name_type
    integer(c_int) compute_callback_name_id
  end type kim_compute_callback_name_type

  type(kim_compute_callback_name_type), protected, &
    bind(c, name="KIM_COMPUTE_CALLBACK_NAME_GetNeighborList") &
    :: kim_compute_callback_name_get_neighbor_list
  type(kim_compute_callback_name_type), protected, &
    bind(c, name="KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm") &
    :: kim_compute_callback_name_process_dedr_term
  type(kim_compute_callback_name_type), protected, &
    bind(c, name="KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term") &
    :: kim_compute_callback_name_process_d2edr2_term

  interface operator (.eq.)
    logical function kim_compute_callback_name_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_compute_callback_name_type
      implicit none
      type(kim_compute_callback_name_type), intent(in) :: left
      type(kim_compute_callback_name_type), intent(in) :: right
    end function kim_compute_callback_name_equal
  end interface operator (.eq.)

  interface
    subroutine kim_compute_callback_name_from_string(string, &
      compute_callback_name)
      import kim_compute_callback_name_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_compute_callback_name_type), intent(out) :: compute_callback_name
    end subroutine kim_compute_callback_name_from_string

    subroutine kim_compute_callback_name_string(compute_callback_name, string)
      import kim_compute_callback_name_type
      implicit none
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      character(len=*), intent(out) :: string
    end subroutine kim_compute_callback_name_string

    subroutine kim_compute_callback_name_get_number_of_compute_callback_names( &
      number_of_compute_callback_names)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_compute_callback_names
    end subroutine &
      kim_compute_callback_name_get_number_of_compute_callback_names

    subroutine kim_compute_callback_name_get_compute_callback_name(index, &
      compute_callback_name, ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_callback_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_compute_callback_name_type), intent(out) :: compute_callback_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_callback_name_get_compute_callback_name
  end interface
end module kim_compute_callback_name_module

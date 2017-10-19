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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_callback_name_module
  use, intrinsic :: iso_c_binding
  use kim_callback_name_id_module
  implicit none
  private

  public &
    kim_callback_name_type, &
    operator (.eq.), &
    operator (.ne.), &
    kim_callback_name_string, &

    kim_callback_name_get_neighbor_list, &
    kim_callback_name_process_dedr_term, &
    kim_callback_name_process_d2edr2_term, &

    kim_callback_name_get_number_of_callbacks, &
    kim_callback_name_get_callback_name

  type, bind(c) :: kim_callback_name_type
    integer(c_int) callback_name_id
  end type kim_callback_name_type

  type(kim_callback_name_type), parameter :: &
    kim_callback_name_get_neighbor_list &
    = kim_callback_name_type(get_neighbor_list_id)
  type(kim_callback_name_type), parameter :: &
    kim_callback_name_process_dedr_term &
    = kim_callback_name_type(process_dedr_term_id)
  type(kim_callback_name_type), parameter :: &
    kim_callback_name_process_d2edr2_term = &
    kim_callback_name_type(process_d2edr2_term_id)

  interface operator (.eq.)
    logical function kim_callback_name_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_callback_name_type
      implicit none
      type(kim_callback_name_type), intent(in) :: left
      type(kim_callback_name_type), intent(in) :: right
    end function kim_callback_name_equal
  end interface operator (.eq.)

  interface
    subroutine kim_callback_name_string(callback_name, string)
      import kim_callback_name_type
      implicit none
      type(kim_callback_name_type), intent(in), value :: callback_name
      character(len=*), intent(out) :: string
    end subroutine kim_callback_name_string

    subroutine kim_callback_name_get_number_of_callbacks(number_of_callbacks)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_callbacks
    end subroutine kim_callback_name_get_number_of_callbacks

    subroutine kim_callback_name_get_callback_name(index, callback_name, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_callback_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_callback_name_type), intent(out) :: callback_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_callback_name_get_callback_name
  end interface
end module kim_callback_name_module

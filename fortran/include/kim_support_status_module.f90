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


module kim_support_status_module
  use, intrinsic :: iso_c_binding
  use kim_support_status_id_module
  implicit none
  private

  public &
    kim_support_status_type, &
    operator (.eq.), &
    operator (.ne.), &
    kim_support_status_string, &

    kim_support_status_required_by_api, &
    kim_support_status_not_supported, &
    kim_support_status_required, &
    kim_support_status_optional

  type, bind(c) :: kim_support_status_type
    integer(c_int) :: support_status_id
  end type kim_support_status_type

  type(kim_support_status_type), parameter :: &
    kim_support_status_required_by_api &
    = kim_support_status_type(required_by_api_id)
  type(kim_support_status_type), parameter :: &
    kim_support_status_not_supported = kim_support_status_type(not_supported_id)
  type(kim_support_status_type), parameter :: &
    kim_support_status_required = kim_support_status_type(required_id)
  type(kim_support_status_type), parameter :: &
    kim_support_status_optional = kim_support_status_type(optional_id)

  interface operator (.eq.)
    logical function kim_support_status_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_support_status_type
      implicit none
      type(kim_support_status_type), intent(in) :: left
      type(kim_support_status_type), intent(in) :: right
    end function kim_support_status_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_support_status_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_support_status_type
      implicit none
      type(kim_support_status_type), intent(in) :: left
      type(kim_support_status_type), intent(in) :: right
    end function kim_support_status_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_support_status_string(support_status, support_status_string)
      import kim_support_status_type
      implicit none
      type(kim_support_status_type), intent(in), value :: support_status
      character(len=*), intent(out) :: support_status_string
    end subroutine kim_support_status_string
  end interface
end module kim_support_status_module

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


module kim_compute_argument_attribute_module
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_attribute_id_module
  implicit none
  private

  public &
    kim_compute_argument_attribute_type, &
    kim_compute_argument_attribute_string, &

    kim_compute_argument_attribute_not_supported, &
    kim_compute_argument_attribute_required, &
    kim_compute_argument_attribute_optional

  type, bind(c) :: kim_compute_argument_attribute_type
    integer(c_int) :: argument_attribute_id
  end type kim_compute_argument_attribute_type

  type(kim_compute_argument_attribute_type), parameter :: &
    kim_compute_argument_attribute_not_supported = &
    kim_compute_argument_attribute_type(not_supported_id)
  type(kim_compute_argument_attribute_type), parameter :: &
    kim_compute_argument_attribute_required = &
    kim_compute_argument_attribute_type(required_id)
  type(kim_compute_argument_attribute_type), parameter :: &
    kim_compute_argument_attribute_optional = &
    kim_compute_argument_attribute_type(optional_id)

  interface
    subroutine kim_compute_argument_attribute_string(&
      compute_argument_attribute, attribute_string)
      import kim_compute_argument_attribute_type
      implicit none
      type(kim_compute_argument_attribute_type), intent(in), value :: &
        compute_argument_attribute
      character(len=*), intent(out) :: attribute_string
    end subroutine kim_compute_argument_attribute_string
  end interface
end module kim_compute_argument_attribute_module

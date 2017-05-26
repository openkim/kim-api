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


module kim_parameter_module
  use, intrinsic :: iso_c_binding
  use kim_parameter_id_module
  implicit none
  private

  public &
    kim_parameter_data_type_type, &

    kim_parameter_data_type_integer, &
    kim_parameter_data_type_real, &
    kim_parameter_data_type_double

  type, bind(c) :: kim_parameter_data_type_type
    integer(c_int) data_type_id
  end type kim_parameter_data_type_type
  type(kim_parameter_data_type_type), parameter :: &
    kim_parameter_data_type_integer = kim_parameter_data_type_type(integer_id)
  type(kim_parameter_data_type_type), parameter :: &
    kim_parameter_data_type_real = kim_parameter_data_type_type(real_id)
  type(kim_parameter_data_type_type), parameter :: &
    kim_parameter_data_type_double = kim_parameter_data_type_type(double_id)
end module kim_parameter_module

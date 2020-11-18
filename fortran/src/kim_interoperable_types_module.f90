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
! Copyright (c) 2016--2020, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-2.2.0 package.
!

module kim_interoperable_types_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_collections_type, &
    kim_compute_arguments_type, &
    kim_log_type, &
    kim_model_compute_arguments_create_type, &
    kim_model_compute_arguments_destroy_type, &
    kim_model_compute_arguments_type, &
    kim_model_compute_type, &
    kim_model_create_type, &
    kim_model_extension_type, &
    kim_model_destroy_type, &
    kim_model_driver_create_type, &
    kim_model_type, &
    kim_model_refresh_type, &
    kim_model_write_parameterized_model_type, &
    kim_simulator_model_type

  type, bind(c) :: kim_collections_type
    private
    type(c_ptr) :: p
  end type kim_collections_type

  type, bind(c) :: kim_compute_arguments_type
    private
    type(c_ptr) :: p
  end type kim_compute_arguments_type

  type, bind(c) :: kim_log_type
    private
    type(c_ptr) :: p
  end type kim_log_type

  type, bind(c) :: kim_model_compute_arguments_create_type
    private
    type(c_ptr) :: p
  end type kim_model_compute_arguments_create_type

  type, bind(c) :: kim_model_compute_arguments_destroy_type
    private
    type(c_ptr) :: p
  end type kim_model_compute_arguments_destroy_type

  type, bind(c) :: kim_model_compute_arguments_type
    private
    type(c_ptr) :: p
  end type kim_model_compute_arguments_type

  type, bind(c) :: kim_model_compute_type
    private
    type(c_ptr) :: p
  end type kim_model_compute_type

  type, bind(c) :: kim_model_create_type
    private
    type(c_ptr) :: p
  end type kim_model_create_type

  type, bind(c) :: kim_model_extension_type
    private
    type(c_ptr) :: p
  end type kim_model_extension_type

  type, bind(c) :: kim_model_destroy_type
    private
    type(c_ptr) :: p
  end type kim_model_destroy_type

  type, bind(c) :: kim_model_driver_create_type
    private
    type(c_ptr) :: p
  end type kim_model_driver_create_type

  type, bind(c) :: kim_model_type
    private
    type(c_ptr) :: p
  end type kim_model_type

  type, bind(c) :: kim_model_refresh_type
    private
    type(c_ptr) :: p
  end type kim_model_refresh_type

  type, bind(c) :: kim_model_write_parameterized_model_type
    private
    type(c_ptr) :: p
  end type kim_model_write_parameterized_model_type

  type, bind(c) :: kim_simulator_model_type
    private
    type(c_ptr) :: p
  end type kim_simulator_model_type
end module kim_interoperable_types_module

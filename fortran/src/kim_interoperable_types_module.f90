!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2022, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!
! SPDX-License-Identifier: LGPL-2.1-or-later
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!

!
! Release: This file is part of the kim-api-2.3.0 package.
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

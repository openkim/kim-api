!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2021, Regents of the University of Minnesota.
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
! Release: This file is part of the kim-api.git repository.
!

module kim_model_headers_module
  use, intrinsic :: iso_c_binding

  use kim_model_create_module
  use kim_log_verbosity_module
  use kim_language_name_module
  use kim_numbering_module
  use kim_model_routine_name_module
  use kim_species_name_module
  use kim_support_status_module
  use kim_unit_system_module
  use kim_model_compute_arguments_create_module
  use kim_compute_argument_name_module
  use kim_data_type_module
  use kim_compute_callback_name_module
  use kim_model_compute_arguments_module
  use kim_model_compute_arguments_destroy_module
  use kim_model_compute_module
  use kim_model_extension_module
  use kim_model_refresh_module
  use kim_model_write_parameterized_model_module
  use kim_model_destroy_module

  public
end module kim_model_headers_module

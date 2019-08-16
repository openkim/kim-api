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
! Copyright (c) 2016--2019, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-2.1.3 package.
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

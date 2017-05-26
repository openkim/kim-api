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


module kim_model_module
  use, intrinsic :: iso_c_binding
  use :: kim_language_name_module
  implicit none
  !private  ! so kim_language_name_module is public

  public &
    kim_model_type, &
    kim_model_create, &
    kim_model_destroy, &
    kim_model_create_compute_arguments, &
    kim_model_destroy_compute_arguments, &
    kim_model_get_influence_distance, &
    kim_model_get_cutoffs, &
    kim_model_print, &
    kim_model_compute, &
    kim_model_reinit, &
    kim_model_get_num_model_species, &
    kim_model_get_model_species, &
    kim_model_get_model_kim_string_length, &
    kim_model_get_model_kim_string, &
    kim_model_get_species_code, &
    kim_model_get_num_params, &
    kim_model_get_parameter_data_type, &
    kim_model_get_parameter, &
    kim_model_get_parameter_description, &
    kim_model_set_sim_buffer, &
    kim_model_get_sim_buffer

  type, bind(c) :: kim_model_type
    type(c_ptr) :: p
  end type kim_model_type

  interface
    subroutine kim_model_create(simulator_string, model_name, model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      character(len=*), intent(in) :: simulator_string
      character(len=*), intent(in) :: model_name
      type(kim_model_type), intent(out), pointer :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_create

    subroutine kim_model_destroy(model)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout), pointer :: model
    end subroutine kim_model_destroy

    subroutine kim_model_create_compute_arguments(model, arguments, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_model_compute_arguments_type), intent(out), pointer :: &
        arguments
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_create_compute_arguments

    subroutine kim_model_destroy_compute_arguments(model, arguments)
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_model_compute_arguments_type), intent(inout), &
        pointer :: arguments
    end subroutine kim_model_destroy_compute_arguments

    subroutine kim_model_get_influence_distance(model, influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(out) :: influence_distance
    end subroutine kim_model_get_influence_distance

    subroutine kim_model_get_cutoffs(model, number_of_cutoffs, cutoffs_ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_cutoffs
      type(c_ptr), intent(out) :: cutoffs_ptr
    end subroutine kim_model_get_cutoffs

    subroutine kim_model_print(model)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end subroutine kim_model_print

    subroutine kim_model_compute(model, arguments, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_model_compute_arguments_type), intent(in) :: arguments
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute

    subroutine kim_model_reinit(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_reinit

    subroutine kim_model_get_num_model_species(model, number_of_species)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_model_get_num_model_species

    subroutine kim_model_get_model_species(model, index, species_name, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_species

    subroutine kim_model_get_model_kim_string_length(model_name, &
      kim_string_length, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*), intent(in) :: model_name
      integer(c_int), intent(out) :: kim_string_length
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_kim_string_length

    subroutine kim_model_get_model_kim_string(model_name, &
      kim_string, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*), intent(in) :: model_name
      character(len=*), intent(out) :: kim_string
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_kim_string

    subroutine kim_model_get_species_code(model, species_name, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_species_code

    subroutine kim_model_get_num_params(model, number_of_parameters)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine kim_model_get_num_params

    subroutine kim_model_get_parameter_data_type(model, index, data_type, ierr)
      use, intrinsic :: iso_c_binding
      use :: kim_parameter_module, only : kim_parameter_data_type_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_parameter_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_data_type

    subroutine kim_model_get_parameter(model, index, extent, ptr, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(out) :: extent
      type(c_ptr), intent(out) :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter

    subroutine kim_model_get_parameter_description(model, index, description, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      character(len=*), intent(out) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_description

    subroutine kim_model_set_sim_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_set_sim_buffer

    subroutine kim_model_get_sim_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_get_sim_buffer

end interface
end module kim_model_module

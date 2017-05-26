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


module kim_model_f_module
  implicit none
  private

  public &
    create, &
    destroy, &
    get_influence_distance, &
    get_cutoffs, &
    set_data, &
    set_method, &
    set_get_neigh, &
    set_neigh_object, &
    set_compute, &
    get_size, &
    print_model, &
    compute, &
    init, &
    reinit, &
    destroy_model, &
    get_num_model_species, &
    get_model_species, &
    get_model_kim_string_length, &
    get_model_kim_string, &
    get_species_code, &
    get_num_params, &
    get_parameter_data_type, &
    get_parameter, &
    get_parameter_description, &
    set_sim_buffer, &
    get_sim_buffer

  interface
    integer(c_int) function create(simulator_string, model_name, &
      model) bind(c, name="KIM_Model_create")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      character(c_char), intent(in) :: simulator_string(*)
      character(c_char), intent(in) :: model_name(*)
      type(c_ptr), intent(out) :: model
    end function create

    subroutine destroy(model) bind(c, name="KIM_Model_destroy")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(c_ptr), intent(inout) :: model
    end subroutine destroy

    subroutine get_influence_distance(model, influence_distance) &
      bind(c, name="KIM_Model_get_influence_distance")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(out) :: influence_distance
    end subroutine get_influence_distance

    subroutine get_cutoffs(model, number_of_cutoffs, cutoffs_ptr) &
      bind(c, name="KIM_Model_get_cutoffs")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_cutoffs
      type(c_ptr), intent(out) :: cutoffs_ptr
    end subroutine get_cutoffs

    integer(c_int) function set_data(model, argument_name, extent, ptr) &
      bind(c, name="KIM_Model_set_data")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
    end function set_data

    integer(c_int) function set_method(model, argument_name, extent, &
      language_name, fptr) bind(c, name="KIM_Model_set_method")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_language_name_module, only : &
        kim_language_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_method

    subroutine set_get_neigh(model, language_name, fptr) &
      bind(c, name="KIM_Model_set_get_neigh")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_get_neigh

    subroutine set_neigh_object(model, ptr) &
      bind(c, name="KIM_Model_set_neighObject")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_neigh_object

    integer(c_int) function set_compute(model, argument_name, flag) &
      bind(c, name="KIM_Model_set_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: flag
    end function set_compute

    integer(c_int) function get_size(model, argument_name, size) &
      bind(c, name="KIM_Model_get_size")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
    end function get_size

    subroutine print_model(model) bind(c, name="KIM_Model_print")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end subroutine print_model

    integer(c_int) function compute(model) bind(c, name="KIM_Model_compute")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end function compute

    integer(c_int) function init(model) bind(c, name="KIM_Model_init")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function init

    integer(c_int) function reinit(model) bind(c, name="KIM_Model_reinit")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function reinit

    integer(c_int) function destroy_model(model) &
      bind(c, name="KIM_Model_destroy_model")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function destroy_model

    subroutine get_num_model_species(model, number_of_species) &
      bind(c, name="KIM_Model_get_num_model_species")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_num_model_species

    integer(c_int) function get_model_species(model, index, species_name) &
      bind(c, name="KIM_Model_get_model_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_model_species

    integer(c_int) function get_model_kim_string_length(model_name, &
      kim_string_length) bind(c, name="KIM_Model_get_model_kim_string_length")
      use, intrinsic :: iso_c_binding
      implicit none
      character(c_char), intent(in) :: model_name(*)
      integer(c_int), intent(out) :: kim_string_length
    end function get_model_kim_string_length

    integer(c_int) function get_model_kim_string(model_name, kim_string) &
      bind(c, name="KIM_Model_get_model_kim_string")
      use, intrinsic :: iso_c_binding
      implicit none
      character(c_char), intent(in) :: model_name(*)
      type(c_ptr), intent(out) :: kim_string
    end function get_model_kim_string

    integer(c_int) function get_species_code(model, species_name, code) &
      bind(c, name="KIM_Model_get_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
    end function get_species_code

    subroutine get_num_params(model, number_of_parameters) &
      bind(c, name="KIM_Model_get_num_parames")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine get_num_params

    integer(c_int) function get_parameter_data_type(model, index, data_type) &
      bind(c, name="KIM_Model_get_parameter_data_type")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      use kim_parameter_module, only : kim_parameter_data_type_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_parameter_data_type_type), intent(out) :: data_type
    end function get_parameter_data_type

    integer(c_int) function get_parameter(model, index, extent, ptr) &
      bind(c, name="KIM_Model_get_parameter")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(out) :: extent
      type(c_ptr), intent(out) :: ptr
    end function get_parameter

    integer(c_int) function get_parameter_description(model, index, &
      description) bind(c, name="KIM_Model_get_parameter_description")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(c_ptr), intent(out) :: description
    end function get_parameter_description

    subroutine set_sim_buffer(model, ptr) &
      bind(c, name="KIM_Model_set_sim_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_sim_buffer

    subroutine get_sim_buffer(model, ptr) &
      bind(c, name="KIM_Model_get_sim_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine get_sim_buffer

  end interface
end module kim_model_f_module


! free functions to implement kim_model_module

subroutine kim_model_create(simulator_string, model_name, model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : create
  implicit none
  character(len=*), intent(in) :: simulator_string
  character(len=*), intent(in) :: model_name
  type(kim_model_type), intent(out), pointer :: model
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: pmodel

  ierr = create(trim(simulator_string)//c_null_char, &
    trim(model_name)//c_null_char, pmodel)
  call c_f_pointer(pmodel, model)
end subroutine kim_model_create

subroutine kim_model_destroy(model)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : destroy
  implicit none
  type(kim_model_type), intent(inout), pointer :: model

  type(c_ptr) :: pmodel
  pmodel = c_loc(model)
  call destroy(pmodel)
  nullify(model)
end subroutine kim_model_destroy

subroutine kim_model_get_influence_distance(model, influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_influence_distance
  implicit none
  type(kim_model_type), intent(in) :: model
  real(c_double), intent(out) :: influence_distance

  call get_influence_distance(model, influence_distance)
end subroutine kim_model_get_influence_distance

subroutine kim_model_get_cutoffs(model, number_of_cutoffs, cutoffs_ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_cutoffs
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_cutoffs
  type(c_ptr), intent(out) :: cutoffs_ptr

  call get_cutoffs(model, number_of_cutoffs, cutoffs_ptr)
end subroutine kim_model_get_cutoffs

subroutine kim_model_set_data(model, argument_name, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_data
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent
  type(c_ptr), intent(in), value :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = set_data(model, argument_name, extent, ptr)
end subroutine kim_model_set_data

subroutine kim_model_set_method(model, argument_name, extent, language_name, &
  fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_method
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_method(model, argument_name, extent, language_name, fptr)
end subroutine kim_model_set_method

subroutine kim_model_set_get_neigh(model, language_name, fptr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_get_neigh
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr

  call set_get_neigh(model, language_name, fptr)
end subroutine kim_model_set_get_neigh

subroutine kim_model_set_neigh_object(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_neigh_object
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(c_ptr), intent(in), value :: ptr

  call set_neigh_object(model, ptr)
end subroutine kim_model_set_neigh_object

subroutine kim_model_set_compute(model, argument_name, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_compute
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: flag
  integer(c_int), intent(out) :: ierr

  ierr = set_compute(model, argument_name, flag)
end subroutine kim_model_set_compute

subroutine kim_model_get_size(model, argument_name, size, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_size
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: size
  integer(c_int), intent(out) :: ierr

  ierr = get_size(model, argument_name, size)
end subroutine kim_model_get_size

subroutine kim_model_print(model)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : print_model
  implicit none
  type(kim_model_type), intent(in) :: model

  call print_model(model)
end subroutine kim_model_print

subroutine kim_model_compute(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : compute
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: ierr

  ierr = compute(model)
end subroutine kim_model_compute

subroutine kim_model_init(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : init
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = init(model)
end subroutine kim_model_init

subroutine kim_model_reinit(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : reinit
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = reinit(model)
end subroutine kim_model_reinit

subroutine kim_model_destroy_model(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : destroy_model
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = destroy_model(model)
end subroutine kim_model_destroy_model

subroutine kim_model_get_num_model_species(model, number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_num_model_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_species

  call get_num_model_species(model, number_of_species)
end subroutine kim_model_get_num_model_species

subroutine kim_model_get_model_species(model, index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_model_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_model_species(model, index, species_name)
end subroutine kim_model_get_model_species

subroutine kim_model_get_model_kim_string_length(model_name, &
  kim_string_length, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_f_module, only : get_model_kim_string_length
  implicit none
  character(len=*), intent(in) :: model_name
  integer(c_int), intent(out) :: kim_string_length
  integer(c_int), intent(out) :: ierr

  ierr = get_model_kim_string_length(trim(model_name)//c_null_char, &
    kim_string_length)
end subroutine kim_model_get_model_kim_string_length

subroutine kim_model_get_model_kim_string(model_name, &
  kim_string, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_f_module, only : get_model_kim_string
  implicit none
  character(len=*), intent(in) :: model_name
  character(len=*), intent(out) :: kim_string
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: p
  character(len=len(kim_string)), pointer :: fp
  integer(c_int) :: null_index

  ierr = get_model_kim_string(trim(model_name)//c_null_char, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  kim_string = fp(1:null_index)
end subroutine kim_model_get_model_kim_string

subroutine kim_model_get_species_code(model, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_species_code
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(out) :: code
  integer(c_int), intent(out) :: ierr

  ierr = get_species_code(model, species_name, code)
end subroutine kim_model_get_species_code

subroutine kim_model_get_num_params(model, number_of_parameters)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_num_params
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_parameters

  call get_num_params(model, number_of_parameters)
end subroutine kim_model_get_num_params

subroutine kim_model_get_parameter_data_type(model, index, data_type, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_parameter_module, only : kim_parameter_data_type_type
  use kim_model_f_module, only : get_parameter_data_type
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  type(kim_parameter_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: ierr

  ierr = get_parameter_data_type(model, index-1, data_type)
end subroutine kim_model_get_parameter_data_type

subroutine kim_model_get_parameter(model, index, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_parameter
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  integer(c_int), intent(out) :: extent
  type(c_ptr), intent(out) :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = get_parameter(model, index-1, extent, ptr)
end subroutine kim_model_get_parameter

subroutine kim_model_get_parameter_description(model, index, description, &
  ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_parameter_description
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  character(len=*), intent(out) :: description
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: p
  character(len=len(description)), pointer :: fp
  integer(c_int) :: null_index

  ierr = get_parameter_description(model, index-1, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  description = fp(1:null_index)
end subroutine kim_model_get_parameter_description

subroutine kim_model_set_sim_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_sim_buffer
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(c_ptr), intent(in), value :: ptr

  call set_sim_buffer(model, ptr)
end subroutine kim_model_set_sim_buffer

subroutine kim_model_get_sim_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_sim_buffer
  implicit none
  type(kim_model_type), intent(in) :: model
  type(c_ptr), intent(out) :: ptr

  call get_sim_buffer(model, ptr)
end subroutine kim_model_get_sim_buffer

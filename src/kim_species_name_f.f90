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


module kim_species_name_f_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    string, &
    get_number_of_species, &
    get_species

  interface
    type(c_ptr) function string(species_name) &
      bind(c, name="KIM_SpeciesNameString")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      implicit none
      type(kim_species_name_type), intent(in), value :: species_name
    end function string

    subroutine get_number_of_species(number_of_species) &
      bind(c, name="KIM_SPECIES_NAME_get_number_of_species")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_number_of_species

    integer(c_int) function get_species(index, species_name) &
      bind(c, name="KIM_SPECIES_NAME_get_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_species

  end interface
end module kim_species_name_f_module


! free functions to implement kim_species_name_module

subroutine kim_species_name_string(species_name, name_string)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_f_module, only : string
  implicit none
  type(kim_species_name_type), intent(in), value :: species_name
  character(len=*), intent(out) :: name_string

  type(c_ptr) :: p
  character(len=len(name_string)), pointer :: fp
  integer(c_int) :: null_index

  p = string(species_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  name_string = fp(1:null_index)
end subroutine kim_species_name_string

subroutine kim_species_name_get_number_of_species(number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_species_name_f_module, only : get_number_of_species
  implicit none
  integer(c_int), intent(out) :: number_of_species

  call get_number_of_species(number_of_species)
end subroutine kim_species_name_get_number_of_species

subroutine kim_species_name_get_species(index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_f_module, only : get_species
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_species(index, species_name)
end subroutine kim_species_name_get_species

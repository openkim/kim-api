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
! Copyright (c) 2016--2018, Regents of the University of Minnesota.
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
    from_string, &
    get_string, &
    get_number_of_species, &
    get_species_name

  interface
    type(kim_species_name_type) function from_string(string) &
      bind(c, name="KIM_SpeciesNameFromString")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(species_name) &
      bind(c, name="KIM_SpeciesNameString")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      implicit none
      type(kim_species_name_type), intent(in), value :: species_name
    end function get_string

    subroutine get_number_of_species(number_of_species) &
      bind(c, name="KIM_SPECIES_NAME_GetNumberOfSpeciesNames")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_number_of_species

    integer(c_int) function get_species_name(index, species_name) &
      bind(c, name="KIM_SPECIES_NAME_GetSpeciesName")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_species_name

  end interface
end module kim_species_name_f_module


! free functions to implement kim_species_name_module

subroutine kim_species_name_from_string(string, species_name)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_f_module, only : from_string
  implicit none
  character(len=*), intent(in) :: string
  type(kim_species_name_type), intent(out) :: species_name

  species_name = from_string(trim(string)//c_null_char)
end subroutine kim_species_name_from_string

logical function kim_species_name_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  implicit none
  type(kim_species_name_type), intent(in) :: left
  type(kim_species_name_type), intent(in) :: right

  kim_species_name_equal &
    = (left%species_name_id .eq. right%species_name_id)
end function kim_species_name_equal

logical function kim_species_name_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_module, only : operator(.eq.)
  implicit none
  type(kim_species_name_type), intent(in) :: left
  type(kim_species_name_type), intent(in) :: right

  kim_species_name_not_equal = .not. (left .eq. right)
end function kim_species_name_not_equal

subroutine kim_species_name_string(species_name, string)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_f_module, only : get_string
  implicit none
  type(kim_species_name_type), intent(in), value :: species_name
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)+1, kind=c_char), pointer :: fp
  integer(c_int) :: null_index

  p = get_string(species_name)
  if (c_associated(p)) then
    call c_f_pointer(p, fp)
    null_index = scan(fp, char(0))-1
    string = fp(1:null_index)
  else
    nullify(fp)
    string = ""
  end if
end subroutine kim_species_name_string

subroutine kim_species_name_get_number_of_species(number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_species_name_f_module, only : get_number_of_species
  implicit none
  integer(c_int), intent(out) :: number_of_species

  call get_number_of_species(number_of_species)
end subroutine kim_species_name_get_number_of_species

subroutine kim_species_name_get_species_name(index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_species_name_f_module, only : get_species_name
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_species_name(index-1, species_name)
end subroutine kim_species_name_get_species_name

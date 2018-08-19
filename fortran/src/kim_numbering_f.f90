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


module kim_numbering_f_module
  implicit none
  private

  public &
    from_string, &
    get_string, &
    get_number_of_numberings, &
    get_numbering


  interface
    type(kim_numbering_type) function from_string(string) &
      bind(c, name="KIM_Numbering_FromString")
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : &
        kim_numbering_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(numbering) &
      bind(c, name="KIM_Numbering_String")
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
    end function get_string

    subroutine get_number_of_numberings(number_of_numberings) &
      bind(c, name="KIM_NUMBERING_GetNumberOfNumberings")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_numberings
    end subroutine get_number_of_numberings

    integer(c_int) function get_numbering(index, numbering) &
      bind(c, name="KIM_NUMBERING_GetNumbering")
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_numbering_type), intent(out) :: numbering
    end function get_numbering
  end interface
end module kim_numbering_f_module

! free functions to implement kim_numbering_module

subroutine kim_numbering_from_string(string, numbering)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_numbering_f_module, only : from_string
  implicit none
  character(len=*, kind=c_char), intent(in) :: string
  type(kim_numbering_type), intent(out) :: numbering

  numbering = from_string(trim(string)//c_null_char)
end subroutine kim_numbering_from_string

logical function kim_numbering_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  implicit none
  type(kim_numbering_type), intent(in) :: left
  type(kim_numbering_type), intent(in) :: right

  kim_numbering_equal &
    = (left%numbering_id .eq. right%numbering_id)
end function kim_numbering_equal

logical function kim_numbering_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_numbering_module, only : operator(.eq.)
  implicit none
  type(kim_numbering_type), intent(in) :: left
  type(kim_numbering_type), intent(in) :: right

  kim_numbering_not_equal = .not. (left .eq. right)
end function kim_numbering_not_equal

subroutine kim_numbering_string(numbering, string)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_numbering_f_module, only : get_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_numbering_type), intent(in), value :: numbering
  character(len=*, kind=c_char), intent(out) :: string

  type(c_ptr) :: p

  p = get_string(numbering)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_numbering_string

subroutine kim_numbering_get_number_of_numberings(number_of_numberings)
  use, intrinsic :: iso_c_binding
  use kim_numbering_f_module, only : get_number_of_numberings
  implicit none
  integer(c_int), intent(out) :: number_of_numberings

  call get_number_of_numberings(number_of_numberings)
end subroutine kim_numbering_get_number_of_numberings

subroutine kim_numbering_get_numbering(index, numbering, ierr)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_numbering_f_module, only : get_numbering
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_numbering_type), intent(out) :: numbering
  integer(c_int), intent(out) :: ierr

  ierr = get_numbering(index-1, numbering)
end subroutine kim_numbering_get_numbering

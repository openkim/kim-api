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
! Release: This file is part of the kim-api-2.4.1 package.
!

module kim_convert_string_module
  implicit none
  private

  public &
    kim_convert_c_char_array_to_string, &
    kim_convert_c_char_ptr_to_string, &
    kim_convert_string_to_c_char_array

contains
  recursive subroutine kim_convert_c_char_array_to_string(c_char_array, string)
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=1, kind=c_char), intent(in) :: c_char_array(:)
    character(len=*, kind=c_char), intent(out) :: string

    integer(c_int) :: i
    integer(c_int) :: null_index
    integer(c_int) :: length

    length = len(string) + 1
    do null_index = 1, length
      if (c_char_array(null_index) == c_null_char) exit
    end do
    if (null_index == length) then
      null_index = len(string)
    else
      null_index = null_index - 1
    end if
    string = ""
    do i = 1, null_index
      string(i:i) = c_char_array(i)
    end do
  end subroutine kim_convert_c_char_array_to_string

  recursive subroutine kim_convert_c_char_ptr_to_string(c_char_ptr, string)
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr), intent(in) :: c_char_ptr
    character(len=*, kind=c_char), intent(out) :: string

    character(len=1, kind=c_char), pointer :: fp(:)
    integer(c_int) :: length

    if (c_associated(c_char_ptr)) then
      length = len(string) + 1
      call c_f_pointer(c_char_ptr, fp, [length])
      call kim_convert_c_char_array_to_string(fp, string)
    else
      string = ""
    end if
  end subroutine kim_convert_c_char_ptr_to_string

  recursive subroutine kim_convert_string_to_c_char_array(string, c_char_array)
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=*, kind=c_char), intent(in) :: string
    character(len=1, kind=c_char), intent(out) :: c_char_array(:)

    c_char_array(:) = trim(string)//c_null_char
  end subroutine kim_convert_string_to_c_char_array
end module kim_convert_string_module

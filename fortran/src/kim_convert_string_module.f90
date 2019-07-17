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
! Release: This file is part of the kim-api-2.1.0 package.
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

    length = len(string)+1
    do null_index=1,length
      if (c_char_array(null_index) .eq. c_null_char) exit
    end do
    if (null_index .eq. length) then
      null_index = len(string)
    else
      null_index = null_index - 1
    end if
    string = ""
    do i=1,null_index
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

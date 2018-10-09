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


module kim_language_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_language_name_type, &

    ! Constants
    KIM_LANGUAGE_NAME_CPP, &
    KIM_LANGUAGE_NAME_C, &
    KIM_LANGUAGE_NAME_FORTRAN, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_language_names, &
    kim_get_language_name


  type, bind(c) :: kim_language_name_type
    integer(c_int) :: language_name_id
  end type kim_language_name_type

  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_cpp") &
    :: KIM_LANGUAGE_NAME_CPP
  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_c") &
    :: KIM_LANGUAGE_NAME_C
  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_fortran") &
    :: KIM_LANGUAGE_NAME_FORTRAN

  interface operator (.eq.)
    module procedure kim_language_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_language_name_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_language_name_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_language_name_to_string
  end interface kim_to_string

contains
  logical function kim_language_name_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_language_name_type), intent(in) :: left
    type(kim_language_name_type), intent(in) :: right

    kim_language_name_equal &
      = (left%language_name_id .eq. right%language_name_id)
  end function kim_language_name_equal

  logical function kim_language_name_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_language_name_type), intent(in) :: left
    type(kim_language_name_type), intent(in) :: right

    kim_language_name_not_equal = .not. (left .eq. right)
  end function kim_language_name_not_equal

  subroutine kim_language_name_from_string(string, language_name)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      type(kim_language_name_type) function from_string(string) &
        bind(c, name="KIM_LanguageName_FromString")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_language_name_type), intent(out) :: language_name

    language_name = from_string(trim(string)//c_null_char)
  end subroutine kim_language_name_from_string

  subroutine kim_language_name_to_string(language_name, string)
    use, intrinsic :: iso_c_binding
    use kim_convert_string_module, only : kim_convert_string
    implicit none
    interface
      type(c_ptr) function get_string(language_name) &
        bind(c, name="KIM_LanguageName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        implicit none
        type(kim_language_name_type), intent(in), value :: language_name
      end function get_string
    end interface
    type(kim_language_name_type), intent(in), value :: language_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(language_name)
    if (c_associated(p)) then
      call kim_convert_string(p, string)
    else
      string = ""
    end if
  end subroutine kim_language_name_to_string

  subroutine kim_get_number_of_language_names( &
    number_of_language_names)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine get_number_of_language_names(number_of_language_names) &
        bind(c, name="KIM_LANGUAGE_NAME_GetNumberOfLanguageNames")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_language_names
      end subroutine get_number_of_language_names
    end interface
    integer(c_int), intent(out) :: number_of_language_names

    call get_number_of_language_names(number_of_language_names)
  end subroutine kim_get_number_of_language_names

  subroutine kim_get_language_name(index, language_name, ierr)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      integer(c_int) function get_language_name(index, language_name) &
        bind(c, name="KIM_LANGUAGE_NAME_GetLanguageName")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        integer(c_int), intent(in), value :: index
        type(kim_language_name_type), intent(out) :: language_name
      end function get_language_name
    end interface
    integer(c_int), intent(in), value :: index
    type(kim_language_name_type), intent(out) :: language_name
    integer(c_int), intent(out) :: ierr

    ierr = get_language_name(index-1, language_name)
  end subroutine kim_get_language_name
end module kim_language_name_module

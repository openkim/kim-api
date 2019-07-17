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


!> \brief \copybrief KIM::LanguageName
!!
!! \sa KIM::LanguageName, KIM_LanguageName
!!
!! \since 2.0
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
    kim_known, &
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_language_names, &
    kim_get_language_name


  !> \brief \copybrief KIM::LanguageName
  !!
  !! \sa KIM::LanguageName, KIM_LanguageName
  !!
  !! \since 2.0
  type, bind(c) :: kim_language_name_type
     !> \brief \copybrief KIM::LanguageName::languageNameID
     !!
     !! \sa KIM::LanguageName::languageNameID, KIM_LanguageName::languageNameID
     !!
     !! \since 2.0
    integer(c_int) :: language_name_id
  end type kim_language_name_type

  !> \brief \copybrief KIM::LANGUAGE_NAME::cpp
  !!
  !! \sa KIM::LANGUAGE_NAME::cpp, KIM_LANGUAGE_NAME_cpp
  !!
  !! \since 2.0
  type(kim_language_name_type), protected, save, &
    bind(c, name="KIM_LANGUAGE_NAME_cpp") &
    :: KIM_LANGUAGE_NAME_CPP

  !> \brief \copybrief KIM::LANGUAGE_NAME::c
  !!
  !! \sa KIM::LANGUAGE_NAME::c, KIM_LANGUAGE_NAME_c
  !!
  !! \since 2.0
  type(kim_language_name_type), protected, save, &
    bind(c, name="KIM_LANGUAGE_NAME_c") &
    :: KIM_LANGUAGE_NAME_C

  !> \brief \copybrief KIM::LANGUAGE_NAME::fortran
  !!
  !! \sa KIM::LANGUAGE_NAME::fortran, KIM_LANGUAGE_NAME_fortran
  !!
  !! \since 2.0
  type(kim_language_name_type), protected, save, &
    bind(c, name="KIM_LANGUAGE_NAME_fortran") &
    :: KIM_LANGUAGE_NAME_FORTRAN

  !> \brief \copybrief KIM::LanguageName::Known
  !!
  !! \sa KIM::LanguageName::Known, KIM_LanguageName_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_language_name_known
  end interface kim_known

  !> \brief \copybrief KIM::LanguageName::operator==()
  !!
  !! \sa KIM::LanguageName::operator==(), KIM_LanguageName_Equal
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_language_name_equal
  end interface operator (.eq.)

  !> \brief \copybrief KIM::LanguageName::operator!=()
  !!
  !! \sa KIM::LanguageName::operator!=(), KIM_LanguageName_NotEqual
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_language_name_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::LanguageName::LanguageName(std::string const &)
  !!
  !! \sa KIM::LanguageName::LanguageName(std::string const &),
  !! KIM_LanguageName_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_language_name_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::LanguageName::ToString
  !!
  !! \sa KIM::LanguageName::ToString, KIM_LanguageName_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_language_name_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::LanguageName::Known
  !!
  !! \sa KIM::LanguageName::Known, KIM_LanguageName_Known
  !!
  !! \since 2.0
  logical recursive function kim_language_name_known(language_name)
    implicit none
    interface
      integer(c_int) recursive function known(language_name) &
        bind(c, name="KIM_LanguageName_Known")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        implicit none
        type(kim_language_name_type), intent(in), value :: language_name
      end function known
    end interface
    type(kim_language_name_type), intent(in) :: language_name

    kim_language_name_known = (known(language_name) /= 0)
  end function kim_language_name_known

  !> \brief \copybrief KIM::LanguageName::operator==()
  !!
  !! \sa KIM::LanguageName::operator==(), KIM_LanguageName_Equal
  !!
  !! \since 2.0
  logical recursive function kim_language_name_equal(lhs, rhs)
    implicit none
    type(kim_language_name_type), intent(in) :: lhs
    type(kim_language_name_type), intent(in) :: rhs

    kim_language_name_equal &
      = (lhs%language_name_id .eq. rhs%language_name_id)
  end function kim_language_name_equal

  !> \brief \copybrief KIM::LanguageName::operator!=()
  !!
  !! \sa KIM::LanguageName::operator!=(), KIM_LanguageName_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_language_name_not_equal(lhs, rhs)
    implicit none
    type(kim_language_name_type), intent(in) :: lhs
    type(kim_language_name_type), intent(in) :: rhs

    kim_language_name_not_equal = .not. (lhs .eq. rhs)
  end function kim_language_name_not_equal

  !> \brief \copybrief KIM::LanguageName::LanguageName(std::string const &)
  !!
  !! \sa KIM::LanguageName::LanguageName(std::string const &),
  !! KIM_LanguageName_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_language_name_from_string(string, language_name)
    implicit none
    interface
      type(kim_language_name_type) recursive function from_string(string) &
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

  !> \brief \copybrief KIM::LanguageName::ToString
  !!
  !! \sa KIM::LanguageName::ToString, KIM_LanguageName_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_language_name_to_string(language_name, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(language_name) &
        bind(c, name="KIM_LanguageName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        implicit none
        type(kim_language_name_type), intent(in), value :: language_name
      end function get_string
    end interface
    type(kim_language_name_type), intent(in) :: language_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(language_name)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_language_name_to_string

  !> \brief \copybrief KIM::LANGUAGE_NAME::GetNumberOfLanguageNames
  !!
  !! \sa KIM::LANGUAGE_NAME::GetNumberOfLanguageNames,
  !! KIM_LANGUAGE_NAME_GetNumberOfLanguageNames
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_language_names( &
    number_of_language_names)
    implicit none
    interface
      recursive subroutine get_number_of_language_names( &
        number_of_language_names) &
        bind(c, name="KIM_LANGUAGE_NAME_GetNumberOfLanguageNames")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_language_names
      end subroutine get_number_of_language_names
    end interface
    integer(c_int), intent(out) :: number_of_language_names

    call get_number_of_language_names(number_of_language_names)
  end subroutine kim_get_number_of_language_names

  !> \brief \copybrief KIM::LANGUAGE_NAME::GetLanguageName
  !!
  !! \sa KIM::LANGUAGE_NAME::GetLanguageName, KIM_LANGUAGE_NAME_GetLanguageName
  !!
  !! \since 2.0
  recursive subroutine kim_get_language_name(index, language_name, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_language_name(index, &
        language_name) bind(c, name="KIM_LANGUAGE_NAME_GetLanguageName")
        use, intrinsic :: iso_c_binding
        import kim_language_name_type
        integer(c_int), intent(in), value :: index
        type(kim_language_name_type), intent(out) :: language_name
      end function get_language_name
    end interface
    integer(c_int), intent(in) :: index
    type(kim_language_name_type), intent(out) :: language_name
    integer(c_int), intent(out) :: ierr

    ierr = get_language_name(index-1, language_name)
  end subroutine kim_get_language_name
end module kim_language_name_module

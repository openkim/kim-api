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
! Release: This file is part of the kim-api.git repository.
!

!> \brief \copybrief KIM::SEM_VER
!!
!! \sa KIM::SEM_VER
!!
!! \since 2.0
module kim_sem_ver_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Routines
    kim_get_sem_ver, &
    kim_is_less_than, &
    kim_parse_sem_ver

contains
  !> \brief \copybrief KIM::SEM_VER::GetSemVer
  !!
  !! \sa KIM::SEM_VER::GetSemVer, KIM_SEM_VER_GetSemVer
  !!
  !! \since 2.0
  recursive subroutine kim_get_sem_ver(version)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_sem_ver() &
        bind(c, name="KIM_SEM_VER_GetSemVer")
        use, intrinsic :: iso_c_binding
        implicit none
      end function get_sem_ver
    end interface
    character(len=*, kind=c_char), intent(out) :: version

    type(c_ptr) :: p

    p = get_sem_ver()
    call kim_convert_c_char_ptr_to_string(p, version)
  end subroutine kim_get_sem_ver

  !> \brief \copybrief KIM::SEM_VER::IsLessThan
  !!
  !! \sa KIM::SEM_VER::IsLessThan, KIM_SEM_VER_IsLessThan
  !!
  !! \since 2.0
  recursive subroutine kim_is_less_than(lhs, rhs, is_less_than, ierr)
    implicit none
    interface
      integer(c_int) recursive function is_less_than_func(lhs, rhs, &
                                                          is_less_than) &
        bind(c, name="KIM_SEM_VER_IsLessThan")
        use, intrinsic :: iso_c_binding
        implicit none
        character(c_char), intent(in) :: lhs(*)
        character(c_char), intent(in) :: rhs(*)
        integer(c_int), intent(out) :: is_less_than
      end function is_less_than_func
    end interface
    character(len=*, kind=c_char), intent(in) :: lhs
    character(len=*, kind=c_char), intent(in) :: rhs
    integer(c_int), intent(out) :: is_less_than
    integer(c_int), intent(out) :: ierr

    ierr = is_less_than_func(trim(lhs)//c_null_char, trim(rhs)//c_null_char, &
                             is_less_than)
  end subroutine kim_is_less_than

  !> \brief \copybrief KIM::SEM_VER::ParseSemVer
  !!
  !! \sa KIM::SEM_VER::ParseSemVer, KIM_SEM_VER_ParseSemVer
  !!
  !! \since 2.0
  recursive subroutine kim_parse_sem_ver(version, major, minor, patch, &
                                         prerelease, build_metadata, ierr)
    use kim_convert_string_module, only: kim_convert_c_char_array_to_string
    implicit none
    interface
      integer(c_int) recursive function parse_sem_ver( &
        version, prerelease_length, build_metadata_length, major, minor, &
        patch, prerelease, build_metadata) &
        bind(c, name="KIM_SEM_VER_ParseSemVer")
        use, intrinsic :: iso_c_binding
        implicit none
        character(c_char), intent(in) :: version(*)
        integer(c_int), intent(in), value :: prerelease_length
        integer(c_int), intent(in), value :: build_metadata_length
        integer(c_int), intent(out) :: major
        integer(c_int), intent(out) :: minor
        integer(c_int), intent(out) :: patch
        type(c_ptr), intent(in), value :: prerelease
        type(c_ptr), intent(in), value :: build_metadata
      end function parse_sem_ver
    end interface
    character(len=*, kind=c_char), intent(in) :: version
    integer(c_int), intent(out) :: major
    integer(c_int), intent(out) :: minor
    integer(c_int), intent(out) :: patch
    character(len=*, kind=c_char), intent(out) :: prerelease
    character(len=*, kind=c_char), intent(out) :: build_metadata
    integer(c_int), intent(out) :: ierr

    character(len=1, kind=c_char), target :: prerelease_local(len(prerelease))
    character(len=1, kind=c_char), target :: &
      build_metadata_local(len(build_metadata))

    ierr = parse_sem_ver(trim(version)//c_null_char, len(prerelease), &
                         len(build_metadata), major, minor, patch, &
                         c_loc(prerelease_local), c_loc(build_metadata_local))
    call kim_convert_c_char_array_to_string(prerelease_local, prerelease)
    call kim_convert_c_char_array_to_string(build_metadata_local, &
                                            build_metadata)
  end subroutine kim_parse_sem_ver
end module kim_sem_ver_module

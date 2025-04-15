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

!> \brief \copybrief KIM::Collection
!!
!! \sa KIM::Collection, KIM_Collection
!!
!! \since 2.1
module kim_collection_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_collection_type, &
    ! Constants
    KIM_COLLECTION_SYSTEM, &
    KIM_COLLECTION_USER, &
    KIM_COLLECTION_ENVIRONMENT_VARIABLE, &
    KIM_COLLECTION_CURRENT_WORKING_DIRECTORY, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_collections, &
    kim_get_collection

  !> \brief \copybrief KIM::Collection
  !!
  !! \sa KIM::Collection, KIM_Collection
  !!
  !! \since 2.1
  type, bind(c) :: kim_collection_type
    !> \brief \copybrief KIM::Collection::collectionID
    !!
    !! \sa KIM::Collection::collectionID, KIM_Collection::collectionID
    !!
    !! \since 2.1
    integer(c_int) collection_id
  end type kim_collection_type

  !> \brief \copybrief KIM::COLLECTION::system
  !!
  !! \sa KIM::COLLECTION::system, KIM_COLLECTION_system
  !!
  !! \since 2.1
  type(kim_collection_type), protected, save, &
    bind(c, name="KIM_COLLECTION_system") &
    :: KIM_COLLECTION_SYSTEM

  !> \brief \copybrief KIM::COLLECTION::user
  !!
  !! \sa KIM::COLLECTION::user, KIM_COLLECTION_user
  !!
  !! \since 2.1
  type(kim_collection_type), protected, save, &
    bind(c, name="KIM_COLLECTION_user") &
    :: KIM_COLLECTION_USER

  !> \brief \copybrief KIM::COLLECTION::environmentVariable
  !!
  !! \sa KIM::COLLECTION::environmentVariable,
  !! KIM_COLLECTION_environmentVariable
  !!
  !! \since 2.1
  type(kim_collection_type), protected, save, &
    bind(c, name="KIM_COLLECTION_environmentVariable") &
    :: KIM_COLLECTION_ENVIRONMENT_VARIABLE

  !> \brief \copybrief KIM::COLLECTION::currentWorkingDirectory
  !!
  !! \sa KIM::COLLECTION::currentWorkingDirectory,
  !! KIM_COLLECTION_currentWorkingDirectory
  !!
  !! \since 2.1
  type(kim_collection_type), protected, save, &
    bind(c, name="KIM_COLLECTION_currentWorkingDirectory") &
    :: KIM_COLLECTION_CURRENT_WORKING_DIRECTORY

  !> \brief \copybrief KIM::Collection::Known
  !!
  !! \sa KIM::Collection::Known, KIM_Collection_Known
  !!
  !! \since 2.1
  interface kim_known
    module procedure kim_collection_known
  end interface kim_known

  !> \brief \copybrief KIM::Collection::operator==()
  !!
  !! \sa KIM::Collection::operator==(), KIM_Collection_Equal
  !!
  !! \since 2.1
  interface operator(.eq.)
    module procedure kim_collection_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::Collection::operator!=()
  !!
  !! \sa KIM::Collection::operator!=(), KIM_Collection_NotEqual
  !!
  !! \since 2.1
  interface operator(.ne.)
    module procedure kim_collection_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::Collection::Collection(std::string const &)
  !!
  !! \sa KIM::Collection::Collection(std::string const &),
  !! KIM_Collection_FromString
  !!
  !! \since 2.1
  interface kim_from_string
    module procedure kim_collection_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::Collection::ToString
  !!
  !! \sa KIM::Collection::ToString, KIM_Collection_ToString
  !!
  !! \since 2.1
  interface kim_to_string
    module procedure kim_collection_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::Collection::Known
  !!
  !! \sa KIM::Collection::Known, KIM_Collection_Known
  !!
  !! \since 2.1
  logical recursive function kim_collection_known(collection)
    implicit none
    interface
      integer(c_int) recursive function known(collection) &
        bind(c, name="KIM_Collection_Known")
        use, intrinsic :: iso_c_binding
        import kim_collection_type
        implicit none
        type(kim_collection_type), intent(in), value :: collection
      end function known
    end interface
    type(kim_collection_type), intent(in) :: collection

    kim_collection_known = (known(collection) /= 0)
  end function kim_collection_known

  !> \brief \copybrief KIM::Collection::operator==()
  !!
  !! \sa KIM::Collection::operator==(), KIM_Collection_Equal
  !!
  !! \since 2.1
  logical recursive function kim_collection_equal(lhs, rhs)
    implicit none
    type(kim_collection_type), intent(in) :: lhs
    type(kim_collection_type), intent(in) :: rhs

    kim_collection_equal &
      = (lhs%collection_id == rhs%collection_id)
  end function kim_collection_equal

  !> \brief \copybrief KIM::Collection::operator!=()
  !!
  !! \sa KIM::Collection::operator!=(), KIM_Collection_NotEqual
  !!
  !! \since 2.1
  logical recursive function kim_collection_not_equal(lhs, rhs)
    implicit none
    type(kim_collection_type), intent(in) :: lhs
    type(kim_collection_type), intent(in) :: rhs

    kim_collection_not_equal = .not. (lhs == rhs)
  end function kim_collection_not_equal

  !> \brief \copybrief KIM::Collection::Collection(std::string const &)
  !!
  !! \sa KIM::Collection::Collection(std::string const &),
  !! KIM_Collection_FromString
  !!
  !! \since 2.1
  recursive subroutine kim_collection_from_string(string, collection)
    implicit none
    interface
      type(kim_collection_type) recursive function from_string(string) &
        bind(c, name="KIM_Collection_FromString")
        use, intrinsic :: iso_c_binding
        import kim_collection_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_collection_type), intent(out) :: collection

    collection = from_string(trim(string)//c_null_char)
  end subroutine kim_collection_from_string

  !> \brief \copybrief KIM::Collection::ToString
  !!
  !! \sa KIM::Collection::ToString, KIM_Collection_ToString
  !!
  !! \since 2.1
  recursive subroutine kim_collection_to_string(collection, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(collection) &
        bind(c, name="KIM_Collection_ToString")
        use, intrinsic :: iso_c_binding
        import kim_collection_type
        implicit none
        type(kim_collection_type), intent(in), value :: collection
      end function get_string
    end interface
    type(kim_collection_type), intent(in) :: collection
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(collection)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_collection_to_string

  !> \brief \copybrief KIM::COLLECTION::GetNumberOfCollections
  !!
  !! \sa KIM::COLLECTION::GetNumberOfCollections,
  !! KIM_COLLECTION_GetNumberOfCollections
  !!
  !! \since 2.1
  recursive subroutine kim_get_number_of_collections(number_of_collections)
    implicit none
    interface
      recursive subroutine get_number_of_collections(number_of_collections) &
        bind(c, name="KIM_COLLECTION_GetNumberOfCollections")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_collections
      end subroutine get_number_of_collections
    end interface
    integer(c_int), intent(out) :: number_of_collections

    call get_number_of_collections(number_of_collections)
  end subroutine kim_get_number_of_collections

  !> \brief \copybrief KIM::COLLECTION::GetCollection
  !!
  !! \sa KIM::COLLECTION::GetCollection, KIM_COLLECTION_GetCollection
  !!
  !! \since 2.1
  recursive subroutine kim_get_collection(index, collection, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_collection(index, collection) &
        bind(c, name="KIM_COLLECTION_GetCollection")
        use, intrinsic :: iso_c_binding
        import kim_collection_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_collection_type), intent(out) :: collection
      end function get_collection
    end interface
    integer(c_int), intent(in) :: index
    type(kim_collection_type), intent(out) :: collection
    integer(c_int), intent(out) :: ierr

    ierr = get_collection(index - 1, collection)
  end subroutine kim_get_collection
end module kim_collection_module

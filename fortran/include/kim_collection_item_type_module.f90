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
! Release: This file is part of the kim-api-2.3.0 package.
!

!> \brief \copybrief KIM::CollectionItemType
!!
!! \sa KIM::CollectionItemType, KIM_CollectionItemType
!!
!! \since 2.1
module kim_collection_item_type_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_collection_item_type_type, &
    ! Constants
    KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER, &
    KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL, &
    KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_collection_item_types, &
    kim_get_collection_item_type

  !> \brief \copybrief KIM::CollectionItemType
  !!
  !! \sa KIM::CollectionItemType, KIM_CollectionItemType
  !!
  !! \since 2.1
  type, bind(c) :: kim_collection_item_type_type
    !> \brief \copybrief KIM::CollectionItemType::collectionItemTypeID
    !!
    !! \sa KIM::CollectionItemType::collectionItemTypeID,
    !! KIM_CollectionItemType::collectionItemTypeID
    !!
    !! \since 2.1
    integer(c_int) collection_item_type_id
  end type kim_collection_item_type_type

  !> \brief \copybrief KIM::COLLECTION_ITEM_TYPE::modelDriver
  !!
  !! \sa KIM::COLLECTION_ITEM_TYPE::modelDriver,
  !! KIM_COLLECTION_ITEM_TYPE_modelDriver
  !!
  !! \since 2.1
  type(kim_collection_item_type_type), protected, save, &
    bind(c, name="KIM_COLLECTION_ITEM_TYPE_modelDriver") &
    :: KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER

  !> \brief \copybrief KIM::COLLECTION_ITEM_TYPE::portableModel
  !!
  !! \sa KIM::COLLECTION_ITEM_TYPE::portableModel,
  !! KIM_COLLECTION_ITEM_TYPE_portableModel
  !!
  !! \since 2.1
  type(kim_collection_item_type_type), protected, save, &
    bind(c, name="KIM_COLLECTION_ITEM_TYPE_portableModel") &
    :: KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL

  !> \brief \copybrief KIM::COLLECTION_ITEM_TYPE::simulatorModel
  !!
  !! \sa KIM::COLLECTION_ITEM_TYPE::simulatorModel,
  !! KIM_COLLECTION_ITEM_TYPE_simulatorModel
  !!
  !! \since 2.1
  type(kim_collection_item_type_type), protected, save, &
    bind(c, name="KIM_COLLECTION_ITEM_TYPE_simulatorModel") &
    :: KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL

  !> \brief \copybrief KIM::CollectionItemType::Known
  !!
  !! \sa KIM::CollectionItemType::Known, KIM_CollectionItemType_Known
  !!
  !! \since 2.1
  interface kim_known
    module procedure kim_collection_item_type_known
  end interface kim_known

  !> \brief \copybrief KIM::CollectionItemType::operator==()
  !!
  !! \sa KIM::CollectionItemType::operator==(), KIM_CollectionItemType_Equal
  !!
  !! \since 2.1
  interface operator(.eq.)
    module procedure kim_collection_item_type_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::CollectionItemType::operator!=()
  !!
  !! \sa KIM::CollectionItemType::operator!=(), KIM_CollectionItemType_NotEqual
  !!
  !! \since 2.1
  interface operator(.ne.)
    module procedure kim_collection_item_type_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief <!--
  !! -->KIM::CollectionItemType::CollectionItemType(std::string const &)
  !!
  !! \sa KIM::CollectionItemType::CollectionItemType(std::string const &),
  !! KIM_CollectionItemType_FromString
  !!
  !! \since 2.1
  interface kim_from_string
    module procedure kim_collection_item_type_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::CollectionItemType::ToString
  !!
  !! \sa KIM::CollectionItemType::ToString, KIM_CollectionItemType_ToString
  !!
  !! \since 2.1
  interface kim_to_string
    module procedure kim_collection_item_type_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::CollectionItemType::Known
  !!
  !! \sa KIM::CollectionItemType::Known, KIM_CollectionItemType_Known
  !!
  !! \since 2.1
  logical recursive function kim_collection_item_type_known( &
    collection_item_type)
    implicit none
    interface
      integer(c_int) recursive function known(collection_item_type) &
        bind(c, name="KIM_CollectionItemType_Known")
        use, intrinsic :: iso_c_binding
        import kim_collection_item_type_type
        implicit none
        type(kim_collection_item_type_type), intent(in), value :: &
          collection_item_type
      end function known
    end interface
    type(kim_collection_item_type_type), intent(in) :: collection_item_type

    kim_collection_item_type_known = (known(collection_item_type) /= 0)
  end function kim_collection_item_type_known

  !> \brief \copybrief KIM::CollectionItemType::operator==()
  !!
  !! \sa KIM::CollectionItemType::operator==(), KIM_CollectionItemType_Equal
  !!
  !! \since 2.1
  logical recursive function kim_collection_item_type_equal(lhs, rhs)
    implicit none
    type(kim_collection_item_type_type), intent(in) :: lhs
    type(kim_collection_item_type_type), intent(in) :: rhs

    kim_collection_item_type_equal &
      = (lhs%collection_item_type_id == rhs%collection_item_type_id)
  end function kim_collection_item_type_equal

  !> \brief \copybrief KIM::CollectionItemType::operator!=()
  !!
  !! \sa KIM::CollectionItemType::operator!=(), KIM_CollectionItemType_NotEqual
  !!
  !! \since 2.1
  logical recursive function kim_collection_item_type_not_equal(lhs, rhs)
    implicit none
    type(kim_collection_item_type_type), intent(in) :: lhs
    type(kim_collection_item_type_type), intent(in) :: rhs

    kim_collection_item_type_not_equal = .not. (lhs == rhs)
  end function kim_collection_item_type_not_equal

  !> \brief \copybrief <!--
  !! -->KIM::CollectionItemType::CollectionItemType(std::string const &)
  !!
  !! \sa KIM::CollectionItemType::CollectionItemType(std::string const &),
  !! KIM_CollectionItemType_FromString
  !!
  !! \since 2.1
  recursive subroutine kim_collection_item_type_from_string( &
    string, collection_item_type)
    implicit none
    interface
      type(kim_collection_item_type_type) recursive function &
        from_string(string) bind(c, name="KIM_CollectionItemType_FromString")
        use, intrinsic :: iso_c_binding
        import kim_collection_item_type_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_collection_item_type_type), intent(out) :: collection_item_type

    collection_item_type = from_string(trim(string)//c_null_char)
  end subroutine kim_collection_item_type_from_string

  !> \brief \copybrief KIM::CollectionItemType::ToString
  !!
  !! \sa KIM::CollectionItemType::ToString, KIM_CollectionItemType_ToString
  !!
  !! \since 2.1
  recursive subroutine kim_collection_item_type_to_string( &
    collection_item_type, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(collection_item_type) &
        bind(c, name="KIM_CollectionItemType_ToString")
        use, intrinsic :: iso_c_binding
        import kim_collection_item_type_type
        implicit none
        type(kim_collection_item_type_type), intent(in), value :: &
          collection_item_type
      end function get_string
    end interface
    type(kim_collection_item_type_type), intent(in) :: collection_item_type
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(collection_item_type)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_collection_item_type_to_string

  !> \brief \copybrief KIM::COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes
  !!
  !! \sa KIM::COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes,
  !! KIM_COLLECTION_ITEM_TYPE_GetNumberOfCollectionItemTypes
  !!
  !! \since 2.1
  recursive subroutine kim_get_number_of_collection_item_types( &
    number_of_collection_item_types)
    implicit none
    interface
      recursive subroutine get_number_of_collection_item_types( &
        number_of_collection_item_types) &
        bind(c, name="KIM_COLLECTION_ITEM_TYPE_GetNumberOfCollectionItemTypes")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_collection_item_types
      end subroutine get_number_of_collection_item_types
    end interface
    integer(c_int), intent(out) :: number_of_collection_item_types

    call get_number_of_collection_item_types(number_of_collection_item_types)
  end subroutine kim_get_number_of_collection_item_types

  !> \brief \copybrief KIM::COLLECTION_ITEM_TYPE::GetCollectionItemType
  !!
  !! \sa KIM::COLLECTION_ITEM_TYPE::GetCollectionItemType,
  !! KIM_COLLECTION_ITEM_TYPE_GetCollectionItemType
  !!
  !! \since 2.1
  recursive subroutine kim_get_collection_item_type(index, &
                                                    collection_item_type, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_collection_item_type( &
        index, collection_item_type) &
        bind(c, name="KIM_COLLECTION_ITEM_TYPE_GetCollectionItemType")
        use, intrinsic :: iso_c_binding
        import kim_collection_item_type_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_collection_item_type_type), intent(out) :: collection_item_type
      end function get_collection_item_type
    end interface
    integer(c_int), intent(in) :: index
    type(kim_collection_item_type_type), intent(out) :: collection_item_type
    integer(c_int), intent(out) :: ierr

    ierr = get_collection_item_type(index - 1, collection_item_type)
  end subroutine kim_get_collection_item_type
end module kim_collection_item_type_module

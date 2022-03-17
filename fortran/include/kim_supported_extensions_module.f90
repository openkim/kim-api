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

!> \brief \copybrief KIM::SupportedExtensions
!!
!! \sa KIM::SupportedExtensions, KIM_SupportedExtensions
!!
!! \since 2.0
module kim_supported_extensions_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_supported_extensions_type, &
    ! Constants
    KIM_SUPPORTED_EXTENSIONS_ID, &
    KIM_MAX_EXTENSION_ID_LENGTH, &
    KIM_MAX_NUMBER_OF_EXTENSIONS

  character(len=*, kind=c_char), parameter &
    :: KIM_SUPPORTED_EXTENSIONS_ID = "KIM_SupportedExtensions"
  integer(c_int), parameter :: KIM_MAX_EXTENSION_ID_LENGTH = 128
  integer(c_int), parameter :: KIM_MAX_NUMBER_OF_EXTENSIONS = 64

  !> \brief \copybrief KIM::SupportedExtensions
  !!
  !! \sa KIM::SupportedExtensions, KIM_SupportedExtensions
  !!
  !! \since 2.0
  type, bind(c) :: kim_supported_extensions_type
    !> \brief \copybrief KIM::SupportedExtensions::numberOfSupportedExtensions
    !!
    !! \sa KIM::SupportedExtensions::numberOfSupportedExtensions,
    !! KIM_SupportedExtensions::numberOfSupportedExtensions
    !!
    !! \since 2.0
    integer(c_int) number_of_supported_extensions

    !> \brief \copybrief KIM::SupportedExtensions::supportedExtensionID
    !!
    !! \sa KIM::SupportedExtensions::supportedExtensionID,
    !! KIM_SupportedExtensions::supportedExtensionID
    !!
    !! \since 2.0
    character(c_char) supported_extension_id(KIM_MAX_EXTENSION_ID_LENGTH, &
                                             KIM_MAX_NUMBER_OF_EXTENSIONS)

    !> \brief \copybrief KIM::SupportedExtensions::supportedExtensionRequired
    !!
    !! \sa KIM::SupportedExtensions::supportedExtensionRequired,
    !! KIM_SupportedExtensions::supportedExtensionRequired
    !!
    !! \since 2.0
    integer(c_int) supported_extension_required(KIM_MAX_NUMBER_OF_EXTENSIONS)
  end type kim_supported_extensions_type
end module kim_supported_extensions_module

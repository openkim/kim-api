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

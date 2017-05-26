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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_attribute_id_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    mandatory_id, &
    not_supported_id, &
    required_id, &
    optional_id

  integer(c_int), parameter :: mandatory_id = 0
  integer(c_int), parameter :: not_supported_id = 1
  integer(c_int), parameter :: required_id = 2
  integer(c_int), parameter :: optional_id = 3
end module kim_attribute_id_module

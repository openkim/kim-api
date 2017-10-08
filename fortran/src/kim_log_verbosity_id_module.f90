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


module kim_log_verbosity_id_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    silent_id, &
    fatal_id, &
    error_id, &
    warning_id, &
    information_id, &
    debug_id

  integer(c_int), parameter :: silent_id = 0
  integer(c_int), parameter :: fatal_id = 1
  integer(c_int), parameter :: error_id = 2
  integer(c_int), parameter :: warning_id = 3
  integer(c_int), parameter :: information_id = 4
  integer(c_int), parameter :: debug_id = 5
end module kim_log_verbosity_id_module

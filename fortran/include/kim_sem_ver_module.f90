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


module kim_sem_ver_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_sem_ver_get_sem_ver, &
    kim_sem_ver_is_less_than, &
    kim_sem_ver_parse_sem_ver

  interface
    subroutine kim_sem_ver_get_sem_ver(version)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*, kind=c_char), intent(out) :: version
    end subroutine kim_sem_ver_get_sem_ver

    subroutine kim_sem_ver_is_less_than(version_a, version_b, is_less_than, &
      ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*, kind=c_char), intent(in) :: version_a
      character(len=*, kind=c_char), intent(in) :: version_b
      integer(c_int), intent(out) :: is_less_than
      integer(c_int), intent(out) :: ierr
    end subroutine kim_sem_ver_is_less_than

    subroutine kim_sem_ver_parse_sem_ver(version, major, minor, patch, &
      prerelease, build_metadata, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*, kind=c_char), intent(in) :: version
      integer(c_int), intent(out) :: major
      integer(c_int), intent(out) :: minor
      integer(c_int), intent(out) :: patch
      character(len=*, kind=c_char), intent(inout) :: prerelease
      character(len=*, kind=c_char), intent(inout) :: build_metadata
      integer(c_int), intent(out) :: ierr
    end subroutine kim_sem_ver_parse_sem_ver
  end interface
end module kim_sem_ver_module

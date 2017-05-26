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


module kim_particle_numbering_module
  use, intrinsic :: iso_c_binding
  use kim_particle_numbering_id_module
  implicit none
  private

  public &
    kim_particle_numbering_type, &
    kim_particle_numbering_string, &

    kim_particle_numbering_zero_based, &
    kim_particle_numbering_one_based

  type, bind(c) :: kim_particle_numbering_type
    integer(c_int) :: particle_numbering_id
  end type kim_particle_numbering_type

  type(kim_particle_numbering_type), parameter :: &
    kim_particle_numbering_zero_based = &
    kim_particle_numbering_type(zero_based_id)
  type(kim_particle_numbering_type), parameter :: &
    kim_particle_numbering_one_based = &
    kim_particle_numbering_type(one_based_id)

  interface
    subroutine kim_particle_numbering_string(particle_numbering, name_string)
      import kim_particle_numbering_type
      implicit none
      type(kim_particle_numbering_type), intent(in), value :: particle_numbering
      character(len=*), intent(out) :: name_string
    end subroutine kim_particle_numbering_string
  end interface
end module kim_particle_numbering_module

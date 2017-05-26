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


module kim_compute_simulator_compute_arguments_module
  use, intrinsic :: iso_c_binding
  use :: kim_language_name_module
  implicit none
  !private  ! so kim_language_name_module is public

  public &
    kim_compute_simulator_compute_arguments_type, &
    kim_compute_simulator_compute_arguments_get_neigh_object, &
    kim_compute_simulator_compute_arguments_get_neigh, &
    kim_compute_simulator_compute_arguments_process_dedr, &
    kim_compute_simulator_compute_arguments_process_d2edr2, &
    kim_compute_simulator_compute_arguments_get_data, &
    kim_compute_simulator_compute_arguments_get_compute, &
    kim_compute_simulator_compute_arguments_get_p_dedr_compute, &
    kim_compute_simulator_compute_arguments_get_p_d2edr2_compute, &
    kim_compute_simulator_compute_arguments_get_size

  type, bind(c) :: kim_compute_simulator_compute_arguments_type
    type(c_ptr) :: p
  end type kim_compute_simulator_compute_arguments_type

  interface
    subroutine kim_compute_simulator_compute_arguments_get_neigh_object( &
      arguments, ptr)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_compute_simulator_compute_arguments_get_neigh_object

    subroutine kim_compute_simulator_compute_arguments_get_neigh(arguments, &
      neighbor_list_index, particle_number, number_of_neighbors, &
      neighbors_of_particle, ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(in), value :: neighbor_list_index
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      type(c_ptr), intent(out) :: neighbors_of_particle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_get_neigh

    subroutine kim_compute_simulator_compute_arguments_process_dedr( &
      arguments, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      type(c_ptr), intent(in) :: dx
      integer(c_int), intent(in), value :: i
      integer(c_int), intent(in), value :: j
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_process_dedr

    subroutine kim_compute_simulator_compute_arguments_process_d2edr2( &
      arguments, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      real(c_double), intent(in), value :: de
      type(c_ptr), intent(in), value :: r
      type(c_ptr), intent(in), value :: dx
      type(c_ptr), intent(in), value :: i
      type(c_ptr), intent(in), value :: j
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_process_d2edr2

    subroutine kim_compute_simulator_compute_arguments_get_data(arguments, &
      argument_name, ptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_get_data

    subroutine kim_compute_simulator_compute_arguments_get_compute(arguments, &
      argument_name, flag, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: flag
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_get_compute

    subroutine kim_compute_simulator_compute_arguments_get_p_dedr_compute( &
      arguments, flag)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(out) :: flag
    end subroutine kim_compute_simulator_compute_arguments_get_p_dedr_compute

    subroutine kim_compute_simulator_compute_arguments_get_p_d2edr2_compute( &
      arguments, flag)
      use, intrinsic :: iso_c_binding
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(out) :: flag
    end subroutine kim_compute_simulator_compute_arguments_get_p_d2edr2_compute

    subroutine kim_compute_simulator_compute_arguments_get_size(arguments, &
      argument_name, size, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_simulator_compute_arguments_get_size

end interface
end module kim_compute_simulator_compute_arguments_module

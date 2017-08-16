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


module kim_model_refresh_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_refresh_type, &
    kim_model_refresh_set_influence_distance_pointer, &
    kim_model_refresh_set_cutoffs_pointer, &
    kim_model_refresh_get_model_buffer_pointer, &
    kim_model_refresh_log, &
    kim_model_refresh_string


  type, bind(c) :: kim_model_refresh_type
    private
    type(c_ptr) :: p
  end type kim_model_refresh_type

  interface
    subroutine kim_model_refresh_set_influence_distance_pointer(&
      model_refresh, influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_refresh_type
      implicit none
      type(kim_model_refresh_type), intent(inout) :: &
        model_refresh
      real(c_double), intent(in), target :: influence_distance
    end subroutine kim_model_refresh_set_influence_distance_pointer

    subroutine kim_model_refresh_set_cutoffs_pointer( &
      model_refresh, number_of_cutoffs, cutoffs)
      use, intrinsic :: iso_c_binding
      import kim_model_refresh_type
      implicit none
      type(kim_model_refresh_type), intent(inout) :: &
        model_refresh
      integer(c_int), intent(in), value :: number_of_cutoffs
      real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)
    end subroutine kim_model_refresh_set_cutoffs_pointer

    subroutine kim_model_refresh_get_model_buffer_pointer( &
      model_refresh, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_refresh_type
      implicit none
      type(kim_model_refresh_type), intent(in) :: &
        model_refresh
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_refresh_get_model_buffer_pointer

    subroutine kim_model_refresh_log(model_refresh, &
      log_verbosity, message, line_number, file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_refresh_type
      implicit none
      type(kim_model_refresh_type), intent(in) :: &
        model_refresh
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*), intent(in) :: file_name
    end subroutine kim_model_refresh_log

    subroutine kim_model_refresh_string(model_refresh, string)
      use, intrinsic :: iso_c_binding
      import kim_model_refresh_type
      implicit none
      type(kim_model_refresh_type), intent(in) :: &
        model_refresh
      character(len=*), intent(out) :: string
    end subroutine kim_model_refresh_string
  end interface
end module kim_model_refresh_module

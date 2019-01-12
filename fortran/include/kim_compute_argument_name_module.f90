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


module kim_compute_argument_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_compute_argument_name_type, &

    ! Constants
    KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING, &
    KIM_COMPUTE_ARGUMENT_NAME_COORDINATES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_VIRIAL, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_VIRIAL, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_compute_argument_names, &
    kim_get_compute_argument_name, &
    kim_get_compute_argument_data_type


  type, bind(c) :: kim_compute_argument_name_type
    integer(c_int) compute_argument_name_id
  end type kim_compute_argument_name_type

  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles") &
    :: KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleContributing") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_coordinates") &
    :: KIM_COMPUTE_ARGUMENT_NAME_COORDINATES
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialEnergy") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialForces") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialVirial") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_VIRIAL
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_VIRIAL

  interface operator (.eq.)
    module procedure kim_compute_argument_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_compute_argument_name_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_compute_argument_name_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_compute_argument_name_to_string
  end interface kim_to_string

contains
  logical recursive function kim_compute_argument_name_equal(lhs, rhs)
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: lhs
    type(kim_compute_argument_name_type), intent(in) :: rhs

    kim_compute_argument_name_equal &
      = (lhs%compute_argument_name_id .eq. rhs%compute_argument_name_id)
  end function kim_compute_argument_name_equal

  logical recursive function kim_compute_argument_name_not_equal(lhs, rhs)
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: lhs
    type(kim_compute_argument_name_type), intent(in) :: rhs

    kim_compute_argument_name_not_equal = .not. (lhs .eq. rhs)
  end function kim_compute_argument_name_not_equal

  recursive subroutine kim_compute_argument_name_from_string(string, &
    compute_argument_name)
    implicit none
    interface
      type(kim_compute_argument_name_type) recursive function from_string( &
        string) bind(c, name="KIM_ComputeArgumentName_FromString")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_compute_argument_name_type), intent(out) :: compute_argument_name

    compute_argument_name = from_string(trim(string)//c_null_char)
  end subroutine kim_compute_argument_name_from_string

  recursive subroutine kim_compute_argument_name_to_string( &
    compute_argument_name, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(compute_argument_name) &
        bind(c, name="KIM_ComputeArgumentName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
      end function get_string
    end interface
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(compute_argument_name)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_compute_argument_name_to_string

  recursive subroutine kim_get_number_of_compute_argument_names( &
    number_of_compute_argument_names)
    implicit none
    interface
      recursive subroutine get_number_of_compute_argument_names( &
        number_of_compute_argument_names) &
        bind(c, &
        name="KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_compute_argument_names
      end subroutine get_number_of_compute_argument_names
    end interface
    integer(c_int), intent(out) :: number_of_compute_argument_names

    call get_number_of_compute_argument_names(number_of_compute_argument_names)
  end subroutine kim_get_number_of_compute_argument_names

  recursive subroutine kim_get_compute_argument_name(index, &
    compute_argument_name, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_compute_argument_name(index, &
        compute_argument_name) &
        bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_compute_argument_name_type), intent(out) :: &
          compute_argument_name
      end function get_compute_argument_name
    end interface
    integer(c_int), intent(in) :: index
    type(kim_compute_argument_name_type), intent(out) :: compute_argument_name
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_name(index-1, compute_argument_name)
  end subroutine kim_get_compute_argument_name

  recursive subroutine kim_get_compute_argument_data_type( &
    compute_argument_name, &
    data_type, ierr)
    use kim_data_type_module, only : kim_data_type_type
    implicit none
    interface
      integer(c_int) recursive function get_compute_argument_data_type( &
        compute_argument_name, data_type) &
        bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType")
        use, intrinsic :: iso_c_binding
        use kim_data_type_module, only : kim_data_type_type
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(kim_data_type_type), intent(out) :: data_type
      end function get_compute_argument_data_type
    end interface
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    type(kim_data_type_type), intent(out) :: data_type
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_data_type(compute_argument_name, data_type)
  end subroutine kim_get_compute_argument_data_type
end module kim_compute_argument_name_module

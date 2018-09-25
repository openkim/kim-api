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


module kim_compute_argument_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private &
    kim_compute_argument_name_equal, &
    kim_compute_argument_name_not_equal

  public &
    kim_compute_argument_name_type, &
    kim_compute_argument_name_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_compute_argument_name_string, &

    kim_compute_argument_name_number_of_particles, &
    kim_compute_argument_name_particle_species_codes, &
    kim_compute_argument_name_particle_contributing, &
    kim_compute_argument_name_coordinates, &
    kim_compute_argument_name_partial_energy, &
    kim_compute_argument_name_partial_forces, &
    kim_compute_argument_name_partial_particle_energy, &
    kim_compute_argument_name_partial_virial, &
    kim_compute_argument_name_partial_particle_virial, &

    kim_get_number_of_compute_argument_names, &
    kim_get_compute_argument_name, &
    kim_get_compute_argument_data_type


  type, bind(c) :: kim_compute_argument_name_type
    integer(c_int) compute_argument_name_id
  end type kim_compute_argument_name_type

  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles") &
    :: kim_compute_argument_name_number_of_particles
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes") &
    :: kim_compute_argument_name_particle_species_codes
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleContributing") &
    :: kim_compute_argument_name_particle_contributing
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_coordinates") &
    :: kim_compute_argument_name_coordinates
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialEnergy") &
    :: kim_compute_argument_name_partial_energy
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialForces") &
    :: kim_compute_argument_name_partial_forces
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy") &
    :: kim_compute_argument_name_partial_particle_energy
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialVirial") &
    :: kim_compute_argument_name_partial_virial
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial") &
    :: kim_compute_argument_name_partial_particle_virial

  interface operator (.eq.)
    module procedure kim_compute_argument_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_compute_argument_name_not_equal
  end interface operator (.ne.)

contains
  subroutine kim_compute_argument_name_from_string(string, &
    compute_argument_name)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      type(kim_compute_argument_name_type) function from_string(string) &
        bind(c, name="KIM_ComputeArgumentName_FromString")
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

  logical function kim_compute_argument_name_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: left
    type(kim_compute_argument_name_type), intent(in) :: right

    kim_compute_argument_name_equal &
      = (left%compute_argument_name_id .eq. right%compute_argument_name_id)
  end function kim_compute_argument_name_equal

  logical function kim_compute_argument_name_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: left
    type(kim_compute_argument_name_type), intent(in) :: right

    kim_compute_argument_name_not_equal = .not. (left .eq. right)
  end function kim_compute_argument_name_not_equal

  subroutine kim_compute_argument_name_string(compute_argument_name, string)
    use, intrinsic :: iso_c_binding
    use kim_convert_string_module, only : kim_convert_string
    implicit none
    interface
      type(c_ptr) function get_string(compute_argument_name) &
        bind(c, name="KIM_ComputeArgumentName_String")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
      end function get_string
    end interface
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(compute_argument_name)
    if (c_associated(p)) then
      call kim_convert_string(p, string)
    else
      string = ""
    end if
  end subroutine kim_compute_argument_name_string

  subroutine kim_get_number_of_compute_argument_names( &
    number_of_compute_argument_names)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine get_number_of_compute_argument_names( &
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

  subroutine kim_get_compute_argument_name(index, &
    compute_argument_name, ierr)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      integer(c_int) function get_compute_argument_name(index, &
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
    integer(c_int), intent(in), value :: index
    type(kim_compute_argument_name_type), intent(out) :: compute_argument_name
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_name(index-1, compute_argument_name)
  end subroutine kim_get_compute_argument_name

  subroutine kim_get_compute_argument_data_type( &
    compute_argument_name, &
    data_type, ierr)
    use, intrinsic :: iso_c_binding
    use kim_data_type_module, only : kim_data_type_type
    implicit none
    interface
      integer(c_int) function get_compute_argument_data_type( &
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
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    type(kim_data_type_type), intent(out) :: data_type
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_data_type(compute_argument_name, data_type)
  end subroutine kim_get_compute_argument_data_type
end module kim_compute_argument_name_module

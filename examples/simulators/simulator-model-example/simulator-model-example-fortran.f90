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
! Copyright (c) 2013--2019, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

module error
  use, intrinsic :: iso_c_binding
  implicit none

  public

contains
  recursive subroutine my_error(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *,"* Error : ", trim(message)
    stop 1
  end subroutine my_error

  recursive subroutine my_warning(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *,"* Warning : ", trim(message)
  end subroutine my_warning
end module error

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program collections_example_fortran
  use, intrinsic :: iso_c_binding
  use error
  use kim_simulator_headers_module
  implicit none

  integer(c_int) :: ierr
  integer(c_int) :: extent
  integer(c_int) :: no_fields
  integer(c_int) :: i
  integer(c_int) :: j
  type(kim_simulator_model_handle_type) :: sm

  character(len=2048, kind=c_char) s_name
  character(len=2048, kind=c_char) s_ver
  character(len=2048, kind=c_char) species
  character(len=2048, kind=c_char) field_name
  character(len=2048, kind=c_char) line
  character(len=2048, kind=c_char) dir_name
  character(len=2048, kind=c_char) spec_name
  character(len=2048, kind=c_char) param_name


  call kim_simulator_model_create( &
    "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu", sm, ierr)

  if (ierr /= 0) then
    call my_error("Can't create SM.")
  end if

  call kim_get_simulator_name_and_version(sm, s_name, s_ver)
  print *, "Simulator name    : ", trim(s_name)
  print *, "Simulator version : ", trim(s_ver)
  print *, ""

  call kim_get_number_of_supported_species(sm, extent)
  print *, "SM supports", extent, " species:"
  do i=1,extent
    call kim_get_supported_species(sm, i, species, ierr)
    if (ierr /= 0) then
      call my_error("Unable to get species.")
    else
      print '("	",I2," ",A)', i, trim(species)
    end if
  end do
  print *, ""

  call kim_add_template_map(sm, "atom-type-sym-list", "Pb Pb Au Pb", ierr)
  if (ierr /= 0) then
    call my_error("Unable to add template map.")
  end if
  call kim_close_template_map(sm)
  call kim_get_number_of_simulator_fields(sm, no_fields)
  print '("SM has ",I2," fields :")', no_fields
  do i=1,no_fields
    call kim_get_simulator_field_metadata(sm, i, extent, field_name, ierr)
    print '("  Field",I2," is ",A," and has ",I2," lines:")', &
      i, trim(field_name), extent

    do j=1,extent
      call kim_get_simulator_field_line(sm, i, j, line, ierr)
      if (ierr /= 0) then
        call my_error("Unable to get field line.")
      else
        print '("	",A)', trim(line)
      end if
    end do
  end do
  print *,""

  call kim_get_parameter_file_directory_name(sm, dir_name)
  print '("SM param dir name is ",A)', trim(dir_name)

  call kim_get_specification_file_name(sm, spec_name)
  print '("SM spec file name is ",A)', trim(spec_name)
  ierr = system("cat "//trim(dir_name)//"/"//trim(spec_name))

  call kim_get_number_of_parameter_files(sm, extent)
  print '("SM has ",I1," parameter files:")', extent
  do i=1,extent
    call kim_get_parameter_file_name(sm, i, param_name, ierr)
    if (ierr /= 0) then
      call my_error("Unable to get parameter file name.")
    else
      print '("Parameter file ",I2," has name ",A)', i, trim(param_name)
      ierr = system("cat "//trim(dir_name)//"/"//trim(param_name))
      print *,""
    end if
  end do

  call kim_simulator_model_destroy(sm)

end program collections_example_fortran

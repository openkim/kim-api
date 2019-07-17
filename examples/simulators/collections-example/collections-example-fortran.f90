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
end module error

module utilities
  implicit none

  public

contains
  subroutine dirs_for_collection(collection, col)
    use, intrinsic :: iso_c_binding
    use kim_collections_module
    use kim_collection_module
    use kim_collection_item_type_module
    implicit none
    type(kim_collection_type), intent(in) :: collection
    type(kim_collections_handle_type), intent(inout) :: col

    integer(c_int) ierr
    integer(c_int) i
    integer(c_int) extent
    character(len=2048, kind=c_char) coll_str
    character(len=2048, kind=c_char) item_type_str
    character(len=2048, kind=c_char) dir_str


    call kim_cache_list_of_directory_names(col, collection, &
      KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER, extent, ierr)
    call kim_to_string(collection, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_directory_name(col, i, dir_str, ierr)
      print '("	",A)', trim(dir_str)
    end do

    call kim_cache_list_of_directory_names(col, collection, &
      KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL, extent, ierr)
    call kim_to_string(collection, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_directory_name(col, i, dir_str, ierr)
      print '("	",A)', trim(dir_str)
    end do

    call kim_cache_list_of_directory_names(col, collection, &
      KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL, extent, ierr)
    call kim_to_string(collection, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_directory_name(col, i, dir_str, ierr)
      print '("	",A)', trim(dir_str)
    end do
  end subroutine dirs_for_collection

  subroutine names_for_collection(kc, col)
    use, intrinsic :: iso_c_binding
    use kim_collections_module
    use kim_collection_module
    use kim_collection_item_type_module
    implicit none
    type(kim_collection_type), intent(in) :: kc
    type(kim_collections_handle_type), intent(inout) :: col

    integer(c_int) ierr
    integer(c_int) i
    integer(c_int) extent
    character(len=2048, kind=c_char) coll_str
    character(len=2048, kind=c_char) item_type_str
    character(len=2048, kind=c_char) name_str

    call kim_cache_list_of_item_names_by_collection_and_type(col, kc, &
      KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER, extent, ierr)
    call kim_to_string(kc, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_item_name_by_collection_and_type(col, i, name_str, ierr)
      print '("	",A)', trim(name_str)
    end do

    call kim_cache_list_of_item_names_by_collection_and_type(col, kc, &
      KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL, extent, ierr)
    call kim_to_string(kc, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_item_name_by_collection_and_type(col, i, name_str, ierr)
      print '("	",A)', trim(name_str)
    end do

    call kim_cache_list_of_item_names_by_collection_and_type(col, kc, &
      KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL, extent, ierr)
    call kim_to_string(kc, coll_str)
    call kim_to_string(KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL, item_type_str)
    print '(A,":",A," :")', trim(coll_str), trim(item_type_str)

    do i=1,extent
      call kim_get_item_name_by_collection_and_type(col, i, name_str, ierr)
      print '("	",A)', trim(name_str)
    end do
  end subroutine names_for_collection
end module utilities

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program collections_example_fortran
  use, intrinsic :: iso_c_binding
  use error
  use utilities
  use kim_collection_module
  use kim_collection_item_type_module
  use kim_collections_module
  implicit none

  integer(c_int) :: ierr
  integer(c_int) :: extent
  integer(c_int) :: i
  type(kim_collections_handle_type) :: col
  type(kim_collection_item_type_type) it
  character(len=2048, kind=c_char) project_name
  character(len=2048, kind=c_char) sem_ver
  character(len=2048, kind=c_char) name
  character(len=2048, kind=c_char) value
  character(len=2048, kind=c_char) file_name
  character(len=2048, kind=c_char) item_type_str

  call kim_collections_create(col, ierr)

  if (ierr /= 0) then
    call my_error("Unable to create collections object.")
  end if

  call kim_get_project_name_and_sem_ver(col, project_name, sem_ver)
  print *, "Project : ", trim(project_name)
  print *, "semVer  : ", trim(sem_ver)
  print *, ""



  it = KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER
  call kim_get_environment_variable_name(col, it, name, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," env name : ",A)', trim(item_type_str), trim(name)
  print *, ""

  it = KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL
  call kim_get_environment_variable_name(col, it, name, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," env name : ",A)', trim(item_type_str), trim(name)
  print *, ""

  it = KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL
  call kim_get_environment_variable_name(col, it, name, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," env name : ",A)', trim(item_type_str), trim(name)
  print *, ""

  call kim_get_configuration_file_environment_variable(col, name, value)
  print '("config file env name  : ",A)', trim(name)
  print '("config file env value : ",A)', trim(value)
  print *, ""

  call kim_get_configuration_file_name(col, file_name)
  print '("config file name : ",A)', trim(file_name)
  print *, ""

  call dirs_for_collection(KIM_COLLECTION_SYSTEM, col)
  call dirs_for_collection(KIM_COLLECTION_USER, col)
  call dirs_for_collection(KIM_COLLECTION_ENVIRONMENT_VARIABLE, col)
  call dirs_for_collection(KIM_COLLECTION_CURRENT_WORKING_DIRECTORY, col)
  print *, ""

  call names_for_collection(KIM_COLLECTION_SYSTEM, col)
  call names_for_collection(KIM_COLLECTION_USER, col)
  call names_for_collection(KIM_COLLECTION_ENVIRONMENT_VARIABLE, col)
  call names_for_collection(KIM_COLLECTION_CURRENT_WORKING_DIRECTORY, col)
  print *, ""

  it = KIM_COLLECTION_ITEM_TYPE_MODEL_DRIVER
  call kim_cache_list_of_item_names_by_type(col, it, extent, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," :")', trim(item_type_str)
  do i=1,extent
    call kim_get_item_name_by_type(col, i, name, ierr)
    print '("	",A)', trim(name)
  end do

  it = KIM_COLLECTION_ITEM_TYPE_PORTABLE_MODEL
  call kim_cache_list_of_item_names_by_type(col, it, extent, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," :")', trim(item_type_str)
  do i=1,extent
    call kim_get_item_name_by_type(col, i, name, ierr)
    print '("	",A)', trim(name)
  end do

  it = KIM_COLLECTION_ITEM_TYPE_SIMULATOR_MODEL
  call kim_cache_list_of_item_names_by_type(col, it, extent, ierr)
  call kim_to_string(it, item_type_str)
  print '(A," :")', trim(item_type_str)
  do i=1,extent
    call kim_get_item_name_by_type(col, i, name, ierr)
    print '("	",A)', trim(name)
  end do

  call kim_collections_destroy(col)

end program collections_example_fortran

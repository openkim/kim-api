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
! Copyright (c) 2016--2020, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-2.2.1 package.
!

!> \brief \copybrief KIM::Collections
!!
!! \sa KIM::Collections, KIM_Collections
!!
!! \since 2.1
module kim_collections_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_collections_handle_type, &
    ! Constants
    KIM_COLLECTIONS_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_collections_create, &
    kim_collections_destroy, &
    kim_get_item_type, &
    kim_get_item_library_file_name_and_collection, &
    kim_cache_list_of_item_metadata_files, &
    kim_get_item_metadata_file_length, &
    kim_get_item_metadata_file_values, &
    kim_cache_list_of_item_names_by_type, &
    kim_get_item_name_by_type, &
    kim_cache_list_of_item_names_by_collection_and_type, &
    kim_get_item_name_by_collection_and_type, &
    kim_get_item_library_file_name_by_collection_and_type, &
    kim_cache_list_of_item_metadata_files_by_collection_and_type, &
    kim_get_item_metadata_file_length_by_collection_and_type, &
    kim_get_item_metadata_file_values_by_collection_and_type, &
    kim_get_project_name_and_sem_ver, &
    kim_get_environment_variable_name, &
    kim_get_configuration_file_environment_variable, &
    kim_get_configuration_file_name, &
    kim_cache_list_of_directory_names, &
    kim_get_directory_name, &
    kim_set_log_id, &
    kim_push_log_verbosity, &
    kim_pop_log_verbosity

  !> \brief \copybrief KIM::Collections
  !!
  !! \sa KIM::Collections, KIM_Collections
  !!
  !! \since 2.1
  type, bind(c) :: kim_collections_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_collections_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.1
  type(kim_collections_handle_type), protected, save &
    :: KIM_COLLECTIONS_NULL_HANDLE

  !> \brief Compares kim_collections_handle_type's for equality.
  !!
  !! \since 2.1
  interface operator(.eq.)
    module procedure kim_collections_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_collections_handle_type's for inequality.
  !!
  !! \since 2.1
  interface operator(.ne.)
    module procedure kim_collections_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::Collections::GetItemType
  !!
  !! \sa KIM::Collections::GetItemType, KIM_Collections_GetItemType
  !!
  !! \since 2.1
  interface kim_get_item_type
    module procedure kim_collections_get_item_type
  end interface kim_get_item_type

  !> \brief \copybrief KIM::Collections::GetItemLibraryFileNameAndCollection
  !!
  !! \sa KIM::Collections::GetItemLibraryFileNameAndCollection,
  !! KIM_Collections_GetItemLibraryFileNameAndCollection
  !!
  !! \since 2.1
  interface kim_get_item_library_file_name_and_collection
    module procedure kim_collections_get_item_library_file_name_and_collection
  end interface kim_get_item_library_file_name_and_collection

  !> \brief \copybrief KIM::Collections::CacheListOfItemMetadataFiles
  !!
  !! \sa KIM::Collections::CacheListOfItemMetadataFiles,
  !! KIM_Collections_CacheListOfItemMetadataFiles
  !!
  !! \since 2.1
  interface kim_cache_list_of_item_metadata_files
    module procedure kim_collections_cache_list_of_item_metadata_files
  end interface kim_cache_list_of_item_metadata_files

  !> \brief Get item metadata file length and determine if the file is
  !! available as a string.
  !!
  !! \sa KIM::Collections::GetItemMetadataFile,
  !! KIM_Collections_GetItemMetadataFile
  !!
  !! \since 2.1
  interface kim_get_item_metadata_file_length
    module procedure kim_collections_get_item_metadata_file_length
  end interface kim_get_item_metadata_file_length

  !> \brief Get the item's metadata file values.
  !!
  !! \sa KIM::Collections::GetItemMetadataFile,
  !! KIM_Collections_GetItemMetadataFile
  !!
  !! \since 2.1
  interface kim_get_item_metadata_file_values
    module procedure kim_collections_get_item_metadata_file_values
  end interface kim_get_item_metadata_file_values

  !> \brief \copybrief KIM::Collections::CacheListOfItemNamesByType
  !!
  !! \sa KIM::Collections::CacheListOfItemNamesByType,
  !! KIM_Collections_CacheListOfItemNamesByType
  !!
  !! \since 2.1
  interface kim_cache_list_of_item_names_by_type
    module procedure kim_collections_cache_list_of_item_names_by_type
  end interface kim_cache_list_of_item_names_by_type

  !> \brief \copybrief KIM::Collections::GetItemNameByType
  !!
  !! \sa KIM::Collections::GetItemNameByType, KIM_Collections_GetItemNameByType
  !!
  !! \since 2.1
  interface kim_get_item_name_by_type
    module procedure kim_collections_get_item_name_by_type
  end interface kim_get_item_name_by_type

  !> \brief \copybrief KIM::Collections::CacheListOfItemNamesByCollectionAndType
  !!
  !! \sa KIM::Collections::CacheListOfItemNamesByCollectionAndType,
  !! KIM_Collections_CacheListOfItemNamesByCollectionAndType
  !!
  !! \since 2.1
  interface kim_cache_list_of_item_names_by_collection_and_type
    module procedure &
      kim_collections_cache_list_of_item_names_by_collection_and_type
  end interface kim_cache_list_of_item_names_by_collection_and_type

  !> \brief \copybrief KIM::Collections::GetItemNameByCollectionAndType
  !!
  !! \sa KIM::Collections::GetItemNameByCollectionAndType,
  !! KIM_Collections_GetItemNameByCollectionAndType
  !!
  !! \since 2.1
  interface kim_get_item_name_by_collection_and_type
    module procedure kim_collections_get_item_name_by_collection_and_type
  end interface kim_get_item_name_by_collection_and_type

  !> \brief \copybrief <!--
  !! -->KIM::Collections::GetItemLibraryFileNameByCollectionAndType
  !!
  !! \sa KIM::Collections::GetItemLibraryFileNameByCollectionAndType,
  !! KIM_Collections_GetItemLibraryFileNameByCollectionAndType
  !!
  !! \since 2.1
  interface kim_get_item_library_file_name_by_collection_and_type
    module procedure &
      kim_collections_get_item_library_file_name_by_coll_and_type
  end interface kim_get_item_library_file_name_by_collection_and_type

  !> \brief \copybrief <!--
  !! -->KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType
  !!
  !! \sa KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType,
  !! KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType
  !!
  !! \since 2.1
  interface kim_cache_list_of_item_metadata_files_by_collection_and_type
    module procedure &
      kim_colls_cache_list_of_item_metadata_files_by_coll_and_type
  end interface kim_cache_list_of_item_metadata_files_by_collection_and_type

  !> \brief Get item metadata file length and determine if the file is
  !! available as a string.
  !!
  !! \sa KIM::Collections::GetItemMetadataFileByCollectionAndType,
  !! KIM_Collections_GetItemMetadataFileByCollectionAndType
  !!
  !! \since 2.1
  interface kim_get_item_metadata_file_length_by_collection_and_type
    module procedure &
      kim_collections_get_item_metadata_file_length_by_coll_and_type
  end interface kim_get_item_metadata_file_length_by_collection_and_type

  !> \brief Get the item's metadata file values.
  !!
  !! \sa KIM::Collections::GetItemMetadataFileByCollectionAndType,
  !! KIM_Collections_GetItemMetadataFileByCollectionAndType
  !!
  !! \since 2.1
  interface kim_get_item_metadata_file_values_by_collection_and_type
    module procedure &
      kim_collections_get_item_metadata_file_values_by_coll_and_type
  end interface kim_get_item_metadata_file_values_by_collection_and_type

  !> \brief \copybrief KIM::Collections::GetProjectNameAndSemVer
  !!
  !! \sa KIM::Collections::GetProjectNameAndSemVer,
  !! KIM_Collections_GetProjectNameAndSemVer
  !!
  !! \since 2.1
  interface kim_get_project_name_and_sem_ver
    module procedure kim_collections_get_project_name_and_sem_ver
  end interface kim_get_project_name_and_sem_ver

  !> \brief \copybrief KIM::Collections::GetEnvironmentVariableName
  !!
  !! \sa KIM::Collections::GetEnvironmentVariableName,
  !! KIM_Collections_GetEnvironmentVariableName
  !!
  !! \since 2.1
  interface kim_get_environment_variable_name
    module procedure kim_collections_get_environment_variable_name
  end interface kim_get_environment_variable_name

  !> \brief \copybrief KIM::Collections::GetConfigurationFileEnvironmentVariable
  !!
  !! \sa KIM::Collections::GetConfigurationFileEnvironmentVariable,
  !! KIM_Collections_GetConfigurationFileEnvironmentVariable
  !!
  !! \since 2.1
  interface kim_get_configuration_file_environment_variable
    module procedure kim_collections_get_configuration_file_environment_variable
  end interface kim_get_configuration_file_environment_variable

  !> \brief \copybrief KIM::Collections::GetConfigurationFileName
  !!
  !! \sa KIM::Collections::GetConfigurationFileName,
  !! KIM_Collections_GetConfigurationFileName
  !!
  !! \since 2.1
  interface kim_get_configuration_file_name
    module procedure kim_collections_get_configuration_file_name
  end interface kim_get_configuration_file_name

  !> \brief \copybrief KIM::Collections::CacheListOfDirectoryNames
  !!
  !! \sa KIM::Collections::CacheListOfDirectoryNames,
  !! KIM_Collections_CacheListOfDirectoryNames
  !!
  !! \since 2.1
  interface kim_cache_list_of_directory_names
    module procedure kim_collections_cache_list_of_directory_names
  end interface kim_cache_list_of_directory_names

  !> \brief \copybrief KIM::Collections::GetDirectoryName
  !!
  !! \sa KIM::Collections::GetDirectoryName, KIM_Collections_GetDirectoryName
  !!
  !! \since 2.1
  interface kim_get_directory_name
    module procedure kim_collections_get_directory_name
  end interface kim_get_directory_name

  !> \brief \copybrief KIM::Collections::SetLogID
  !!
  !! \sa KIM::Collections::SetLogID, KIM_Collections_SetLogID
  !!
  !! \since 2.1
  interface kim_set_log_id
    module procedure kim_collections_set_log_id
  end interface kim_set_log_id

  !> \brief \copybrief KIM::Collections::PushLogVerbosity
  !!
  !! \sa KIM::Collections::PushLogVerbosity, KIM_Collections_PushLogVerbosity
  !!
  !! \since 2.1
  interface kim_push_log_verbosity
    module procedure kim_collections_push_log_verbosity
  end interface kim_push_log_verbosity

  !> \brief \copybrief KIM::Collections::PopLogVerbosity
  !!
  !! \sa KIM::Collections::, KIM_Collections_PopLogVerbosity
  !!
  !! \since 2.1
  interface kim_pop_log_verbosity
    module procedure kim_collections_pop_log_verbosity
  end interface kim_pop_log_verbosity

contains
  !> \brief Compares kim_collections_handle_type's for equality.
  !!
  !! \since 2.1
  logical recursive function kim_collections_handle_equal(lhs, rhs)
    implicit none
    type(kim_collections_handle_type), intent(in) :: lhs
    type(kim_collections_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_collections_handle_equal = .true.
    else
      kim_collections_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_collections_handle_equal

  !> \brief Compares kim_collections_handle_type's for inequality.
  !!
  !! \since 2.1
  logical recursive function kim_collections_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_collections_handle_type), intent(in) :: lhs
    type(kim_collections_handle_type), intent(in) :: rhs

    kim_collections_handle_not_equal = .not. (lhs == rhs)
  end function kim_collections_handle_not_equal

  !> \brief \copybrief KIM::Collections::Create
  !!
  !! \sa KIM::Collections::Create, KIM_Collections_Create
  !!
  !! \since 2.1
  recursive subroutine kim_collections_create(collections_handle, ierr)
    implicit none
    interface
      integer(c_int) recursive function create(collections) &
        bind(c, name="KIM_Collections_Create")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(out) :: collections
      end function create
    end interface
    type(kim_collections_handle_type), intent(out) :: collections_handle
    integer(c_int), intent(out) :: ierr

    type(c_ptr) :: pcollections

    ierr = create(pcollections)
    collections_handle%p = pcollections
  end subroutine kim_collections_create

  !> \brief \copybrief KIM::Collections::Destroy
  !!
  !! \sa KIM::Collections::Destroy, KIM_Collections_Destroy
  !!
  !! \since 2.1
  recursive subroutine kim_collections_destroy(collections_handle)
    implicit none
    interface
      recursive subroutine destroy(collections) &
        bind(c, name="KIM_Collections_Destroy")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(inout) :: collections
      end subroutine destroy
    end interface
    type(kim_collections_handle_type), intent(inout) :: collections_handle

    type(c_ptr) :: pcollections
    pcollections = collections_handle%p
    call destroy(pcollections)
    collections_handle%p = c_null_ptr
  end subroutine kim_collections_destroy

  !> \brief \copybrief KIM::Collections::GetItemType
  !!
  !! \sa KIM::Collections::GetItemType, KIM_Collections_GetItemType
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_item_type(collections_handle, &
                                                     item_name, item_type, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function get_item_type( &
        collections, item_name, item_type) &
        bind(c, name="KIM_Collections_GetItemType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        character(c_char), intent(in) :: item_name(*)
        type(kim_collection_item_type_type), intent(out) :: item_type
      end function get_item_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    character(len=*, kind=c_char), intent(in) :: item_name
    type(kim_collection_item_type_type), intent(out) :: item_type
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_type(collections, trim(item_name)//c_null_char, item_type)
  end subroutine kim_collections_get_item_type

  !> \brief \copybrief KIM::Collections::GetItemLibraryFileNameAndCollection
  !!
  !! \sa KIM::Collections::GetItemLibraryFileNameAndCollection,
  !! KIM_Collections_GetItemLibraryFileNameAndCollection
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_get_item_library_file_name_and_collection( &
    collections_handle, item_type, item_name, file_name, collection, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_collection_module, only: kim_collection_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function &
        get_item_library_file_name_and_collection( &
        collections, item_type, item_name, file_name, collection) &
        bind(c, name="KIM_Collections_GetItemLibraryFileNameAndCollection")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_module, only: kim_collection_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_item_type_type), intent(in), value :: item_type
        character(c_char), intent(in) :: item_name(*)
        type(c_ptr), intent(out) :: file_name
        type(kim_collection_type), intent(out) :: collection
      end function get_item_library_file_name_and_collection
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_item_type_type), intent(in) :: item_type
    character(len=*, kind=c_char), intent(in) :: item_name
    character(len=*, kind=c_char), intent(out) :: file_name
    type(kim_collection_type), intent(out) :: collection
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) :: pfile_name

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_library_file_name_and_collection( &
           collections, &
           item_type, &
           trim(item_name)//c_null_char, &
           pfile_name, &
           collection)
    call kim_convert_c_char_ptr_to_string(pfile_name, file_name)
  end subroutine kim_collections_get_item_library_file_name_and_collection

  !> \brief \copybrief KIM::Collections::CacheListOfItemMetadataFiles
  !!
  !! \sa KIM::Collections::CacheListOfItemMetadataFiles,
  !! KIM_Collections_CacheListOfItemMetadataFiles
  !!
  !! \since 2.1
  recursive subroutine kim_collections_cache_list_of_item_metadata_files( &
    collections_handle, item_type, item_name, extent, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function cache_list_of_item_metadata_files( &
        collections, item_type, item_name, extent) &
        bind(c, name="KIM_Collections_CacheListOfItemMetadataFiles")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_item_type_type), intent(in), value :: item_type
        character(c_char), intent(in) :: item_name(*)
        integer(c_int), intent(out) :: extent
      end function cache_list_of_item_metadata_files
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_item_type_type), intent(in) :: item_type
    character(len=*, kind=c_char), intent(in) :: item_name
    integer(c_int), intent(out) :: extent
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = cache_list_of_item_metadata_files(collections, item_type, &
                                             trim(item_name)//c_null_char, &
                                             extent)
  end subroutine kim_collections_cache_list_of_item_metadata_files

  !> \brief Get item metadata file length and determine if the file is
  !! available as a string.
  !!
  !! \sa KIM::Collections::GetItemMetadataFile,
  !! KIM_Collections_GetItemMetadataFile
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_item_metadata_file_length( &
    collections_handle, index, file_length, available_as_string, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    implicit none
    interface
      integer(c_int) recursive function get_item_metadata_file( &
        collections, index, file_name, file_length, file_raw_data, &
        available_as_string, file_string) &
        bind(c, name="KIM_Collections_GetItemMetadataFile_fortran")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: file_name
        integer(c_long), intent(out) :: file_length
        type(c_ptr), intent(out) :: file_raw_data
        integer(c_int), intent(out) :: available_as_string
        type(c_ptr), intent(out) :: file_string
      end function get_item_metadata_file
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    integer(c_long), intent(out) :: file_length
    integer(c_int), intent(out) :: available_as_string
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pfile_name, pfile_raw_data, pfile_string

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_metadata_file(collections, &
                                  index - 1, &
                                  pfile_name, &
                                  file_length, &
                                  pfile_raw_data, &
                                  available_as_string, &
                                  pfile_string)
  end subroutine kim_collections_get_item_metadata_file_length

  !> \brief Get the item's metadata file values.
  !!
  !! \sa KIM::Collections::GetItemMetadataFile,
  !! KIM_Collections_GetItemMetadataFile
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_item_metadata_file_values( &
    collections_handle, index, file_name, file_raw_data, file_string, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_item_metadata_file( &
        collections, index, file_name, file_length, file_raw_data, &
        available_as_string, file_string) &
        bind(c, name="KIM_Collections_GetItemMetadataFile_fortran")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: file_name
        integer(c_long), intent(out) :: file_length
        type(c_ptr), intent(out) :: file_raw_data
        integer(c_int), intent(out) :: available_as_string
        type(c_ptr), intent(out) :: file_string
      end function get_item_metadata_file
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: file_name
    integer(c_signed_char), intent(out) :: file_raw_data(:)
    character(len=*, kind=c_char), intent(out) :: file_string
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    integer(c_long) file_length
    integer(c_int) available_as_string
    type(c_ptr) pfile_name, pfile_raw_data, pfile_string
    integer(c_signed_char), pointer :: file_raw_data_fpointer(:)

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_metadata_file(collections, &
                                  index - 1, &
                                  pfile_name, &
                                  file_length, &
                                  pfile_raw_data, &
                                  available_as_string, &
                                  pfile_string)
    if (ierr == 0) then
      if (size(file_raw_data) < file_length) then
        ierr = 1
        return
      end if
      if (available_as_string == 1) then
        if (len(file_string) < file_length) then
          ierr = 1
          return
        end if
      end if

      call kim_convert_c_char_ptr_to_string(pfile_name, file_name)
      if (c_associated(pfile_raw_data)) then
        call c_f_pointer(pfile_raw_data, file_raw_data_fpointer, [file_length])
      else
        nullify (file_raw_data_fpointer)
      end if
      file_raw_data = file_raw_data_fpointer(1:file_length)

      if (available_as_string == 1) then
        call kim_convert_c_char_ptr_to_string(pfile_string, file_string)
      end if
    end if
  end subroutine kim_collections_get_item_metadata_file_values

  !> \brief \copybrief KIM::Collections::CacheListOfItemNamesByType
  !!
  !! \sa KIM::Collections::CacheListOfItemNamesByType,
  !! KIM_Collections_CacheListOfItemNamesByType
  !!
  !! \since 2.1
  recursive subroutine kim_collections_cache_list_of_item_names_by_type( &
    collections_handle, item_type, extent, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function cache_list_of_item_names_by_type( &
        collections, item_type, extent) &
        bind(c, name="KIM_Collections_CacheListOfItemNamesByType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_item_type_type), intent(in), value :: item_type
        integer(c_int), intent(out) :: extent
      end function cache_list_of_item_names_by_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_item_type_type), intent(in) :: item_type
    integer(c_int), intent(out) :: extent
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = cache_list_of_item_names_by_type(collections, item_type, extent)
  end subroutine kim_collections_cache_list_of_item_names_by_type

  !> \brief \copybrief KIM::Collections::GetItemNameByType
  !!
  !! \sa KIM::Collections::GetItemNameByType, KIM_Collections_GetItemNameByType
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_item_name_by_type( &
    collections_handle, index, item_name, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_item_name_by_type( &
        collections, index, item_name) &
        bind(c, name="KIM_Collections_GetItemNameByType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: item_name
      end function get_item_name_by_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: item_name
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pitem_name

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_name_by_type(collections, index - 1, pitem_name)
    call kim_convert_c_char_ptr_to_string(pitem_name, item_name)
  end subroutine kim_collections_get_item_name_by_type

  !> \brief \copybrief KIM::Collections::CacheListOfItemNamesByCollectionAndType
  !!
  !! \sa KIM::Collections::CacheListOfItemNamesByCollectionAndType,
  !! KIM_Collections_CacheListOfItemNamesByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_cache_list_of_item_names_by_collection_and_type( &
    collections_handle, collection, item_type, extent, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_module, only: kim_collection_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function &
        cache_list_of_item_names_by_collection_and_type( &
        collections, collection, item_type, extent) &
        bind(c, name="KIM_Collections_CacheListOfItemNamesByCollectionAndType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_module, only: kim_collection_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_type), intent(in), value :: collection
        type(kim_collection_item_type_type), intent(in), value :: item_type
        integer(c_int), intent(out) :: extent
      end function cache_list_of_item_names_by_collection_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_type), intent(in) :: collection
    type(kim_collection_item_type_type), intent(in) :: item_type
    integer(c_int), intent(out) :: extent
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = cache_list_of_item_names_by_collection_and_type(collections, &
                                                           collection, &
                                                           item_type, &
                                                           extent)
  end subroutine kim_collections_cache_list_of_item_names_by_collection_and_type

  !> \brief \copybrief KIM::Collections::GetItemNameByCollectionAndType
  !!
  !! \sa KIM::Collections::GetItemNameByCollectionAndType,
  !! KIM_Collections_GetItemNameByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_item_name_by_collection_and_type( &
    collections_handle, index, item_name, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_item_name_by_collection_and_type( &
        collections, index, item_name) &
        bind(c, name="KIM_Collections_GetItemNameByCollectionAndType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: item_name
      end function get_item_name_by_collection_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: item_name
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pitem_name

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_name_by_collection_and_type(collections, index - 1, &
                                                pitem_name)
    call kim_convert_c_char_ptr_to_string(pitem_name, item_name)
  end subroutine kim_collections_get_item_name_by_collection_and_type

  !> \brief \copybrief <!--
  !! -->KIM::Collections::GetItemLibraryFileNameByCollectionAndType
  !!
  !! \sa KIM::Collections::GetItemLibraryFileNameByCollectionAndType,
  !! KIM_Collections_GetItemLibraryFileNameByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_get_item_library_file_name_by_coll_and_type( &
    collections_handle, collection, item_type, item_name, file_name, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_collection_module, only: kim_collection_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function &
        get_item_library_file_name_by_coll_and_type( &
        collections, collection, item_type, item_name, file_name) &
        bind(c, &
             name="KIM_Collections_GetItemLibraryFileNameByCollectionAndType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_module, only: kim_collection_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_type), intent(in), value :: collection
        type(kim_collection_item_type_type), intent(in), value :: item_type
        character(c_char), intent(in) :: item_name(*)
        type(c_ptr), intent(out) :: file_name
      end function get_item_library_file_name_by_coll_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_type), intent(in) :: collection
    type(kim_collection_item_type_type), intent(in) :: item_type
    character(len=*, kind=c_char), intent(in) :: item_name
    character(len=*, kind=c_char), intent(out) :: file_name
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pfile_name

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_library_file_name_by_coll_and_type( &
           collections, &
           collection, &
           item_type, &
           trim(item_name)//c_null_char, &
           pfile_name)
    call kim_convert_c_char_ptr_to_string(pfile_name, file_name)
  end subroutine kim_collections_get_item_library_file_name_by_coll_and_type

  !> \brief \copybrief <!--
  !! -->KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType
  !!
  !! \sa KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType,
  !! KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine &
    kim_colls_cache_list_of_item_metadata_files_by_coll_and_type( &
    collections_handle, collection, item_type, item_name, extent, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_module, only: kim_collection_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function &
        cache_list_of_item_metadata_files_by_coll_and_type( &
        collections, collection, item_type, item_name, extent) &
        bind(c, &
             name= &
             "KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_module, only: kim_collection_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_type), intent(in), value :: collection
        type(kim_collection_item_type_type), intent(in), value :: item_type
        character(c_char), intent(in) :: item_name(*)
        integer(c_int), intent(out) :: extent
      end function cache_list_of_item_metadata_files_by_coll_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_type), intent(in) :: collection
    type(kim_collection_item_type_type), intent(in) :: item_type
    character(len=*, kind=c_char), intent(in) :: item_name
    integer(c_int), intent(out) :: extent
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = cache_list_of_item_metadata_files_by_coll_and_type( &
           collections, &
           collection, &
           item_type, &
           trim(item_name)//c_null_char, &
           extent)
  end subroutine kim_colls_cache_list_of_item_metadata_files_by_coll_and_type

  !> \brief \copybrief KIM::Collections::GetItemMetadataFileByCollectionAndType
  !!
  !! \sa KIM::Collections::GetItemMetadataFileByCollectionAndType,
  !! KIM_Collections_GetItemMetadataFileByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_get_item_metadata_file_length_by_coll_and_type( &
    collections_handle, index, file_length, available_as_string, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    implicit none
    interface
      integer(c_int) recursive function &
        get_item_metadata_file_by_coll_and_type( &
        collections, index, &
        file_name, &
        file_length, &
        file_raw_data, &
        available_as_string, &
        file_string) &
        bind(c, &
             name= &
             "KIM_Collections_GetItemMetadataFileByCollectionAndType_fortran")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: file_name
        integer(c_long), intent(out) :: file_length
        type(c_ptr), intent(out) :: file_raw_data
        integer(c_int), intent(out) :: available_as_string
        type(c_ptr), intent(out) :: file_string
      end function get_item_metadata_file_by_coll_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in), value :: index
    integer(c_long), intent(out) :: file_length
    integer(c_int), intent(out) :: available_as_string
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pfile_name, pfile_raw_data, pfile_string

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_metadata_file_by_coll_and_type(collections, &
                                                   index - 1, &
                                                   pfile_name, &
                                                   file_length, &
                                                   pfile_raw_data, &
                                                   available_as_string, &
                                                   pfile_string)
  end subroutine kim_collections_get_item_metadata_file_length_by_coll_and_type

  !> \brief Get the item's metadata file values.
  !!
  !! \sa KIM::Collections::GetItemMetadataFileByCollectionAndType,
  !! KIM_Collections_GetItemMetadataFileByCollectionAndType
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_get_item_metadata_file_values_by_coll_and_type( &
    collections_handle, index, file_name, file_raw_data, file_string, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function &
        get_item_metadata_file_by_coll_and_type(collections, &
                                                index, &
                                                file_name, &
                                                file_length, &
                                                file_raw_data, &
                                                available_as_string, &
                                                file_string) &
        bind(c, &
             name= &
             "KIM_Collections_GetItemMetadataFileByCollectionAndType_fortran")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: file_name
        integer(c_long), intent(out) :: file_length
        type(c_ptr), intent(out) :: file_raw_data
        integer(c_int), intent(out) :: available_as_string
        type(c_ptr), intent(out) :: file_string
      end function get_item_metadata_file_by_coll_and_type
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: file_name
    integer(c_signed_char), intent(out) :: file_raw_data(:)
    character(len=*, kind=c_char), intent(out) :: file_string
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    integer(c_long) file_length
    integer(c_int) available_as_string
    type(c_ptr) pfile_name, pfile_raw_data, pfile_string
    integer(c_signed_char), pointer :: file_raw_data_fpointer(:)

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_item_metadata_file_by_coll_and_type(collections, &
                                                   index - 1, &
                                                   pfile_name, &
                                                   file_length, &
                                                   pfile_raw_data, &
                                                   available_as_string, &
                                                   pfile_string)
    if (ierr == 0) then
      if (size(file_raw_data) < file_length) then
        ierr = 1
        return
      end if
      if (available_as_string == 1) then
        if (len(file_string) < file_length) then
          ierr = 1
          return
        end if
      end if

      call kim_convert_c_char_ptr_to_string(pfile_name, file_name)
      if (c_associated(pfile_raw_data)) then
        call c_f_pointer(pfile_raw_data, file_raw_data_fpointer, [file_length])
      else
        nullify (file_raw_data_fpointer)
      end if
      file_raw_data = file_raw_data_fpointer(1:file_length)

      if (available_as_string == 1) then
        call kim_convert_c_char_ptr_to_string(pfile_string, file_string)
      end if
    end if
  end subroutine kim_collections_get_item_metadata_file_values_by_coll_and_type

  !> \brief \copybrief KIM::Collections::GetProjectNameAndSemVer
  !!
  !! \sa KIM::Collections::GetProjectNameAndSemVer,
  !! KIM_Collections_GetProjectNameAndSemVer
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_project_name_and_sem_ver( &
    collections_handle, project_name, sem_ver)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_project_name_and_sem_ver(collections, &
                                                        project_name, sem_ver) &
        bind(c, name="KIM_Collections_GetProjectNameAndSemVer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(c_ptr), intent(out) :: project_name
        type(c_ptr), intent(out) :: sem_ver
      end subroutine get_project_name_and_sem_ver
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    character(len=*, kind=c_char), intent(out) :: project_name
    character(len=*, kind=c_char), intent(out) :: sem_ver
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pproject_name, psem_ver

    call c_f_pointer(collections_handle%p, collections)
    call get_project_name_and_sem_ver(collections, pproject_name, psem_ver)
    call kim_convert_c_char_ptr_to_string(pproject_name, project_name)
    call kim_convert_c_char_ptr_to_string(psem_ver, sem_ver)
  end subroutine kim_collections_get_project_name_and_sem_ver

  !> \brief \copybrief KIM::Collections::GetEnvironmentVariableName
  !!
  !! \sa KIM::Collections::GetEnvironmentVariableName,
  !! KIM_Collections_GetEnvironmentVariableName
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_environment_variable_name( &
    collections_handle, item_type, name, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function get_environment_variable_name( &
        collections, item_type, name) &
        bind(c, name="KIM_Collections_GetEnvironmentVariableName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_item_type_type), intent(in), value :: item_type
        type(c_ptr), intent(out) :: name
      end function get_environment_variable_name
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_item_type_type), intent(in) :: item_type
    character(len=*, kind=c_char), intent(out) :: name
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pname

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_environment_variable_name(collections, item_type, pname)
    call kim_convert_c_char_ptr_to_string(pname, name)
  end subroutine kim_collections_get_environment_variable_name

  !> \brief \copybrief KIM::Collections::GetConfigurationFileEnvironmentVariable
  !!
  !! \sa KIM::Collections::GetConfigurationFileEnvironmentVariable,
  !! KIM_Collections_GetConfigurationFileEnvironmentVariable
  !!
  !! \since 2.1
  recursive subroutine &
    kim_collections_get_configuration_file_environment_variable( &
    collections_handle, name, value)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_configuration_file_environment_variable( &
        collections, name, value) &
        bind(c, name="KIM_Collections_GetConfigurationFileEnvironmentVariable")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(c_ptr), intent(out) :: name
        type(c_ptr), intent(out) :: value
      end subroutine get_configuration_file_environment_variable
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    character(len=*, kind=c_char), intent(out) :: name
    character(len=*, kind=c_char), intent(out) :: value
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pname, pvalue

    call c_f_pointer(collections_handle%p, collections)
    call get_configuration_file_environment_variable(collections, pname, pvalue)
    call kim_convert_c_char_ptr_to_string(pname, name)
    call kim_convert_c_char_ptr_to_string(pvalue, value)
  end subroutine kim_collections_get_configuration_file_environment_variable

  !> \brief \copybrief KIM::Collections::GetConfigurationFileName
  !!
  !! \sa KIM::Collections::GetConfigurationFileName,
  !! KIM_Collections_GetConfigurationFileName
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_configuration_file_name( &
    collections_handle, file_name)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_configuration_file_name(collections, file_name) &
        bind(c, name="KIM_Collections_GetConfigurationFileName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(c_ptr), intent(out) :: file_name
      end subroutine get_configuration_file_name
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    character(len=*, kind=c_char), intent(out) :: file_name
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pfile_name

    call c_f_pointer(collections_handle%p, collections)
    call get_configuration_file_name(collections, pfile_name)
    call kim_convert_c_char_ptr_to_string(pfile_name, file_name)
  end subroutine kim_collections_get_configuration_file_name

  !> \brief \copybrief KIM::Collections::CacheListOfDirectoryNames
  !!
  !! \sa KIM::Collections::CacheListOfDirectoryNames,
  !! KIM_Collections_CacheListOfDirectoryNames
  !!
  !! \since 2.1
  recursive subroutine kim_collections_cache_list_of_directory_names( &
    collections_handle, collection, item_type, extent, ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_collection_module, only: kim_collection_type
    use kim_collection_item_type_module, only: kim_collection_item_type_type
    implicit none
    interface
      integer(c_int) recursive function cache_list_of_directory_names( &
        collections, collection, item_type, extent) &
        bind(c, name="KIM_Collections_CacheListOfDirectoryNames")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        use kim_collection_module, only: kim_collection_type
        use kim_collection_item_type_module, only: &
          kim_collection_item_type_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_collection_type), intent(in), value :: collection
        type(kim_collection_item_type_type), intent(in), value :: item_type
        integer(c_int), intent(out) :: extent
      end function cache_list_of_directory_names
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collection_type), intent(in) :: collection
    type(kim_collection_item_type_type), intent(in) :: item_type
    integer(c_int), intent(out) :: extent
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    ierr = cache_list_of_directory_names(collections, collection, item_type, &
                                         extent)
  end subroutine kim_collections_cache_list_of_directory_names

  !> \brief \copybrief KIM::Collections::GetDirectoryName
  !!
  !! \sa KIM::Collections::GetDirectoryName, KIM_Collections_GetDirectoryName
  !!
  !! \since 2.1
  recursive subroutine kim_collections_get_directory_name(collections_handle, &
                                                          index, &
                                                          directory_name, &
                                                          ierr)
    use kim_interoperable_types_module, only: kim_collections_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_directory_name(collections, index, &
                                                           directory_name) &
        bind(c, name="KIM_Collections_GetDirectoryName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: directory_name
      end function get_directory_name
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: directory_name
    integer(c_int), intent(out) :: ierr
    type(kim_collections_type), pointer :: collections

    type(c_ptr) pdirectory_name

    call c_f_pointer(collections_handle%p, collections)
    ierr = get_directory_name(collections, index - 1, pdirectory_name)
    call kim_convert_c_char_ptr_to_string(pdirectory_name, directory_name)
  end subroutine kim_collections_get_directory_name

  !> \brief \copybrief KIM::Collections::SetLogID
  !!
  !! \sa KIM::Collections::SetLogID, KIM_Collections_SetLogID
  !!
  !! \since 2.1
  recursive subroutine kim_collections_set_log_id(collections_handle, log_id)
    use kim_interoperable_types_module, only: kim_collections_type
    implicit none
    interface
      recursive subroutine set_log_id(collections, log_id) &
        bind(c, name="KIM_Collections_SetLogID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        character(c_char), intent(in) :: log_id(*)
      end subroutine set_log_id
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    character(len=*, kind=c_char), intent(in) :: log_id
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    call set_log_id(collections, trim(log_id)//c_null_char)
  end subroutine kim_collections_set_log_id

  !> \brief \copybrief KIM::Collections::PushLogVerbosity
  !!
  !! \sa KIM::Collections::PushLogVerbosity, KIM_Collections_PushLogVerbosity
  !!
  !! \since 2.1
  recursive subroutine kim_collections_push_log_verbosity(collections_handle, &
                                                          log_verbosity)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_collections_type
    implicit none
    interface
      recursive subroutine push_log_verbosity(collections, log_verbosity) &
        bind(c, name="KIM_Collections_PushLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_log_verbosity
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    call push_log_verbosity(collections, log_verbosity)
  end subroutine kim_collections_push_log_verbosity

  !> \brief \copybrief KIM::Collections::PopLogVerbosity
  !!
  !! \sa KIM::Collections::, KIM_Collections_PopLogVerbosity
  !!
  !! \since 2.1
  recursive subroutine kim_collections_pop_log_verbosity(collections_handle)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_collections_type
    implicit none
    interface
      recursive subroutine pop_log_verbosity(collections) &
        bind(c, name="KIM_Collections_PopLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_collections_type
        implicit none
        type(kim_collections_type), intent(in) :: collections
      end subroutine pop_log_verbosity
    end interface
    type(kim_collections_handle_type), intent(in) :: collections_handle
    type(kim_collections_type), pointer :: collections

    call c_f_pointer(collections_handle%p, collections)
    call pop_log_verbosity(collections)
  end subroutine kim_collections_pop_log_verbosity
end module kim_collections_module

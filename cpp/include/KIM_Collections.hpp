//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_COLLECTIONS_HPP_
#define KIM_COLLECTIONS_HPP_

#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class Collection;
class CollectionItemType;
class CollectionsImplementation;

/// \brief Provides the interface to the %KIM API %Collections and is meant to
/// be used by simulators.
///
/// The %KIM API defines interfaces to various types of "items"
/// (KIM::CollectionItemType) and these items are organized and stored in
/// various "collections" (KIM::Collection) on the user's system.  Items are
/// generally installed to or removed from the collections by using the \c
/// kim-api-collections-management utility, and the %KIM API/PMI and %KIM
/// API/SMI find items within the collections using the KIM::Collections
/// interface.  Typically, the collections are associated with different levels
/// of user permissions on the system.  The KIM::COLLECTION::system collection
/// is available to all users on the system and is managed by the system
/// administrator and cannot be changed by users without administrator
/// privileges.  The KIM::COLLECTION::user collection is specific to each user
/// and the use has full privileges to manage the collection.  The
/// KIM::COLLECTION::environmentVariable and the
/// KIM::COLLECTION::currentWorkingDirectory collections allow users more
/// dynamic flexibility to manage and store items for special purposes.
///
/// Each collection consists of a set of items organized by their item type
/// (KIM::CollectionItemType).  There are currently three item types:
/// KIM::COLLECTION_ITEM_TYPE::portableModel represents %KIM Portable Models
/// that can be used with any simulator that supports the %KIM API/PMI;
/// KIM::COLLECTION_ITEM_TYPE::simulatorModel represents the %KIM Simulator
/// Models that can be used with a specific simulator that supports the %KIM
/// API/SMI; and KIM::COLLECTION_ITEM_TYPE::modelDriver represents %KIM %Model
/// Drivers that are libraries of code that can be used by multiple Portable
/// Models to help reduce code duplication.  The KIM::Collections interface
/// provides programatic access to the contents and system settings for the
/// %KIM API collections and items stored within them.  The contents and
/// settings of the collections can change during the lifetime of a
/// KIM::Collections object (due to installation or removal of items by other
/// processes on the machine and/or changes to environment variables or the
/// configuration file).  Therefore, when lists of information about the
/// collections are requested (via a "CacheListOf...()" routine), the
/// KIM::Collections interface first creates a cache of the list and then
/// provides access to the cached list via a getter ("Get...()") routine.  The
/// cached list is only updated when the simulator makes another request for
/// the list (via a "CacheListOf...()" routine).
///
/// Items in the %KIM API collections are generally referred to by name.  An
/// item name is required to be a valid C-identifier (no other restrictions are
/// imposed by the %KIM API).  Items in different collections can have the same
/// name and items of different type can have the same name.  However, this
/// should generally be avoided.  Typically, an item is referred to by
/// specifying its type and name.  In such cases the %KIM API will search
/// through the %KIM API collections in a specific order <!------------------
/// -->(\anchor collection_search_order first the
/// KIM::COLLECTION::currentWorkingDirectory, then
/// KIM::COLLECTION::environmentVariable, then KIM::COLLECTION::user, and
/// finally KIM::COLLECTION::system) and return the first occurrence of an item
/// with the requested type and name that is found.  In some cases only the
/// name of the desired item is known, and Collections::GetItemType must be
/// used first to determine the item's type.  The Collections::GetItemType
/// routine will search through each collection (in the order described just
/// \ref collection_search_order "above") and through each item type within
/// each collection in a specific order <!------------------------------------
/// --> (\anchor collection_item_type_search_order first the
/// KIM::COLLECTION_ITEM_TYPE::portableModel type, then the
/// KIM::COLLECTION_ITEM_TYPE::simulatorModel type, and finally the
/// KIM::COLLECTION_ITEM_TYPE::modelDriver type) and return the first
/// occurrence of an item with the requested name that is found.
///
/// \sa KIM_Collections, kim_collections_module::kim_collections_handle_type
///
/// \since 2.1
class Collections
{
 public:
  /// \brief Create a new %KIM API Collections object.
  ///
  /// \param[out] collections Pointer to the newly created Collections object.
  ///
  /// \return \c true if the %KIM API is unable to allocate a new log object.
  /// \return \c false otherwise.
  ///
  /// \post \c `collections == NULL` if an error occurs.
  ///
  /// \sa KIM_Collections_Create, kim_collections_module::kim_collections_create
  ///
  /// \since 2.1
  static int Create(Collections ** const collections);

  /// \brief Destroy a previously Collections::Create'd object.
  ///
  /// \param[inout] collections Pointer to the Collections object.
  ///
  /// \pre \c collections points to a previously created %KIM API Collections
  ///      object.
  ///
  /// \post `collections == NULL`.
  ///
  /// \sa KIM_Collections_Destroy,
  /// kim_collections_module::kim_collections_destroy
  ///
  /// \since 2.1
  static void Destroy(Collections ** const collections);

  /// \brief Get the KIM::CollectionItemType of the item in the %KIM API
  /// collections with a specific name.
  ///
  /// Searches for an item with the given name in the %KIM API collections and
  /// item types (using the standard order for <!-----------------------------
  /// --> \ref collection_search_order "collections" and <!-------------------
  /// --> \ref collection_item_type_search_order "item types").
  ///
  /// \param[in]  itemName The item name to be found.
  /// \param[out] itemType The KIM::CollectionItemType of the item.
  ///
  /// \return \c true if an item with the specificed name cannot be found.
  /// \return \c false otherwise.
  ///
  /// \post \c itemType is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemType,
  /// kim_collections_module::kim_get_item_type
  ///
  /// \since 2.1
  int GetItemType(std::string const & itemName,
                  CollectionItemType * const itemType) const;

  /// \brief Get the item's library file name and its KIM::Collection.
  ///
  /// \param[in]  itemType The KIM::CollectionItemType of the item.
  /// \param[in]  itemName The name of the item.
  /// \param[out] fileName The absolute file and path name of the item's
  ///             library.
  /// \param[out] collection The KIM::Collection in which the item was found.
  ///
  /// \return \c true if \c itemType is unknown.
  /// \return \c true if a item with the specified type and name cannot be
  ///         found.
  /// \return \c false otherwise.
  ///
  /// \pre \c fileName and \c collection may be \c NULL if the corresponding
  ///      value is not needed.
  ///
  /// \post \c fileName and \c collection are unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemLibraryFileNameAndCollection,
  /// kim_collections_module::kim_get_item_library_file_name_and_collection
  ///
  /// \since 2.1
  int GetItemLibraryFileNameAndCollection(CollectionItemType const itemType,
                                          std::string const & itemName,
                                          std::string const ** const fileName,
                                          Collection * const collection) const;

  /// \brief Cache a list of an item's metadata files.
  ///
  /// \param[in]  itemType The KIM::CollectionItemType of the item.
  /// \param[in]  itemName The name of the item.
  /// \param[out] extent The number of metadata files in the list.
  ///
  /// \return \c true if \c itemType is unknown.
  /// \return \c true if the list is not successfully cached for some reason.
  /// \return \c false otherwise.
  ///
  /// \post `extent == 0` and the cached list is empty if an error occurs.
  ///
  /// \sa KIM_Collections_CacheListOfItemMetadataFiles,
  /// kim_collections_module::kim_cache_list_of_item_metadata_files
  ///
  /// \since 2.1
  int CacheListOfItemMetadataFiles(CollectionItemType const itemType,
                                   std::string const & itemName,
                                   int * const extent);

  /// \brief Get the name and content of one of an item's metadata files.
  ///
  /// Provide access to the specified metadata file's name and raw data.  If
  /// there are no embedded NULL characters in the raw data, the file contents
  /// are also provided as a string, for convenience.
  ///
  /// \param[in]  index Zero-based index for the metadata file of interest.
  /// \param[out] fileName The basename (file name without path) of the
  ///             metadata file.
  /// \param[out] fileLength The length of the metadata file.
  /// \param[out] fileRawData The raw metadata file content, as a contiguous
  ///             block of memory of length \c fileLength.
  /// \param[out] availableAsString An integer that is set to \c true if the
  ///             metadata file has no embedded \c NULL characters, and set to
  ///             \c false otherwise.
  /// \param[out] fileString The contents of the metadata file as a string, if
  ///             `availableAsString == true`, \c NULL otherwise.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::CacheListOfItemMetadataFiles or the KIM::Collections
  /// object is Collections::Destroy'd.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \pre Collections::CacheListOfItemMetadataFiles must have been
  ///      successfully executed before Collections::GetItemMetadataFile is
  ///      called.
  ///
  /// \pre \c fileName, \c fileLength, \c fileRawData, \c availableAsString,
  ///      and \c fileString may be \c NULL if the corresponding value is not
  ///      needed.
  ///
  /// \post \c fileName, \c fileLength, \c fileRawData, \c availableAsString,
  ///       \c fileString are unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemMetadataFile,
  /// kim_collections_module::kim_get_item_metadata_file_length,
  /// kim_collections_module::kim_get_item_metadata_file_values
  ///
  /// \since 2.1
  int GetItemMetadataFile(int const index,
                          std::string const ** const fileName,
                          unsigned int * const fileLength,
                          unsigned char const ** const fileRawData,
                          int * const availableAsString,
                          std::string const ** const fileString) const;

  /// \brief Cache a list of all item names of a specific type in the %KIM API
  /// collections.
  ///
  /// \param[in]  itemType The KIM::CollectionItemType of the items.
  /// \param[out] extent The number of item names in the list.
  ///
  /// \return \c true if \c itemType is unknown.
  /// \return \c false otherwise.
  ///
  /// \post `extent == 0` and the cached list is empty if an error occurs.
  ///
  /// \sa KIM_Collections_CacheListOfItemNamesByType,
  /// kim_collections_module::kim_cache_list_of_item_names_by_type
  ///
  /// \since 2.1
  int CacheListOfItemNamesByType(CollectionItemType const itemType,
                                 int * const extent);

  /// \brief Get the name of an item from the cached list.
  ///
  /// \param[in]  index Zero-based index for the item name of interest.
  /// \param[out] itemName The item's name.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::CacheListOfItemNamesByType or the KIM::Collections
  /// object is Collections::Destroy'd.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \pre Collections::CacheListOfItemNamesByType must have been successfully
  ///      executed before Collections::GetItemNameByType is called.
  ///
  /// \post \c itemName is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemNameByType,
  /// kim_collections_module::kim_get_item_name_by_type
  ///
  /// \since 2.1
  int GetItemNameByType(int const index,
                        std::string const ** const itemName) const;

  /// \brief Cache a list of all item names of a specific type in a specific
  /// collection.
  ///
  /// \param[in]  collection The KIM::Collection of the items.
  /// \param[in]  itemType The KIM::CollectionItemType of the items.
  /// \param[out] extent The number of item names in the list.
  ///
  /// \return \c true if \c collection or \c itemType are unknown.
  /// \return \c false otherwise.
  ///
  /// \post `extent == 0` and the cached list is empty if an error occurs.
  ///
  /// \sa KIM_Collections_CacheListOfItemNamesByCollectionAndType,
  /// kim_collections_module<!--
  /// -->::kim_cache_list_of_item_names_by_collection_and_type
  ///
  /// \since 2.1
  int CacheListOfItemNamesByCollectionAndType(Collection const collection,
                                              CollectionItemType const itemType,
                                              int * const extent);

  /// \brief Get the name of an item from the cached list.
  ///
  /// \param[in]  index Zero-based index for the item name of interest.
  /// \param[out] itemName The item's name.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::CacheListOfItemNamesByCollectionAndType or the
  /// KIM::Collections object is Collections::Destroy'd.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \pre Collections::CacheListOfItemNamesByCollectionAndType must have been
  ///      successfully executed before
  ///      Collections::GetItemNameByCollectionAndType is called.
  ///
  /// \post \c itemName is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemNameByCollectionAndType,
  /// kim_collections_module::kim_get_item_name_by_collection_and_type
  ///
  /// \since 2.1
  int GetItemNameByCollectionAndType(int const index,
                                     std::string const ** const itemName) const;

  /// \brief Get the item's library file name.
  ///
  /// \param[in]  collection The KIM::Collection of the item.
  /// \param[in]  itemType The KIM::CollectionItemType of the item.
  /// \param[in]  itemName The name of the item.
  /// \param[out] fileName The absolute file and path name of the item's
  ///             library.
  ///
  /// \return \c true if \c collection or \c itemType are unknown.
  /// \return \c true if a item with the specified type and name cannot be
  ///         found in the specified collection.
  /// \return \c false otherwise.
  ///
  /// \post \c fileName is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemLibraryFileNameByCollectionAndType,
  /// kim_collections_module::<!--
  /// -->kim_get_item_library_file_name_by_collection_and_type
  ///
  /// \since 2.1
  int GetItemLibraryFileNameByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      std::string const ** const fileName) const;

  /// \brief Cache a list of an item's metadata files.
  ///
  /// \param[in]  collection The KIM::Collection of the item.
  /// \param[in]  itemType The KIM::CollectionItemType of the item.
  /// \param[in]  itemName The name of the item.
  /// \param[out] extent The number of metadata files in the list.
  ///
  /// \return \c true if \c collection or \c itemType are unknown.
  /// \return \c true if the list is not successfully cached for some reason.
  /// \return \c false otherwise.
  ///
  /// \post `extent == 0` and the cached list is empty if an error occurs.
  ///
  /// \sa KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType,
  /// kim_collections_module::<!--
  /// -->kim_cache_list_of_item_metadata_files_by_collection_and_type
  ///
  /// \since 2.1
  int CacheListOfItemMetadataFilesByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      int * const extent);

  /// \brief Get the name and content of one of an item's metadata files.
  ///
  /// Provide access to the specified metadata file's name and raw data.  If
  /// there are no embedded NULL characters in the raw data, the file contents
  /// are also provided as a string, for convenience.
  ///
  /// \param[in]  index Zero-based index for the metadata file of interest.
  /// \param[out] fileName The basename (file name without path) of the
  ///             metadata file.
  /// \param[out] fileLength The length of the metadata file.
  /// \param[out] fileRawData The raw metadata file content, as a contiguous
  ///             block of memory of length \c fileLength.
  /// \param[out] availableAsString An integer that is set to \c true if the
  ///             metadata file has no embedded \c NULL characters, and set to
  ///             \c false otherwise.
  /// \param[out] fileString The contents of the metadata file as a string, if
  ///             `availableAsString == true`, \c NULL otherwise.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::CacheListOfItemMetadataFilesByCollectionAndType or
  /// the KIM::Collections object is Collections::Destroy'd.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \pre Collections::CacheListOfItemMetadataFilesByCollectionAndType must
  ///      have been successfully executed before
  ///      Collections::GetItemMetadataFileByCollectionAndType is called.
  ///
  /// \pre \c fileName, \c fileLength, \c fileRawData, \c availableAsString,
  ///      and \c fileString may be \c NULL if the corresponding value is not
  ///      needed.
  ///
  /// \post \c fileName, \c fileLength, \c fileRawData, \c availableAsString,
  ///       \c fileString are unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetItemMetadataFileByCollectionAndType,
  /// kim_collections_module::kim_get_item_metadata_file_length_<!--
  /// -->by_collection_and_type,
  /// kim_collections_module::kim_get_item_metadata_file_values_<!--
  /// -->by_collection_and_type
  ///
  /// \since 2.1
  int GetItemMetadataFileByCollectionAndType(
      int const index,
      std::string const ** const fileName,
      unsigned int * const fileLength,
      unsigned char const ** const fileRawData,
      int * const availableAsString,
      std::string const ** const fileString) const;


  /// \brief Get the %KIM API project name and full Semantic Version string.
  ///
  /// The %KIM API project name and version string are controlled by CMake
  /// settings during the configuration process.
  ///
  /// \note String pointers obtained from this routine are valid until the
  /// KIM::Collections object is Collections::Destroy'd.
  ///
  /// \param[out] projectName The project name.
  /// \param[out] semVer The complete Semantic Version string.
  ///
  /// \pre \c projectName and \c semVer may be \c NULL if the corresponding
  ///      value is not needed.
  ///
  /// \sa KIM_Collections_GetProjectNameAndSemVer,
  /// kim_collections_module::kim_get_project_name_and_sem_ver
  ///
  /// \since 2.1
  void GetProjectNameAndSemVer(std::string const ** const projectName,
                               std::string const ** const semVer) const;

  /// \brief Get the names of environment variables that store configuration
  /// settings for the KIM::COLLECTION::environmentVariable collection.
  ///
  /// \param[in]  itemType The KIM::CollectionItemType of interest.
  /// \param[out] name The environment variable name.
  ///
  /// \return \c true if \c itemType is unknown.
  /// \return \c false otherwise.
  ///
  /// \post \c name is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetEnvironmentVariableName,
  /// kim_collections_module::kim_get_environment_variable_name
  ///
  /// \since 2.1
  int GetEnvironmentVariableName(CollectionItemType const itemType,
                                 std::string const ** const name) const;

  /// \brief Get the name and value of the environment variable that stores the
  /// name of the %KIM API user configuration file.
  ///
  /// The %KIM API user configuration file contains configuration settings for
  /// the KIM::COLLECTION::user collection.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::GetConfigurationFileEnvironmentVariable or the
  /// KIM::Collections object is Collections::Destroy'd.
  ///
  /// \param[out] name The name of the environment variable.
  /// \param[out] value The current value of the environment variable.
  ///
  /// \pre \c name and \c value may be \c NULL if the corresponding value is
  ///      not needed.
  ///
  /// \sa KIM_Collections_GetConfigurationFileEnvironmentVariable,
  /// kim_collections_module::kim_get_configuration_file_environment_variable
  ///
  /// \since 2.1
  void GetConfigurationFileEnvironmentVariable(
      std::string const ** const name, std::string const ** const value) const;

  /// \brief Get the absolute file and path name of the %KIM API user
  /// configuration file.
  ///
  /// The %KIM API user configuration file contains configuration settings for
  /// the KIM::COLLECTION::user collection.
  ///
  /// \note String pointer obtained from this routine are valid until the next
  /// call to Collections::GetConfigurationFileName or the KIM::Collections
  /// object is Collections::Destroy'd.
  ///
  /// \param[out] fileName The absolute file and path name of the configuration
  ///             file.
  ///
  /// \sa KIM_Collections_GetConfigurationFileName,
  /// kim_collections_module::kim_get_configuration_file_name
  ///
  /// \since 2.1
  void GetConfigurationFileName(std::string const ** const fileName) const;

  /// \brief Cache a list of directory names where a specific %KIM API
  /// collection stores library files for a specific item type.
  ///
  /// \param[in]  collection The KIM::Collection of the items.
  /// \param[in]  itemType The KIM::CollectionItemType of the items.
  /// \param[out] extent The number of directory names in the list.
  ///
  /// \return \c true if \c collection or \c itemType are unknown.
  /// \return \c true if the list is not successfully cached for some reason.
  /// \return \c false otherwise.
  ///
  /// \post `extent == 0` and the cached list is empty if an error occurs.
  ///
  /// \sa KIM_Collections_CacheListOfDirectoryNames,
  /// kim_collections_module::kim_cache_list_of_directory_names
  ///
  /// \since 2.1
  int CacheListOfDirectoryNames(Collection const collection,
                                CollectionItemType const itemType,
                                int * const extent);

  /// \brief Get the name of a directory from the cached list.
  ///
  /// \param[in]  index Zero-based index for the directory name of interest.
  /// \param[out] directoryName The directory's name.
  ///
  /// \note String pointers obtained from this routine are valid until the next
  /// call to Collections::CacheListOfDirectoryNames or the KIM::Collections
  /// object is Collections::Destroy'd.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \pre Collections::CacheListOfDirectoryNames must have been successfully
  ///      executed before Collections::GetDirectoryName is called.
  ///
  /// \post \c directoryName is unchanged if an error occurs.
  ///
  /// \sa KIM_Collections_GetDirectoryName,
  /// kim_collections_module::kim_get_directory_name
  ///
  /// \since 2.1
  int GetDirectoryName(int const index,
                       std::string const ** const directoryName) const;

  /// \brief Set the identity of the Log object
  /// associated with the Collections object.
  ///
  /// \param[in] logID String identifying the Collections object's Log object.
  ///
  /// \sa KIM_Collections_SetLogID, kim_collections_module::kim_set_log_id
  ///
  /// \since 2.1
  void SetLogID(std::string const & logID);

  /// \brief Push a new LogVerbosity onto the Collections object's Log object
  /// verbosity stack.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_Collections_PushLogVerbosity,
  /// kim_collections_module::kim_push_log_verbosity
  ///
  /// \since 2.1
  void PushLogVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the Collections object's Log object
  /// verbosity stack.
  ///
  /// \sa KIM_Collections_PopLogVerbosity,
  /// kim_collections_module::kim_pop_log_verbosity
  ///
  /// \since 2.1
  void PopLogVerbosity();

 private:
  // do not allow copy constructor or operator=
  Collections(Collections const &);
  void operator=(Collections const &);

  Collections();
  ~Collections();

  CollectionsImplementation * pimpl;
};  // class Collections
}  // namespace KIM

#endif  // KIM_COLLECTIONS_HPP_

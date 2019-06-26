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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
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

/// \brief Provides the interface to the %KIM API Collections and is meant to
/// be used by simulators.
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
  /// \return \c false
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

  /// @@@ add docs
  ///
  /// \since 2.1
  int GetItemType(std::string const & itemName,
                  CollectionItemType * const itemType) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  int GetItemLibraryFileNameAndCollection(CollectionItemType const itemType,
                                          std::string const & itemName,
                                          std::string const ** const fileName,
                                          Collection * const collection) const;

  /// @@@ add docs
  ///
  /// pointers are valid until next call
  ///
  /// \since 2.1
  int CacheListOfItemMetadataFiles(CollectionItemType const itemType,
                                   std::string const & itemName,
                                   int * const extent);
  int GetItemMetadataFile(int const index,
                          std::string const ** const fileName,
                          unsigned int * const fileLength,
                          unsigned char const ** const fileRawData,
                          int * const availableAsString,
                          std::string const ** const fileString) const;

  /// @@@ add docs
  ///
  /// \since 4.1
  int CacheListOfItemNamesByType(CollectionItemType const itemType,
                                 int * const extent);
  int GetItemNameByType(int const index,
                        std::string const ** const itemName) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  int CacheListOfItemNamesByCollectionAndType(Collection const collection,
                                              CollectionItemType const itemType,
                                              int * const extent);
  int GetItemNameByCollectionAndType(int const index,
                                     std::string const ** const itemName) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  int GetItemLibraryFileNameByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      std::string const ** const fileName) const;

  /// @@@ add docs
  ///
  /// pointers are valid until next call
  ///
  /// \since 2.1
  int CacheListOfItemMetadataFilesByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      int * const extent);
  int GetItemMetadataFileByCollectionAndType(
      int const index,
      std::string const ** const fileName,
      unsigned int * const fileLength,
      unsigned char const ** const fileRawData,
      int * const availableAsString,
      std::string const ** const fileString) const;


  /// @@@ add docs
  ///
  /// \since 2.1
  void GetProjectNameAndSemVer(std::string const ** const projectName,
                               std::string const ** const semVer) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  int GetEnvironmentVariableName(CollectionItemType const itemType,
                                 std::string const ** const name) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  void GetConfigurationFileEnvironmentVariable(
      std::string const ** const name, std::string const ** const value) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  void GetConfigurationFileName(std::string const ** const fileName) const;

  /// @@@ add docs
  ///
  /// \since 2.1
  int CacheListOfDirectoryNames(Collection const collection,
                                CollectionItemType const itemType,
                                int * const extent);
  int GetDirectoryName(int const index,
                       std::string const ** const directoryName) const;

  /// \brief Set the identity of the Log object
  /// associated with the Collections object.
  ///
  /// \param[in] logID String identifying the Collections object's Log object.
  ///
  /// \sa KIM_Collections_SetLogID, kim_Collections_module::kim_set_log_id
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

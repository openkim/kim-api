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


#ifndef KIM_COLLECTIONS_IMPLEMENTATION_HPP_
#define KIM_COLLECTIONS_IMPLEMENTATION_HPP_

#include <sstream>
#include <string>
#include <vector>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif


namespace KIM
{
// Forward declaration
class Log;

class CollectionsImplementation
{
 public:
  static int
  Create(CollectionsImplementation ** const collectionsImplementation);
  static void
  Destroy(CollectionsImplementation ** const collectionsImplementation);

  int GetItemType(std::string const & itemName,
                  CollectionItemType * const itemType) const;

  int GetItemLibraryFileNameAndCollection(CollectionItemType const itemType,
                                          std::string const & itemName,
                                          std::string const ** const fileName,
                                          Collection * const collection) const;
  int CacheListOfItemMetadataFiles(CollectionItemType const itemType,
                                   std::string const & itemName,
                                   int * const extent);
  int GetItemMetadataFile(int const index,
                          std::string const ** const fileName,
                          unsigned int * const fileLength,
                          unsigned char const ** const fileRawData,
                          int * const availableAsString,
                          std::string const ** const fileString) const;
  int CacheListOfItemNamesByType(CollectionItemType const itemType,
                                 int * const extent);
  int GetItemNameByType(int const index,
                        std::string const ** const itemName) const;

  int CacheListOfItemNamesByCollectionAndType(Collection const collection,
                                              CollectionItemType const itemType,
                                              int * const extent);
  int GetItemNameByCollectionAndType(int const index,
                                     std::string const ** const itemName) const;

  int GetItemLibraryFileNameByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      std::string const ** const fileName) const;

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

  void GetProjectNameAndSemVer(std::string const ** const projectName,
                               std::string const ** const semVer) const;

  int GetEnvironmentVariableName(CollectionItemType const itemType,
                                 std::string const ** const name) const;

  void GetConfigurationFileEnvironmentVariable(
      std::string const ** const name, std::string const ** const value) const;

  void GetConfigurationFileName(std::string const ** const fileName) const;

  int CacheListOfDirectoryNames(Collection const collection,
                                CollectionItemType const itemType,
                                int * const extent);
  int GetDirectoryName(int const index,
                       std::string const ** const directoryName) const;

  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();
  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

 private:
  // do not allow copy constructor or operator=
  CollectionsImplementation(CollectionsImplementation const &);
  void operator=(CollectionsImplementation const &);

  CollectionsImplementation(Log * const log);
  ~CollectionsImplementation();

  Log * log_;

  mutable std::string getItemLibraryFileNameAndCollection_FileName_;

  std::vector<std::string> cacheListOfItemMetadataFiles_Names_;
  std::vector<int> cacheListOfItemMetadataFiles_availableAsString_;
  std::vector<std::string> cacheListOfItemMetadataFiles_RawData_;

  std::vector<std::string> cacheListOfItemNamesByType_;

  std::vector<std::string> cacheListOfItemNamesByCollectionAndType_;

  mutable std::string getItemLibraryFileNameByCollectionAndType_;

  std::vector<std::string>
      cacheListOfItemMetadataFilesByCollectionAndType_FileNames_;
  std::vector<int>
      cacheListOfItemMetadataFilesByCollectionAndType_AvailableAsString_;
  std::vector<std::string>
      cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_;

  mutable std::string getProjectNameAndSemVer_ProjectName_;
  mutable std::string getProjectNameAndSemVer_SemVer_;

  mutable std::string getEnvironmentVariableName_;

  mutable std::string getConfigurationFileEnvironmentVariable_Name_;
  mutable std::string getConfigurationFileEnvironmentVariable_Value_;

  mutable std::string getConfigurationFileName_;

  std::vector<std::string> cacheListOfDirectoryNames_;
};  // class CollectionsImplementation
}  // namespace KIM
#endif  // KIM_COLLECTIONS_IMPLEMENTATION_HPP_

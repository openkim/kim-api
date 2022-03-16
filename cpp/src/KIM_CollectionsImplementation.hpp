//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_COLLECTIONS_IMPLEMENTATION_HPP_
#define KIM_COLLECTIONS_IMPLEMENTATION_HPP_

#include <map>
#include <sstream>
#include <string>
#include <vector>

#ifndef KIM_FILESYSTEM_Path_HPP_
#include "KIM_FilesystemPath.hpp"
#endif

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

  static bool
  WriteConfigurationFileAndCreateDirectories(  // for internal use only
      FILESYSTEM::Path const & fileName,
      std::map<CollectionItemType,
               FILESYSTEM::PathList,
               COLLECTION_ITEM_TYPE::Comparator> const & dirsMap);

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

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

#include <list>
#include <vector>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

#ifndef KIM_SHARED_LIBRARY_HPP_
#include "KIM_SharedLibrary.hpp"
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

  int GetTypeOfItem(std::string const & itemName,
                    CollectionItemType * const typeOfItem) const;

  int GetItem(CollectionItemType const itemType,
              std::string const & itemName,
              std::string const ** const path,
              int * const metadataExtent,
              Collection * const collection) const;
  int GetItemMetadata(CollectionItemType const itemType,
                      std::string const & itemName,
                      int const index,
                      std::string const ** const metadataID,
                      unsigned int * const metadataLength,
                      unsigned char const ** const metadataRawData,
                      int * const availableAsString,
                      std::string const ** const metadataString) const;
  int GetItemNamesByType(CollectionItemType const itemType,
                         std::string const ** const itemNames) const;

  int GetItemNamesByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const ** const itemNames) const;
  int GetItemByCollectionAndType(Collection const collection,
                                 CollectionItemType const itemType,
                                 std::string const & itemName,
                                 std::string const ** const path,
                                 int * const metadataExtent) const;
  int GetItemMetadataByCollectionAndType(
      Collection const collection,
      CollectionItemType const itemType,
      std::string const & itemName,
      int const index,
      std::string const ** const metadataID,
      unsigned int * const metadataLength,
      unsigned char const ** const metadataRawData,
      int * const availableAsString,
      std::string const ** const metadataString) const;

  void GetProjectNameAndSemVer(std::string const ** const projectName,
                               std::string const ** const semVer) const;

  int GetEnvironmentVariableName(CollectionItemType const itemType,
                                 std::string const ** const name) const;

  void GetConfigurationFileEnvironmentVariable(
      std::string const ** const name, std::string const ** const value) const;

  void GetConfigurationFilePath(std::string const ** const filePath) const;

  int GetDirectories(Collection const collection,
                     CollectionItemType const itemType,
                     std::string const ** const directories) const;

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

  mutable std::string getItemPath_;

  mutable std::string getItemMetadataID_;
  mutable std::string getItemMetadataString_;

  mutable std::string getItemNamesByType_;

  mutable std::string getItemNamesByCollectionAndType_;

  mutable std::string getItemByCollectionAndTypePath_;

  mutable std::string getItemMetadataByCollectionAndTypeID_;
  mutable std::string getItemMetadataByCollectionAndTypeString_;

  mutable std::string getProjectNameAndSemVerProjectName_;
  mutable std::string getProjectNameAndSemVerSemVer_;

  mutable std::string getEnvironmentVariableName_;

  mutable std::string getConfigurationFileEnvironmentVariableName_;
  mutable std::string getConfigurationFileEnvironmentVariableValue_;

  mutable std::string getConfigurationFilePath_;

  mutable std::string getDirectories_;
};  // class CollectionsImplementation
}  // namespace KIM
#endif  // KIM_COLLECTIONS_IMPLEMENTATION_HPP_

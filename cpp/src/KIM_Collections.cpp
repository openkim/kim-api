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
#include "KIM_Collections.hpp"
#endif

#ifndef KIM_COLLECTIONS_IMPLEMENTATION_HPP_
#include "KIM_CollectionsImplementation.hpp"
#endif

namespace KIM
{
int Collections::Create(Collections ** const collections)
{
  *collections = new Collections();

  int error = CollectionsImplementation::Create(&((*collections)->pimpl));
  if (error)
  {
    delete *collections;
    *collections = NULL;
    return true;
  }
  else
  {
    return false;
  }
}

void Collections::Destroy(Collections ** const collections)
{
  CollectionsImplementation::Destroy(&((*collections)->pimpl));
  delete *collections;
  *collections = NULL;
}

int Collections::GetTypeOfItem(std::string const & itemName,
                               CollectionItemType * const typeOfItem) const
{
  return pimpl->GetTypeOfItem(itemName, typeOfItem);
}

int Collections::GetItem(CollectionItemType const itemType,
                         std::string const & itemName,
                         std::string const ** const path,
                         int * const metadataExtent,
                         Collection * const collection) const
{
  return pimpl->GetItem(itemType, itemName, path, metadataExtent, collection);
}

int Collections::GetItemMetadata(
    CollectionItemType const itemType,
    std::string const & itemName,
    int const index,
    std::string const ** const metadataID,
    int * const metadataLength,
    unsigned char const ** const metadataRawData,
    int * const availableAsString,
    std::string const ** const metadataString) const
{
  return pimpl->GetItemMetadata(itemType,
                                itemName,
                                index,
                                metadataID,
                                metadataLength,
                                metadataRawData,
                                availableAsString,
                                metadataString);
}

int Collections::GetItemNamesByType(CollectionItemType const itemType,
                                    std::string const ** const itemNames) const
{
  return pimpl->GetItemNamesByType(itemType, itemNames);
}

int Collections::GetItemNamesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const ** const itemNames) const
{
  return pimpl->GetItemNamesByCollectionAndType(
      collection, itemType, itemNames);
}

int Collections::GetItemByCollectionAndType(Collection const collection,
                                            CollectionItemType const itemType,
                                            std::string const & itemName,
                                            std::string const ** const path,
                                            int * const metadataExtent) const
{
  return pimpl->GetItemByCollectionAndType(
      collection, itemType, itemName, path, metadataExtent);
}

int Collections::GetItemMetadataByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    int const index,
    std::string const ** const metadataID,
    int * const metadataLength,
    unsigned char const ** const metadataRawData,
    int * const availableAsString,
    std::string const ** const metadataString) const
{
  return pimpl->GetItemMetadataByCollectionAndType(collection,
                                                   itemType,
                                                   itemName,
                                                   index,
                                                   metadataID,
                                                   metadataLength,
                                                   metadataRawData,
                                                   availableAsString,
                                                   metadataString);
}

void Collections::GetProjectNameAndSemVer(
    std::string const ** const projectName,
    std::string const ** const semVer) const
{
  pimpl->GetProjectNameAndSemVer(projectName, semVer);
}

int Collections::GetEnvironmentVariableName(
    CollectionItemType const itemType, std::string const ** const name) const
{
  return pimpl->GetEnvironmentVariableName(itemType, name);
}

void Collections::GetConfigurationFileEnvironmentVariable(
    std::string const ** const name, std::string const ** const value) const
{
  pimpl->GetConfigurationFileEnvironmentVariable(name, value);
}

void Collections::GetConfigurationFilePath(
    std::string const ** const filePath) const
{
  pimpl->GetConfigurationFilePath(filePath);
}

int Collections::GetDirectories(Collection const collection,
                                CollectionItemType const itemType,
                                std::string const ** const directories) const
{
  return pimpl->GetDirectories(collection, itemType, directories);
}

void Collections::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void Collections::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void Collections::PopLogVerbosity() { pimpl->PopLogVerbosity(); }

Collections::Collections() : pimpl(NULL) {}

Collections::~Collections() {}

}  // namespace KIM

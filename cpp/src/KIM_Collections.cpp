//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
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


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_COLLECTIONS_HPP_
#include "KIM_Collections.hpp"
#endif

#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
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
  else { return false; }
}

void Collections::Destroy(Collections ** const collections)
{
  if (*collections != NULL)
  {
    CollectionsImplementation::Destroy(&((*collections)->pimpl));
  }
  delete *collections;
  *collections = NULL;
}

int Collections::GetItemType(std::string const & itemName,
                             CollectionItemType * const itemType) const
{
  return pimpl->GetItemType(itemName, itemType);
}

int Collections::GetItemLibraryFileNameAndCollection(
    CollectionItemType const itemType,
    std::string const & itemName,
    std::string const ** const fileName,
    Collection * const collection) const
{
  return pimpl->GetItemLibraryFileNameAndCollection(
      itemType, itemName, fileName, collection);
}

int Collections::CacheListOfItemMetadataFiles(CollectionItemType const itemType,
                                              std::string const & itemName,
                                              int * const extent)
{
  return pimpl->CacheListOfItemMetadataFiles(itemType, itemName, extent);
}

int Collections::GetItemMetadataFile(
    int const index,
    std::string const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    std::string const ** const fileString) const
{
  return pimpl->GetItemMetadataFile(
      index, fileName, fileLength, fileRawData, availableAsString, fileString);
}

int Collections::CacheListOfItemNamesByType(CollectionItemType const itemType,
                                            int * const extent)
{
  return pimpl->CacheListOfItemNamesByType(itemType, extent);
}

int Collections::GetItemNameByType(int const index,
                                   std::string const ** const itemName) const
{
  return pimpl->GetItemNameByType(index, itemName);
}

int Collections::CacheListOfItemNamesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    int * const extent)
{
  return pimpl->CacheListOfItemNamesByCollectionAndType(
      collection, itemType, extent);
}

int Collections::GetItemNameByCollectionAndType(
    int const index, std::string const ** const itemNames) const
{
  return pimpl->GetItemNameByCollectionAndType(index, itemNames);
}

int Collections::GetItemLibraryFileNameByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    std::string const ** const fileName) const
{
  return pimpl->GetItemLibraryFileNameByCollectionAndType(
      collection, itemType, itemName, fileName);
}

int Collections::CacheListOfItemMetadataFilesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    int * const extent)
{
  return pimpl->CacheListOfItemMetadataFilesByCollectionAndType(
      collection, itemType, itemName, extent);
}

int Collections::GetItemMetadataFileByCollectionAndType(
    int const index,
    std::string const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    std::string const ** const fileString) const
{
  return pimpl->GetItemMetadataFileByCollectionAndType(
      index, fileName, fileLength, fileRawData, availableAsString, fileString);
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

void Collections::GetConfigurationFileName(
    std::string const ** const fileName) const
{
  pimpl->GetConfigurationFileName(fileName);
}

int Collections::CacheListOfDirectoryNames(Collection const collection,
                                           CollectionItemType const itemType,
                                           int * const extent)
{
  return pimpl->CacheListOfDirectoryNames(collection, itemType, extent);
}

int Collections::GetDirectoryName(
    int const index, std::string const ** const directoryName) const
{
  return pimpl->GetDirectoryName(index, directoryName);
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

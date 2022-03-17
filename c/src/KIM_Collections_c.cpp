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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <climits>
#include <cstddef>
#include <iostream>
#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"


#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif
extern "C" {
#ifndef KIM_COLLECTION_H_
#include "KIM_Collection.h"
#endif
}  // extern "C"

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif
extern "C" {
#ifndef KIM_COLLECTION_ITEM_TYPE_H_
#include "KIM_CollectionItemType.h"
#endif
}  // extern "C"

#ifndef KIM_COLLECTIONS_HPP_
#include "KIM_Collections.hpp"
#endif
extern "C" {
#ifndef KIM_COLLECTIONS_H_
#include "KIM_Collections.h"
#endif
}  // extern "C"

struct KIM_Collections
{
  void * p;
};

#define CONVERT_POINTER           \
  KIM::Collections * pCollections \
      = reinterpret_cast<KIM::Collections *>(collections->p)

namespace
{
KIM::CollectionItemType makeItemTypeCpp(KIM_CollectionItemType const itemType)
{
  return KIM::CollectionItemType(itemType.collectionItemTypeID);
}

KIM::Collection makeCollectionCpp(KIM_Collection const collection)
{
  return KIM::Collection(collection.collectionID);
}

KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


int KIM_Collections_Create(KIM_Collections ** const collections)
{
  KIM::Collections * pCollections;
  int error = KIM::Collections::Create(&pCollections);
  if (error)
  {
    *collections = NULL;
    return true;
  }
  else
  {
    (*collections) = new KIM_Collections;
    (*collections)->p = (void *) pCollections;
    return false;
  }
}

void KIM_Collections_Destroy(KIM_Collections ** const collections)
{
  if (*collections != NULL)
  {
    KIM::Collections * pCollections
        = reinterpret_cast<KIM::Collections *>((*collections)->p);

    KIM::Collections::Destroy(&pCollections);
  }
  delete (*collections);
  *collections = NULL;
}

int KIM_Collections_GetItemType(KIM_Collections * const collections,
                                char const * const itemName,
                                KIM_CollectionItemType * const itemType)
{
  CONVERT_POINTER;

  return pCollections->GetItemType(
      itemName, reinterpret_cast<KIM::CollectionItemType *>(itemType));
}

int KIM_Collections_GetItemLibraryFileNameAndCollection(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    char const ** const fileName,
    KIM_Collection * const collection)
{
  CONVERT_POINTER;

  std::string const * pStrFileName;
  int error = pCollections->GetItemLibraryFileNameAndCollection(
      makeItemTypeCpp(itemType),
      itemName,
      &pStrFileName,
      reinterpret_cast<KIM::Collection *>(collection));
  if (error)
    return true;
  else
  {
    *fileName = pStrFileName->c_str();
    return false;
  }
}

int KIM_Collections_CacheListOfItemMetadataFiles(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    int * const extent)
{
  CONVERT_POINTER;

  return pCollections->CacheListOfItemMetadataFiles(
      makeItemTypeCpp(itemType), itemName, extent);
}

int KIM_Collections_GetItemMetadataFile(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString)
{
  CONVERT_POINTER;

  std::string const * pStrFileName;
  std::string const ** ppStrFileName;
  if (fileName == NULL)
    ppStrFileName = NULL;
  else
    ppStrFileName = &pStrFileName;

  std::string const * pStrFileString;
  std::string const ** ppStrFileString;
  if (fileString == NULL)
    ppStrFileString = NULL;
  else
    ppStrFileString = &pStrFileString;

  int error = pCollections->GetItemMetadataFile(index,
                                                ppStrFileName,
                                                fileLength,
                                                fileRawData,
                                                availableAsString,
                                                ppStrFileString);
  if (error)
    return true;
  else
  {
    if (fileName != NULL) *fileName = pStrFileName->c_str();
    if (fileString != NULL)
    {
      if (pStrFileString == NULL)
        *fileString = NULL;
      else
        *fileString = pStrFileString->c_str();
    }
    return false;
  }
}

extern "C" {
int KIM_Collections_GetItemMetadataFile_fortran(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    long * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString)
{
  unsigned int uFileLength;
  int error = KIM_Collections_GetItemMetadataFile(collections,
                                                  index,
                                                  fileName,
                                                  &uFileLength,
                                                  fileRawData,
                                                  availableAsString,
                                                  fileString);
  if (error)
    return true;
  else if ((sizeof(int) == sizeof(long)) && (uFileLength > INT_MAX))
  {
    std::cerr << "* Error : long overflow for fileLength. " << __FILE__ << ":"
              << __LINE__ << std::endl;
    return true;
  }
  else
  {
    *fileLength = static_cast<long>(uFileLength);
    return false;
  }
}
}  // extern "C"

int KIM_Collections_CacheListOfItemNamesByType(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    int * const extent)
{
  CONVERT_POINTER;

  return pCollections->CacheListOfItemNamesByType(makeItemTypeCpp(itemType),
                                                  extent);
}

int KIM_Collections_GetItemNameByType(KIM_Collections * const collections,
                                      int const index,
                                      char const ** const itemName)
{
  CONVERT_POINTER;

  std::string const * pStrItemName;
  int error = pCollections->GetItemNameByType(index, &pStrItemName);
  if (error)
    return true;
  else
  {
    *itemName = pStrItemName->c_str();
    return false;
  }
}

int KIM_Collections_CacheListOfItemNamesByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    int * const extent)
{
  CONVERT_POINTER;

  return pCollections->CacheListOfItemNamesByCollectionAndType(
      makeCollectionCpp(collection), makeItemTypeCpp(itemType), extent);
}

int KIM_Collections_GetItemNameByCollectionAndType(
    KIM_Collections * const collections,
    int const index,
    char const ** const itemName)
{
  CONVERT_POINTER;

  std::string const * pStrItemName;
  int error
      = pCollections->GetItemNameByCollectionAndType(index, &pStrItemName);
  if (error)
    return true;
  else
  {
    *itemName = pStrItemName->c_str();
    return false;
  }
}

int KIM_Collections_GetItemLibraryFileNameByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    char const ** const fileName)
{
  CONVERT_POINTER;

  std::string const * pStrFileName;
  int error = pCollections->GetItemLibraryFileNameByCollectionAndType(
      makeCollectionCpp(collection),
      makeItemTypeCpp(itemType),
      itemName,
      &pStrFileName);
  if (error)
    return true;
  else
  {
    *fileName = pStrFileName->c_str();
    return false;
  }
}

int KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    int * const extent)
{
  CONVERT_POINTER;

  return pCollections->CacheListOfItemMetadataFilesByCollectionAndType(
      makeCollectionCpp(collection),
      makeItemTypeCpp(itemType),
      itemName,
      extent);
}

int KIM_Collections_GetItemMetadataFileByCollectionAndType(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString)
{
  CONVERT_POINTER;

  std::string const * pStrFileName;
  std::string const ** ppStrFileName;
  if (fileName == NULL)
    ppStrFileName = NULL;
  else
    ppStrFileName = &pStrFileName;

  std::string const * pStrFileString;
  std::string const ** ppStrFileString;
  if (fileString == NULL)
    ppStrFileString = NULL;
  else
    ppStrFileString = &pStrFileString;

  int error
      = pCollections->GetItemMetadataFileByCollectionAndType(index,
                                                             ppStrFileName,
                                                             fileLength,
                                                             fileRawData,
                                                             availableAsString,
                                                             ppStrFileString);
  if (error)
    return true;
  else
  {
    if (fileName != NULL) *fileName = pStrFileName->c_str();
    if (fileString != NULL)
    {
      if (pStrFileString == NULL)
        *fileString = NULL;
      else
        *fileString = pStrFileString->c_str();
    }
    return false;
  }
}

extern "C" {
int KIM_Collections_GetItemMetadataFileByCollectionAndType_fortran(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    long * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString)
{
  unsigned int uFileLength;
  int error = KIM_Collections_GetItemMetadataFileByCollectionAndType(
      collections,
      index,
      fileName,
      &uFileLength,
      fileRawData,
      availableAsString,
      fileString);
  if (error)
    return true;
  else if ((sizeof(int) == sizeof(long)) && (uFileLength > INT_MAX))
  {
    std::cerr << "* Error : long overflow for fileLength. " << __FILE__ << ":"
              << __LINE__ << std::endl;
    return true;
  }
  else
  {
    *fileLength = static_cast<long>(uFileLength);
    return false;
  }
}
}  // extern "C"

void KIM_Collections_GetProjectNameAndSemVer(
    KIM_Collections * const collections,
    char const ** const projectName,
    char const ** const semVer)
{
  CONVERT_POINTER;

  std::string const * pStrProjectName;
  std::string const ** ppStrProjectName;
  if (projectName == NULL)
    ppStrProjectName = NULL;
  else
    ppStrProjectName = &pStrProjectName;

  std::string const * pStrSemVer;
  std::string const ** ppStrSemVer;
  if (semVer == NULL)
    ppStrSemVer = NULL;
  else
    ppStrSemVer = &pStrSemVer;

  pCollections->GetProjectNameAndSemVer(ppStrProjectName, ppStrSemVer);
  if (projectName != NULL) *projectName = pStrProjectName->c_str();
  if (semVer != NULL) *semVer = pStrSemVer->c_str();
}

int KIM_Collections_GetEnvironmentVariableName(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const ** const name)
{
  CONVERT_POINTER;

  std::string const * pStrName;
  int error = pCollections->GetEnvironmentVariableName(
      makeItemTypeCpp(itemType), &pStrName);
  if (error)
    return true;
  else
  {
    *name = pStrName->c_str();
    return false;
  }
}

void KIM_Collections_GetConfigurationFileEnvironmentVariable(
    KIM_Collections * const collections,
    char const ** const name,
    char const ** const value)
{
  CONVERT_POINTER;

  std::string const * pStrName;
  std::string const ** ppStrName;
  if (name == NULL)
    ppStrName = NULL;
  else
    ppStrName = &pStrName;

  std::string const * pStrValue;
  std::string const ** ppStrValue;
  if (value == NULL)
    ppStrValue = NULL;
  else
    ppStrValue = &pStrValue;

  pCollections->GetConfigurationFileEnvironmentVariable(ppStrName, ppStrValue);

  if (name != NULL) *name = pStrName->c_str();
  if (value != NULL) *value = pStrValue->c_str();
}

void KIM_Collections_GetConfigurationFileName(
    KIM_Collections * const collections, char const ** const fileName)
{
  CONVERT_POINTER;

  std::string const * pStrFileName;
  pCollections->GetConfigurationFileName(&pStrFileName);
  *fileName = pStrFileName->c_str();
}

int KIM_Collections_CacheListOfDirectoryNames(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    int * const extent)
{
  CONVERT_POINTER;

  return pCollections->CacheListOfDirectoryNames(
      makeCollectionCpp(collection), makeItemTypeCpp(itemType), extent);
}

int KIM_Collections_GetDirectoryName(KIM_Collections * const collections,
                                     int const index,
                                     char const ** const directoryName)
{
  CONVERT_POINTER;

  std::string const * pStrDirectoryName;
  int error = pCollections->GetDirectoryName(index, &pStrDirectoryName);
  if (error)
    return true;
  else
  {
    *directoryName = pStrDirectoryName->c_str();
    return false;
  }
}

void KIM_Collections_SetLogID(KIM_Collections * const collections,
                              char const * const logID)
{
  CONVERT_POINTER;

  pCollections->SetLogID(logID);
}

void KIM_Collections_PushLogVerbosity(KIM_Collections * const collections,
                                      KIM_LogVerbosity const logVerbosity)
{
  CONVERT_POINTER;

  pCollections->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Collections_PopLogVerbosity(KIM_Collections * const collections)
{
  CONVERT_POINTER;

  pCollections->PopLogVerbosity();
}

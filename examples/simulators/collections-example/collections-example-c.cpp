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


extern "C" {
#include "KIM_Collection.h"
#include "KIM_CollectionItemType.h"
#include "KIM_Collections.h"
}
#include <iostream>

void dirsForCollection(KIM_Collection const collection,
                       KIM_Collections * const col)
{
  int extent = 0;

  {
    KIM_Collections_CacheListOfDirectoryNames(
        col, collection, KIM_COLLECTION_ITEM_TYPE_modelDriver, &extent);
    std::cout << KIM_Collection_ToString(collection) << ":"
              << KIM_CollectionItemType_ToString(
                     KIM_COLLECTION_ITEM_TYPE_modelDriver)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * dir;
      KIM_Collections_GetDirectoryName(col, i, &dir);
      std::cout << "\t" << dir << std::endl;
    }

    extent = 0;
    KIM_Collections_CacheListOfDirectoryNames(
        col, collection, KIM_COLLECTION_ITEM_TYPE_portableModel, &extent);
    std::cout << KIM_Collection_ToString(collection) << ":"
              << KIM_CollectionItemType_ToString(
                     KIM_COLLECTION_ITEM_TYPE_portableModel)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * dir;
      KIM_Collections_GetDirectoryName(col, i, &dir);
      std::cout << "\t" << dir << std::endl;
    }

    extent = 0;
    KIM_Collections_CacheListOfDirectoryNames(
        col, collection, KIM_COLLECTION_ITEM_TYPE_simulatorModel, &extent);
    std::cout << KIM_Collection_ToString(collection) << ":"
              << KIM_CollectionItemType_ToString(
                     KIM_COLLECTION_ITEM_TYPE_simulatorModel)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * dir;
      KIM_Collections_GetDirectoryName(col, i, &dir);
      std::cout << "\t" << dir << std::endl;
    }
  }
}

void namesForCollection(KIM_Collection kc, KIM_Collections * const col)
{
  int extent;
  KIM_Collections_CacheListOfItemNamesByCollectionAndType(
      col, kc, KIM_COLLECTION_ITEM_TYPE_modelDriver, &extent);
  std::cout << KIM_Collection_ToString(kc) << ":"
            << KIM_CollectionItemType_ToString(
                   KIM_COLLECTION_ITEM_TYPE_modelDriver)
            << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    char const * name;
    KIM_Collections_GetItemNameByCollectionAndType(col, i, &name);
    std::cout << "\t" << name << std::endl;
  }
  KIM_Collections_CacheListOfItemNamesByCollectionAndType(
      col, kc, KIM_COLLECTION_ITEM_TYPE_portableModel, &extent);
  std::cout << KIM_Collection_ToString(kc) << ":"
            << KIM_CollectionItemType_ToString(
                   KIM_COLLECTION_ITEM_TYPE_portableModel)
            << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    char const * name;
    KIM_Collections_GetItemNameByCollectionAndType(col, i, &name);
    std::cout << "\t" << name << std::endl;
  }
  KIM_Collections_CacheListOfItemNamesByCollectionAndType(
      col, kc, KIM_COLLECTION_ITEM_TYPE_simulatorModel, &extent);
  std::cout << KIM_Collection_ToString(kc) << ":"
            << KIM_CollectionItemType_ToString(
                   KIM_COLLECTION_ITEM_TYPE_simulatorModel)
            << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    char const * name;
    KIM_Collections_GetItemNameByCollectionAndType(col, i, &name);
    std::cout << "\t" << name << std::endl;
  }
}


int main()
{
  KIM_Collections * col;

  int error = KIM_Collections_Create(&col);

  if (error)
  {
    std::cerr << "Unable to create collections object." << std::endl;
    return 1;
  }

  {
    char const * project;
    char const * semVer;
    KIM_Collections_GetProjectNameAndSemVer(col, &project, &semVer);

    std::cout << "Project : " << project << std::endl;
    std::cout << "semVer  : " << semVer << std::endl;
    std::cout << std::endl;
  }

  {
    char const * name;
    KIM_Collections_GetEnvironmentVariableName(
        col, KIM_COLLECTION_ITEM_TYPE_modelDriver, &name);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_modelDriver)
              << " env name : " << name << std::endl;
    std::cout << std::endl;
  }
  {
    char const * name;
    KIM_Collections_GetEnvironmentVariableName(
        col, KIM_COLLECTION_ITEM_TYPE_portableModel, &name);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_portableModel)
              << " env name : " << name << std::endl;
    std::cout << std::endl;
  }
  {
    char const * name;
    KIM_Collections_GetEnvironmentVariableName(
        col, KIM_COLLECTION_ITEM_TYPE_simulatorModel, &name);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_simulatorModel)
              << " env name : " << name << std::endl;
    std::cout << std::endl;
  }


  {
    char const * name;
    char const * value;
    KIM_Collections_GetConfigurationFileEnvironmentVariable(col, &name, &value);
    std::cout << "config file env name : " << name << std::endl
              << "config file env value: " << value << std::endl;
    std::cout << std::endl;
  }


  {
    char const * fileName;
    KIM_Collections_GetConfigurationFileName(col, &fileName);
    std::cout << "config file name : " << fileName << std::endl;
    std::cout << std::endl;
  }


  {
    dirsForCollection(KIM_COLLECTION_system, col);
    dirsForCollection(KIM_COLLECTION_user, col);
    dirsForCollection(KIM_COLLECTION_environmentVariable, col);
    dirsForCollection(KIM_COLLECTION_currentWorkingDirectory, col);
    std::cout << std::endl;
  }


  {
    namesForCollection(KIM_COLLECTION_system, col);
    namesForCollection(KIM_COLLECTION_user, col);
    namesForCollection(KIM_COLLECTION_environmentVariable, col);
    namesForCollection(KIM_COLLECTION_currentWorkingDirectory, col);
    std::cout << std::endl;
  }


  {
    int extent;
    KIM_Collections_CacheListOfItemNamesByType(
        col, KIM_COLLECTION_ITEM_TYPE_modelDriver, &extent);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_modelDriver)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * name;
      KIM_Collections_GetItemNameByType(col, i, &name);
      std::cout << "\t" << name << std::endl;
    }
    KIM_Collections_CacheListOfItemNamesByType(
        col, KIM_COLLECTION_ITEM_TYPE_portableModel, &extent);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_portableModel)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * name;
      KIM_Collections_GetItemNameByType(col, i, &name);
      std::cout << "\t" << name << std::endl;
    }
    KIM_Collections_CacheListOfItemNamesByType(
        col, KIM_COLLECTION_ITEM_TYPE_simulatorModel, &extent);
    std::cout << KIM_CollectionItemType_ToString(
        KIM_COLLECTION_ITEM_TYPE_simulatorModel)
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      char const * name;
      KIM_Collections_GetItemNameByType(col, i, &name);
      std::cout << "\t" << name << std::endl;
    }
  }

  {
    char const * fileName;
    KIM_Collection collection;
    int error = KIM_Collections_GetItemLibraryFileNameAndCollection(
        col,
        KIM_COLLECTION_ITEM_TYPE_simulatorModel,
        "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu",
        &fileName,
        &collection);
    if (!error)
      std::cout
          << "Simulator Model Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu"
          << " has library name '" << fileName << "' and is part of the '"
          << KIM_Collection_ToString(collection) << "' collection."
          << std::endl;
    else
      std::cout << "Error from GetItemLibraryFileNameAndCollection."
                << std::endl;
  }

  {
    int extent;
    int error = KIM_Collections_CacheListOfItemMetadataFiles(
        col,
        KIM_COLLECTION_ITEM_TYPE_simulatorModel,
        "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu",
        &extent);
    if (error)
      std::cout << "Error from CacheListOfItemMetadataFiles." << std::endl;
    else
    {
      char const * fileName;
      unsigned int fileLength;
      unsigned char const * fileRawData;
      int availableAsString;
      char const * fileString;
      for (int i = 0; i < extent; ++i)
      {
        KIM_Collections_GetItemMetadataFile(col,
                                            i,
                                            &fileName,
                                            &fileLength,
                                            &fileRawData,
                                            &availableAsString,
                                            &fileString);
        std::cout << "Metadata File " << i << ", " << fileName
                  << ", is of length " << fileLength << std::endl
                  << fileString << std::endl;
      }
    }
  }

  KIM_Collections_Destroy(&col);
  return 0;
}

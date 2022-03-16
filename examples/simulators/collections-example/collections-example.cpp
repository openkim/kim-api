//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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


#include "KIM_Collection.hpp"
#include "KIM_CollectionItemType.hpp"
#include "KIM_Collections.hpp"
#include <iostream>
#include <string>

void dirsForCollection(KIM::Collection const collection,
                       KIM::Collections * const col)
{
  int extent = 0;

  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    col->CacheListOfDirectoryNames(collection, modelDriver, &extent);
    std::cout << collection.ToString() << ":" << modelDriver.ToString()
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * dir;
      col->GetDirectoryName(i, &dir);
      std::cout << "\t" << *dir << std::endl;
    }

    extent = 0;
    col->CacheListOfDirectoryNames(collection, portableModel, &extent);
    std::cout << collection.ToString() << ":" << portableModel.ToString()
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * dir;
      col->GetDirectoryName(i, &dir);
      std::cout << "\t" << *dir << std::endl;
    }

    extent = 0;
    col->CacheListOfDirectoryNames(collection, simulatorModel, &extent);
    std::cout << collection.ToString() << ":" << simulatorModel.ToString()
              << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * dir;
      col->GetDirectoryName(i, &dir);
      std::cout << "\t" << *dir << std::endl;
    }
  }
}

void namesForCollection(KIM::Collection kc, KIM::Collections * const col)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;
  int extent;
  col->CacheListOfItemNamesByCollectionAndType(kc, modelDriver, &extent);
  std::cout << kc.ToString() << ":" << modelDriver.ToString() << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    std::string const * name;
    col->GetItemNameByCollectionAndType(i, &name);
    std::cout << "\t" << *name << std::endl;
  }
  col->CacheListOfItemNamesByCollectionAndType(kc, portableModel, &extent);
  std::cout << kc.ToString() << ":" << portableModel.ToString() << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    std::string const * name;
    col->GetItemNameByCollectionAndType(i, &name);
    std::cout << "\t" << *name << std::endl;
  }
  col->CacheListOfItemNamesByCollectionAndType(kc, simulatorModel, &extent);
  std::cout << kc.ToString() << ":" << simulatorModel.ToString() << " :\n";
  for (int i = 0; i < extent; ++i)
  {
    std::string const * name;
    col->GetItemNameByCollectionAndType(i, &name);
    std::cout << "\t" << *name << std::endl;
  }
}


int main()
{
  KIM::Collections * col;

  int error = KIM::Collections::Create(&col);

  if (error)
  {
    std::cerr << "Unable to create collections object." << std::endl;
    return 1;
  }

  {
    std::string const * project;
    std::string const * semVer;
    col->GetProjectNameAndSemVer(&project, &semVer);

    std::cout << "Project : " << *project << std::endl;
    std::cout << "semVer  : " << *semVer << std::endl;
    std::cout << std::endl;
  }

  {
    std::string const * name;
    col->GetEnvironmentVariableName(KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                    &name);
    std::cout << KIM::COLLECTION_ITEM_TYPE::modelDriver.ToString()
              << " env name : " << *name << std::endl;
    std::cout << std::endl;
  }
  {
    std::string const * name;
    col->GetEnvironmentVariableName(KIM::COLLECTION_ITEM_TYPE::portableModel,
                                    &name);
    std::cout << KIM::COLLECTION_ITEM_TYPE::portableModel.ToString()
              << " env name : " << *name << std::endl;
    std::cout << std::endl;
  }
  {
    std::string const * name;
    col->GetEnvironmentVariableName(KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                    &name);
    std::cout << KIM::COLLECTION_ITEM_TYPE::simulatorModel.ToString()
              << " env name : " << *name << std::endl;
    std::cout << std::endl;
  }


  {
    std::string const * name;
    std::string const * value;
    col->GetConfigurationFileEnvironmentVariable(&name, &value);
    std::cout << "config file env name : " << *name << std::endl
              << "config file env value: " << *value << std::endl;
    std::cout << std::endl;
  }


  {
    std::string const * fileName;
    col->GetConfigurationFileName(&fileName);
    std::cout << "config file name : " << *fileName << std::endl;
    std::cout << std::endl;
  }


  {
    using namespace KIM::COLLECTION;
    dirsForCollection(KIM::COLLECTION::system, col);
    dirsForCollection(user, col);
    dirsForCollection(environmentVariable, col);
    dirsForCollection(currentWorkingDirectory, col);
    std::cout << std::endl;
  }


  {
    using namespace KIM::COLLECTION;
    namesForCollection(KIM::COLLECTION::system, col);
    namesForCollection(user, col);
    namesForCollection(environmentVariable, col);
    namesForCollection(currentWorkingDirectory, col);
    std::cout << std::endl;
  }


  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    int extent;
    col->CacheListOfItemNamesByType(modelDriver, &extent);
    std::cout << modelDriver.ToString() << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * name;
      col->GetItemNameByType(i, &name);
      std::cout << "\t" << *name << std::endl;
    }
    col->CacheListOfItemNamesByType(portableModel, &extent);
    std::cout << portableModel.ToString() << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * name;
      col->GetItemNameByType(i, &name);
      std::cout << "\t" << *name << std::endl;
    }
    col->CacheListOfItemNamesByType(simulatorModel, &extent);
    std::cout << simulatorModel.ToString() << " :\n";
    for (int i = 0; i < extent; ++i)
    {
      std::string const * name;
      col->GetItemNameByType(i, &name);
      std::cout << "\t" << *name << std::endl;
    }
  }

  {
    std::string const * fileName;
    KIM::Collection collection;
    int error = col->GetItemLibraryFileNameAndCollection(
        KIM::COLLECTION_ITEM_TYPE::simulatorModel,
        "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu",
        &fileName,
        &collection);
    if (!error)
      std::cout
          << "Simulator Model Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu"
          << " has library name '" << *fileName << "' and is part of the '"
          << collection.ToString() << "' collection." << std::endl;
    else
      std::cout << "Error from GetItemLibraryFileNameAndCollection."
                << std::endl;
  }

  {
    int extent;
    int error = col->CacheListOfItemMetadataFiles(
        KIM::COLLECTION_ITEM_TYPE::simulatorModel,
        "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu",
        &extent);
    if (error)
      std::cout << "Error from CacheListOfItemMetadataFiles." << std::endl;
    else
    {
      std::string const * fileName;
      unsigned int fileLength;
      unsigned char const * fileRawData;
      int availableAsString;
      std::string const * fileString;
      for (int i = 0; i < extent; ++i)
      {
        col->GetItemMetadataFile(i,
                                 &fileName,
                                 &fileLength,
                                 &fileRawData,
                                 &availableAsString,
                                 &fileString);
        std::cout << "Metadata File " << i << ", " << *fileName
                  << ", is of length " << fileLength << std::endl
                  << *fileString << std::endl;
      }
    }
  }

  KIM::Collections::Destroy(&col);
  return 0;
}

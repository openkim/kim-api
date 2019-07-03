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


#include "KIM_Collection.hpp"
#include "KIM_CollectionItemType.hpp"
#include "KIM_Collections.hpp"
#include <iomanip>
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


  return 0;
}

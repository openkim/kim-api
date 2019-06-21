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


#include "KIM_Collection.hpp"
#include "KIM_CollectionItemType.hpp"
#include "KIM_Collections.hpp"
#include <iomanip>
#include <iostream>
#include <string>

void dirsForCollection(KIM::Collection const collection, KIM::Collections & col)
{
  std::string const * dirs;

  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    col.GetDirectories(collection, modelDriver, &dirs);
    std::cout << collection.ToString() << ":" << modelDriver.ToString() << " : "
              << *dirs << std::endl;

    col.GetDirectories(collection, model, &dirs);
    std::cout << collection.ToString() << ":" << model.ToString() << " : "
              << *dirs << std::endl;

    col.GetDirectories(collection, simulatorModel, &dirs);
    std::cout << collection.ToString() << ":" << simulatorModel.ToString()
              << " : " << *dirs << std::endl;
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
    col->GetEnvironmentVariableName(KIM::COLLECTION_ITEM_TYPE::model, &name);
    std::cout << KIM::COLLECTION_ITEM_TYPE::model.ToString()
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
    std::string const * filePath;
    col->GetConfigurationFilePath(&filePath);
    std::cout << "config filepath : " << *filePath << std::endl;
    std::cout << std::endl;
  }


  {
    using namespace KIM::COLLECTION;
    dirsForCollection(KIM::COLLECTION::system, *col);
    dirsForCollection(user, *col);
    dirsForCollection(environmentVariable, *col);
    dirsForCollection(currentWorkingDirectory, *col);
    std::cout << std::endl;
  }


  {
    namespace KC = KIM::COLLECTION;
    using namespace KIM::COLLECTION_ITEM_TYPE;
    std::string const * names;
    col->GetItemNamesByCollectionAndType(KC::system, modelDriver, &names);
    std::cout << KC::system.ToString() << ":" << modelDriver.ToString() << " : "
              << *names << std::endl;
    col->GetItemNamesByCollectionAndType(KC::system, model, &names);
    std::cout << KC::system.ToString() << ":" << model.ToString() << " : "
              << *names << std::endl;
    col->GetItemNamesByCollectionAndType(KC::system, simulatorModel, &names);
    std::cout << KC::system.ToString() << ":" << simulatorModel.ToString()
              << " : " << *names << std::endl;
    std::cout << std::endl;
  }

  {
    using namespace KIM::COLLECTION;
    using namespace KIM::COLLECTION_ITEM_TYPE;
    std::string const * names;
    col->GetItemNamesByCollectionAndType(user, modelDriver, &names);
    std::cout << user.ToString() << ":" << modelDriver.ToString() << " : "
              << *names << std::endl;
    col->GetItemNamesByCollectionAndType(user, model, &names);
    std::cout << user.ToString() << ":" << model.ToString() << " : " << *names
              << std::endl;
    col->GetItemNamesByCollectionAndType(user, simulatorModel, &names);
    std::cout << user.ToString() << ":" << simulatorModel.ToString() << " : "
              << *names << std::endl;
    std::cout << std::endl;
  }

  {
    using namespace KIM::COLLECTION;
    using namespace KIM::COLLECTION_ITEM_TYPE;
    std::string const * names;
    col->GetItemNamesByCollectionAndType(
        environmentVariable, modelDriver, &names);
    std::cout << environmentVariable.ToString() << ":" << modelDriver.ToString()
              << " : " << *names << std::endl;
    col->GetItemNamesByCollectionAndType(environmentVariable, model, &names);
    std::cout << environmentVariable.ToString() << ":" << model.ToString()
              << " : " << *names << std::endl;
    col->GetItemNamesByCollectionAndType(
        environmentVariable, simulatorModel, &names);
    std::cout << environmentVariable.ToString() << ":"
              << simulatorModel.ToString() << " : " << *names << std::endl;
    std::cout << std::endl;
  }

  {
    using namespace KIM::COLLECTION;
    using namespace KIM::COLLECTION_ITEM_TYPE;
    std::string const * names;
    col->GetItemNamesByCollectionAndType(
        currentWorkingDirectory, modelDriver, &names);
    std::cout << currentWorkingDirectory.ToString() << ":"
              << modelDriver.ToString() << " : " << *names << std::endl;
    col->GetItemNamesByCollectionAndType(
        currentWorkingDirectory, model, &names);
    std::cout << currentWorkingDirectory.ToString() << ":" << model.ToString()
              << " : " << *names << std::endl;
    col->GetItemNamesByCollectionAndType(
        currentWorkingDirectory, simulatorModel, &names);
    std::cout << currentWorkingDirectory.ToString() << ":"
              << simulatorModel.ToString() << " : " << *names << std::endl;
    std::cout << std::endl;
  }

  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    std::string const * names;
    col->GetItemNamesByType(modelDriver, &names);
    std::cout << modelDriver.ToString() << " : " << *names << std::endl;
    col->GetItemNamesByType(model, &names);
    std::cout << model.ToString() << " : " << *names << std::endl;
    col->GetItemNamesByType(simulatorModel, &names);
    std::cout << simulatorModel.ToString() << " : " << *names << std::endl;
    std::cout << std::endl;
  }


  return 0;
}

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
// Copyright (c) 2013--2019, Regents of the University of Minnesota.
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
#include "KIM_Log.hpp"
#include "KIM_LogVerbosity.hpp"
#include "KIM_Version.hpp"
#include <cstring>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <vector>

void usage(std::string name)
{
  size_t beg = name.find_last_of("/");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr
      << "Usage:\n"
      << "  " << name
      << " env (env | model_drivers | portable_models | simulator_models)\n"
      << "  " << name
      << " config_file (env | name | model_drivers | portable_models | "
         "simulator_models)\n"
      << "  " << name << " type <item-name>\n"
      << "  " << name << " metadata <item-name>\n"
      << "  " << name
      << " system (project | model_drivers | portable_models | "
         "simulator_models)\n"
      << "  " << name << " model_drivers [--log] [find <model-driver-name>]\n"
      << "  " << name
      << " portable_models [--log] [find <portable-model-name>]\n"
      << "  " << name
      << " simulator_models [--log] [find <simulator-model-name>]\n"
      << "  " << name << " --version\n";
  // note: this interface is likely to change in future kim-api releases
}

namespace CI
{
void printDirs(int const extent, KIM::Collections const * const col)
{
  for (int i = 0; i < extent; ++i)
  {
    std::string const * dir;
    col->GetDirectoryName(i, &dir);
    std::cout << *dir << std::endl;
  }
}

enum ENV_OPTIONS {
  E_ENV,
  E_MODEL_DRIVERS,
  E_PORTABLE_MODELS,
  E_SIMULATOR_MODELS
};
void env(ENV_OPTIONS const opt)
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::Collections * col;
  KIM::Collections::Create(&col);

  switch (opt)
  {
    case E_ENV:
    {
      using namespace KIM::COLLECTION_ITEM_TYPE;
      std::string const * envName;
      col->GetEnvironmentVariableName(modelDriver, &envName);
      std::cout << *envName << " ";

      col->GetEnvironmentVariableName(portableModel, &envName);
      std::cout << *envName << " ";

      col->GetEnvironmentVariableName(simulatorModel, &envName);
      std::cout << *envName;

      std::cout << std::endl;
      break;
    }
    case E_MODEL_DRIVERS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case E_PORTABLE_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case E_SIMULATOR_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::Collections::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

enum CONFIG_FILE_OPTIONS {
  CF_ENV,
  CF_NAME,
  CF_MODEL_DRIVERS,
  CF_PORTABLE_MODELS,
  CF_SIMULATOR_MODELS
};
void configFile(CONFIG_FILE_OPTIONS const opt)
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::Collections * col;
  KIM::Collections::Create(&col);

  switch (opt)
  {
    case CF_ENV:
    {
      std::string const * fl;
      std::string const * val;
      col->GetConfigurationFileEnvironmentVariable(&fl, &val);
      std::cout << *fl << " " << *val << std::endl;
      break;
    }
    case CF_NAME:
    {
      std::string const * dir;
      col->GetConfigurationFileName(&dir);
      std::cout << *dir << std::endl;
      break;
    }
    case CF_MODEL_DRIVERS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case CF_PORTABLE_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case CF_SIMULATOR_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::Collections::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

void project()
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::Collections * col;
  KIM::Collections::Create(&col);

  std::string const * project;
  std::string const * version;
  col->GetProjectNameAndSemVer(&project, &version);

  std::cout << *project << " " << *version << std::endl;

  KIM::Collections::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

enum SYS_OPTIONS { S_MODEL_DRIVERS, S_PORTABLE_MODELS, S_SIMULATOR_MODELS };
void sys(SYS_OPTIONS const opt)
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::Collections * col;
  KIM::Collections::Create(&col);

  switch (opt)
  {
    case S_MODEL_DRIVERS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case S_PORTABLE_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case S_SIMULATOR_MODELS:
    {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::Collections::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

void listItems(KIM::CollectionItemType const type,
               bool const list_all,
               std::string const & name,
               bool const log)
{
  if (log)
    KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::debug);
  else
    KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::Collections * col;
  int error = KIM::Collections::Create(&col);

  if (!error)
  {
    if (list_all)
    {
      std::vector<KIM::Collection> colList;
      colList.push_back(KIM::COLLECTION::currentWorkingDirectory);
      colList.push_back(KIM::COLLECTION::environmentVariable);
      colList.push_back(KIM::COLLECTION::user);
      colList.push_back(KIM::COLLECTION::system);

      for (std::vector<KIM::Collection>::size_type i = 0; i < colList.size();
           ++i)
      {
        int extent;
        error = col->CacheListOfItemNamesByCollectionAndType(
            colList[i], type, &extent);
        if (error) break;

        for (int j = 0; j < extent; ++j)
        {
          std::string const * itemName;
          col->GetItemNameByCollectionAndType(j, &itemName);
          std::string const * itemLibraryFileName;
          error = col->GetItemLibraryFileNameByCollectionAndType(
              colList[i], type, *itemName, &itemLibraryFileName);

          if (!error)
          {
            std::string const notAvailable("VERSION-NOT-AVAILABLE");
            std::string const * itemCompVer = &notAvailable;
            int mdExtent;
            col->CacheListOfItemMetadataFilesByCollectionAndType(
                colList[i], type, *itemName, &mdExtent);
            for (int k = 0; k < mdExtent; ++k)
            {
              std::string const * mdFileName;
              std::string const * mdStr;
              int asString;
              error = col->GetItemMetadataFileByCollectionAndType(
                  k, &mdFileName, NULL, NULL, &asString, &mdStr);
              if ((!error) && (*mdFileName == "item-compiled-with-version.txt")
                  && (asString))
              {
                itemCompVer = mdStr;
                break;
              }
            }

            std::size_t found = itemLibraryFileName->find_last_of("/");
            std::string dir = itemLibraryFileName->substr(0, found);
            found = dir.find_last_of("/");
            dir = dir.substr(0, found);
            std::string s(*itemCompVer);
            s.erase(s.find_last_not_of(" \n\r\t") + 1);  // rtrim
            std::cout << colList[i].ToString() << " " << *itemName << " " << dir
                      << " " << s << std::endl;
          }
        }
      }
    }
    else
    {
      std::string const * itemFileName;
      KIM::Collection collection;
      int error = col->GetItemLibraryFileNameAndCollection(
          type, name, &itemFileName, &collection);

      if (!error)
      {
        std::string const notAvailable("VERSION-NOT-AVAILABLE");
        std::string const * itemCompVer = &notAvailable;
        int extent;
        col->CacheListOfItemMetadataFilesByCollectionAndType(
            collection, type, name, &extent);
        for (int j = 0; j < extent; ++j)
        {
          std::string const * mdFileName;
          std::string const * mdStr;
          int asString;
          error = col->GetItemMetadataFileByCollectionAndType(
              j, &mdFileName, NULL, NULL, &asString, &mdStr);
          if ((!error) && (*mdFileName == "item-compiled-with-version.txt")
              && (asString))
          {
            itemCompVer = mdStr;
            break;
          }
        }
        std::size_t found = itemFileName->find_last_of("/");
        std::string dir = itemFileName->substr(0, found);
        found = dir.find_last_of("/");
        dir = dir.substr(0, found);
        std::string s(*itemCompVer);
        s.erase(s.find_last_not_of(" \n\r\t") + 1);  // rtrim
        std::cout << collection.ToString() << " " << name << " " << dir << " "
                  << s << std::endl;
      }
    }

    KIM::Collections::Destroy(&col);
  }

  KIM::Log::PopDefaultVerbosity();
}
}  // namespace CI


int processEnv(int argc, char * argv[]);
int processConfigFile(int argc, char * argv[]);
int getItemType(int argc, char * argv[]);
int getItemMetadata(int argc, char * argv[]);
int processSystem(int argc, char * argv[]);
int processItems(int argc, char * argv[]);

int main(int argc, char * argv[])
{
  int returnVal = 0;

  if (argc < 2) { returnVal = 1; }
  else
  {
    if (0 == strcmp("env", argv[1])) { returnVal = processEnv(argc, argv); }
    else if (0 == strcmp("config_file", argv[1]))
    {
      returnVal = processConfigFile(argc, argv);
    }
    else if (0 == strcmp("type", argv[1]))
    {
      returnVal = getItemType(argc, argv);
    }
    else if (0 == strcmp("metadata", argv[1]))
    {
      returnVal = getItemMetadata(argc, argv);
    }
    else if (0 == strcmp("system", argv[1]))
    {
      returnVal = processSystem(argc, argv);
    }
    else if ((0 == strcmp("model_drivers", argv[1]))
             || (0 == strcmp("portable_models", argv[1]))
             || (0 == strcmp("simulator_models", argv[1])))
    {
      returnVal = processItems(argc, argv);
    }
    else if (0 == strcmp("--version", argv[1]))
    {
      std::cout << KIM_VERSION_STRING << std::endl;
      returnVal = 0;
    }
    else
    {
      returnVal = 1;
    }
  }

  if (returnVal != 0) usage(argv[0]);
  return returnVal;
}


int processEnv(int argc, char * argv[])
{
  int returnVal = 0;
  CI::ENV_OPTIONS opt;
  if (argc != 3) { returnVal = 1; }
  else
  {
    if (0 == strcmp("env", argv[2])) { opt = CI::E_ENV; }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      opt = CI::E_MODEL_DRIVERS;
    }
    else if (0 == strcmp("portable_models", argv[2]))
    {
      opt = CI::E_PORTABLE_MODELS;
    }
    else if (0 == strcmp("simulator_models", argv[2]))
    {
      opt = CI::E_SIMULATOR_MODELS;
    }
    else
    {
      returnVal = 1;
    }
  }

  if (0 == returnVal) { CI::env(opt); }

  return returnVal;
}

int processConfigFile(int argc, char * argv[])
{
  int returnVal = 0;
  CI::CONFIG_FILE_OPTIONS opt;
  if (argc != 3) { returnVal = 1; }
  else
  {
    if (0 == strcmp("env", argv[2])) { opt = CI::CF_ENV; }
    else if (0 == strcmp("name", argv[2]))
    {
      opt = CI::CF_NAME;
    }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      opt = CI::CF_MODEL_DRIVERS;
    }
    else if (0 == strcmp("portable_models", argv[2]))
    {
      opt = CI::CF_PORTABLE_MODELS;
    }
    else if (0 == strcmp("simulator_models", argv[2]))
    {
      opt = CI::CF_SIMULATOR_MODELS;
    }
    else
    {
      returnVal = 1;
    }
  }

  if (0 == returnVal) { CI::configFile(opt); }

  return returnVal;
}

int getItemType(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 3) { returnVal = 1; }
  else
  {
    KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
    KIM::Collections * col;
    int error = KIM::Collections::Create(&col);
    if (!error)
    {
      KIM::CollectionItemType itemType;
      error = col->GetItemType(argv[2], &itemType);
      if (error)
        returnVal = 1;
      else
      {
        std::cout << itemType.ToString() << std::endl;
      }

      KIM::Collections::Destroy(&col);
    }

    KIM::Log::PopDefaultVerbosity();
  }

  return returnVal;
}

int getItemMetadata(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 3) { returnVal = 1; }
  else
  {
    KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
    KIM::Collections * col;
    int error = KIM::Collections::Create(&col);

    if (!error)
    {
      KIM::CollectionItemType itemType;
      error = col->GetItemType(argv[2], &itemType);
      if (error)
        returnVal = 1;
      else
      {
        error = col->GetItemLibraryFileNameAndCollection(
            itemType, argv[2], NULL, NULL);
        if (error)
          returnVal = 1;
        else
        {
          int extent;
          col->CacheListOfItemMetadataFiles(itemType, argv[2], &extent);
          for (int i = 0; i < extent; ++i)
          {
            std::string const * fileName;
            std::string const * data;
            unsigned int fileLength;
            int asString;
            error = col->GetItemMetadataFile(
                i, &fileName, &fileLength, NULL, &asString, &data);
            if ((error) || (!asString))
            {
              std::cout << "=== MD " << i << " === "
                        << "not available" << std::endl;
              std::cout << "======="
                        << "="
                        << "====="
                        << "=============" << std::endl;
            }
            else
            {
              std::string s(*data);
              s.erase(s.find_last_not_of(" \n\r\t") + 1);  // rtrim
              std::cout << "=== MD " << i << " === " << *fileName
                        << " === " << fileLength << " ===" << std::endl;
              std::cout << s << std::endl;
              std::cout << "======="
                        << "="
                        << "====="
                        << "=============" << std::endl;
            }
          }
        }
      }

      KIM::Collections::Destroy(&col);
    }

    KIM::Log::PopDefaultVerbosity();
  }

  return returnVal;
}

int processSystem(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 3) { returnVal = 1; }
  else
  {
    if (0 == strcmp("project", argv[2])) { CI::project(); }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      CI::sys(CI::S_MODEL_DRIVERS);
    }
    else if (0 == strcmp("portable_models", argv[2]))
    {
      CI::sys(CI::S_PORTABLE_MODELS);
    }
    else if (0 == strcmp("simulator_models", argv[2]))
    {
      CI::sys(CI::S_SIMULATOR_MODELS);
    }
    else
    {
      returnVal = 1;
    }
  }

  return returnVal;
}

int processItems(int argc, char * argv[])
{
  int returnVal = 0;
  bool list_all = true;
  std::string name;

  bool log = false;
  if (argc >= 3)
  {
    if (0 == strcmp("--log", argv[2]))
    {
      for (int i = 3; i < argc; ++i) argv[i - 1] = argv[i];
      argc--;
      log = true;
    }
  }

  if ((argc == 3) || (argc > 4)) { returnVal = 1; }
  else
  {
    if (argc == 4)
    {
      if (0 == strcmp("find", argv[2]))
      {
        list_all = false;
        name = argv[3];
      }
      else
      {
        returnVal = 1;
      }
    }
  }

  if (0 == returnVal)
  {
    using namespace KIM::COLLECTION_ITEM_TYPE;

    if (0 == strcmp("model_drivers", argv[1]))
    { CI::listItems(modelDriver, list_all, name, log); }
    else if (0 == strcmp("portable_models", argv[1]))
    {
      CI::listItems(portableModel, list_all, name, log);
    }
    else if (0 == strcmp("simulator_models", argv[1]))
    {
      CI::listItems(simulatorModel, list_all, name, log);
    }
    else
    {
      returnVal = 1;  // unknown argument
    }
  }

  return returnVal;
}

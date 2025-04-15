//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
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
// Release: This file is part of the kim-api-2.4.1 package.
//


#include "KIM_Collection.hpp"
#include "KIM_CollectionItemType.hpp"
#include "KIM_CollectionsImplementation.hpp"  // using non-public internal API
#include "KIM_FilesystemPath.hpp"  // using non-public internal API
#include "KIM_Log.hpp"
#include "KIM_LogVerbosity.hpp"
#include "KIM_Version.hpp"
#include <cstring>
#include <iostream>
#include <map>
#include <string>
#include <vector>

void usage(std::string name)
{
  size_t beg = name.find_last_of("/\\");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr
      << "Usage:\n"
      << "  " << name
      << " env (env | model_drivers | portable_models | simulator_models)\n"
      << "  " << name
      << " config_file (env | name | model_drivers | portable_models | "
         "simulator_models)\n"
      << "  " << name
      << " write_config_file <file-name> <model-drivers-dirs> "
         "<portable-models-dirs> <simulator-models-dir>\n"
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
void printDirs(int const extent,
               KIM::CollectionsImplementation const * const col)
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
  KIM::CollectionsImplementation * col;
  KIM::CollectionsImplementation::Create(&col);

  switch (opt)
  {
    case E_ENV: {
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
    case E_MODEL_DRIVERS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case E_PORTABLE_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case E_SIMULATOR_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::environmentVariable,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::CollectionsImplementation::Destroy(&col);
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
  KIM::CollectionsImplementation * col;
  KIM::CollectionsImplementation::Create(&col);

  switch (opt)
  {
    case CF_ENV: {
      std::string const * fl;
      std::string const * val;
      col->GetConfigurationFileEnvironmentVariable(&fl, &val);
      std::cout << *fl << " " << *val << std::endl;
      break;
    }
    case CF_NAME: {
      std::string const * dir;
      col->GetConfigurationFileName(&dir);
      std::cout << *dir << std::endl;
      break;
    }
    case CF_MODEL_DRIVERS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case CF_PORTABLE_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case CF_SIMULATOR_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::user,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::CollectionsImplementation::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

void project()
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::CollectionsImplementation * col;
  KIM::CollectionsImplementation::Create(&col);

  std::string const * project;
  std::string const * version;
  col->GetProjectNameAndSemVer(&project, &version);

  std::cout << *project << " " << *version << std::endl;

  KIM::CollectionsImplementation::Destroy(&col);
  KIM::Log::PopDefaultVerbosity();
}

enum SYS_OPTIONS { S_MODEL_DRIVERS, S_PORTABLE_MODELS, S_SIMULATOR_MODELS };
void sys(SYS_OPTIONS const opt)
{
  KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
  KIM::CollectionsImplementation * col;
  KIM::CollectionsImplementation::Create(&col);

  switch (opt)
  {
    case S_MODEL_DRIVERS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::modelDriver,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case S_PORTABLE_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::portableModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
    case S_SIMULATOR_MODELS: {
      int extent;
      col->CacheListOfDirectoryNames(KIM::COLLECTION::system,
                                     KIM::COLLECTION_ITEM_TYPE::simulatorModel,
                                     &extent);
      printDirs(extent, col);
      break;
    }
  }

  KIM::CollectionsImplementation::Destroy(&col);
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
  KIM::CollectionsImplementation * col;
  int error = KIM::CollectionsImplementation::Create(&col);

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
            std::size_t found = itemLibraryFileName->find_last_of("/\\");
            std::string dir = itemLibraryFileName->substr(0, found);
            found = dir.find_last_of("/\\");
            dir = dir.substr(0, found);
            std::cout << colList[i].ToString() << " " << *itemName << " " << dir
                      << std::endl;
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
        std::size_t found = itemFileName->find_last_of("/\\");
        std::string dir = itemFileName->substr(0, found);
        found = dir.find_last_of("/\\");
        dir = dir.substr(0, found);
        std::cout << collection.ToString() << " " << name << " " << dir
                  << std::endl;
      }
    }

    KIM::CollectionsImplementation::Destroy(&col);
  }

  KIM::Log::PopDefaultVerbosity();
}
}  // namespace CI


int processEnv(int argc, char * argv[]);
int processConfigFile(int argc, char * argv[]);
int writeConfigFile(int argc, char * argv[]);
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
    else if (0 == strcmp("write_config_file", argv[1]))
    {
      returnVal = writeConfigFile(argc, argv);
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
    else { returnVal = 1; }
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
    else { returnVal = 1; }
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
    else if (0 == strcmp("name", argv[2])) { opt = CI::CF_NAME; }
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
    else { returnVal = 1; }
  }

  if (0 == returnVal) { CI::configFile(opt); }

  return returnVal;
}

int writeConfigFile(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 6) { returnVal = 1; }
  else
  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    KIM::FILESYSTEM::Path fileName(argv[2]);
    std::map<KIM::CollectionItemType,
             KIM::FILESYSTEM::PathList,
             KIM::COLLECTION_ITEM_TYPE::Comparator>
        dirsMap;
    if (!dirsMap[modelDriver].Parse(argv[3])) return 1;
    if (!dirsMap[portableModel].Parse(argv[4])) return 1;
    if (!dirsMap[simulatorModel].Parse(argv[5])) return 1;

    returnVal = KIM::CollectionsImplementation::
        WriteConfigurationFileAndCreateDirectories(fileName, dirsMap);
  }

  return returnVal;
}

int getItemType(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 3) { returnVal = 1; }
  else
  {
    KIM::Log::PushDefaultVerbosity(KIM::LOG_VERBOSITY::silent);
    KIM::CollectionsImplementation * col;
    int error = KIM::CollectionsImplementation::Create(&col);
    if (!error)
    {
      KIM::CollectionItemType itemType;
      error = col->GetItemType(argv[2], &itemType);
      if (error)
        returnVal = 1;
      else { std::cout << itemType.ToString() << std::endl; }

      KIM::CollectionsImplementation::Destroy(&col);
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
    KIM::CollectionsImplementation * col;
    int error = KIM::CollectionsImplementation::Create(&col);

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

      KIM::CollectionsImplementation::Destroy(&col);
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
    else { returnVal = 1; }
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
      else { returnVal = 1; }
    }
  }

  if (0 == returnVal)
  {
    using namespace KIM::COLLECTION_ITEM_TYPE;

    if (0 == strcmp("model_drivers", argv[1]))
    {
      CI::listItems(modelDriver, list_all, name, log);
    }
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

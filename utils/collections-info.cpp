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

#include "KIM_Log.hpp"
#include "KIM_LogVerbosity.hpp"
#include "KIM_Version.hpp"
#include "old_KIM_API_DIRS.h"
#include <cstring>
#include <iostream>
#include <list>
#include <string>
#include <vector>
using namespace OLD_KIM;

void usage(std::string name)
{
  size_t beg = name.find_last_of("/");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr << "Usage:\n"
            << "  " << name
            << " env (env | model_drivers | models | simulator_models)\n"
            << "  " << name
            << " config_file (env | name | model_drivers | models | "
               "simulator_models)\n"
            << "  " << name
            << " system (library | model_drivers | models | simulator_models)\n"
            << "  " << name
            << " model_drivers [--log] [find <model-driver-name>]\n"
            << "  " << name << " models [--log] [find <model-name>]\n"
            << "  " << name
            << " simulator_models [--log] [find <simulator-model-name>]\n"
            << "  " << name << " --version\n";
  // note: this interface is likely to change in future kim-api releases
}

namespace CI
{
void listDirs(OLD_KIM::CollectionType const collection,
              OLD_KIM::CollectionItemType const type)
{
  std::list<std::pair<std::string, std::string> > lst;
  pushDirs(collection, type, &lst, NULL);
  for (std::list<std::pair<std::string, std::string> >::const_iterator itr
       = lst.begin();
       itr != lst.end();
       ++itr)
  { std::cout << itr->second << std::endl; }
}

enum ENV_OPTIONS { E_ENV, E_MODEL_DRIVERS, E_MODELS, E_SIMULATOR_MODELS };
void env(ENV_OPTIONS const opt)
{
  switch (opt)
  {
    case E_ENV:
    {
      std::map<OLD_KIM::CollectionItemType const, std::string> envVarNames;
      getEnvironmentVariableNames(&envVarNames);

      std::cout << envVarNames[KIM_MODEL_DRIVERS] << " "
                << envVarNames[KIM_MODELS] << " "
                << envVarNames[KIM_SIMULATOR_MODELS] << std::endl;
      break;
    }
    case E_MODEL_DRIVERS: listDirs(KIM_ENVIRONMENT, KIM_MODEL_DRIVERS); break;
    case E_MODELS: listDirs(KIM_ENVIRONMENT, KIM_MODELS); break;
    case E_SIMULATOR_MODELS:
      listDirs(KIM_ENVIRONMENT, KIM_SIMULATOR_MODELS);
      break;
  }
}

enum CONFIG_FILE_OPTIONS {
  CF_ENV,
  CF_NAME,
  CF_MODEL_DRIVERS,
  CF_MODELS,
  CF_SIMULATOR_MODELS
};
void configFile(CONFIG_FILE_OPTIONS const opt)
{
  switch (opt)
  {
    case CF_ENV:
    {
      std::vector<std::string> configFileName = getConfigFileName();
      std::cout << configFileName[1] << " " << configFileName[2] << std::endl;
      break;
    }
    case CF_NAME: std::cout << getConfigFileName()[0] << std::endl; break;
    case CF_MODEL_DRIVERS: listDirs(KIM_USER, KIM_MODEL_DRIVERS); break;
    case CF_MODELS: listDirs(KIM_USER, KIM_MODELS); break;
    case CF_SIMULATOR_MODELS: listDirs(KIM_USER, KIM_SIMULATOR_MODELS); break;
  }
}

enum SYS_OPTIONS { S_MODEL_DRIVERS, S_MODELS, S_SIMULATOR_MODELS };
void sys(SYS_OPTIONS const opt)
{
  switch (opt)
  {
    case S_MODEL_DRIVERS: listDirs(KIM_SYSTEM, KIM_MODEL_DRIVERS); break;
    case S_MODELS: listDirs(KIM_SYSTEM, KIM_MODELS); break;
    case S_SIMULATOR_MODELS: listDirs(KIM_SYSTEM, KIM_SIMULATOR_MODELS); break;
  }
}

void listItems(OLD_KIM::CollectionItemType const type,
               bool const list_all,
               std::string const & name,
               KIM::Log * const log)
{
  std::list<std::vector<std::string> > items;
  getAvailableItems(type, items, log);

  std::list<std::vector<std::string> >::const_iterator itr;
  for (itr = items.begin(); itr != items.end(); ++itr)
  {
    if (list_all)
    {
      std::cout << (*itr)[IE_COLLECTION] << " " << (*itr)[IE_NAME] << " "
                << (*itr)[IE_DIR] << " " << (*itr)[IE_VER] << std::endl;
    }
    else
    {
      if (name == (*itr)[1])
      {
        std::cout << (*itr)[IE_COLLECTION] << " " << (*itr)[IE_NAME] << " "
                  << (*itr)[IE_DIR] << " " << (*itr)[IE_VER] << std::endl;
        break;
      }
    }
  }
}
}  // namespace CI


int processEnv(int argc, char * argv[]);
int processConfigFile(int argc, char * argv[]);
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
    else if (0 == strcmp("system", argv[1]))
    {
      returnVal = processSystem(argc, argv);
    }
    else if ((0 == strcmp("model_drivers", argv[1]))
             || (0 == strcmp("models", argv[1]))
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
    else if (0 == strcmp("models", argv[2]))
    {
      opt = CI::E_MODELS;
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
    else if (0 == strcmp("models", argv[2]))
    {
      opt = CI::CF_MODELS;
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

int processSystem(int argc, char * argv[])
{
  int returnVal = 0;
  if (argc != 3) { returnVal = 1; }
  else
  {
    if (0 == strcmp("library", argv[2]))
    { std::cout << getSystemLibraryFileName() << std::endl; }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      CI::listDirs(KIM_SYSTEM, KIM_MODEL_DRIVERS);
    }
    else if (0 == strcmp("models", argv[2]))
    {
      CI::listDirs(KIM_SYSTEM, KIM_MODELS);
    }
    else if (0 == strcmp("simulator_models", argv[2]))
    {
      CI::listDirs(KIM_SYSTEM, KIM_SIMULATOR_MODELS);
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

  KIM::Log * log = NULL;
  if (argc >= 3)
  {
    if (0 == strcmp("--log", argv[2]))
    {
      for (int i = 3; i < argc; ++i) argv[i - 1] = argv[i];
      argc--;
      KIM::Log::Create(&log);
      log->PushVerbosity(KIM::LOG_VERBOSITY::debug);
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
    if (0 == strcmp("model_drivers", argv[1]))
    { CI::listItems(KIM_MODEL_DRIVERS, list_all, name, log); }
    else if (0 == strcmp("models", argv[1]))
    {
      CI::listItems(KIM_MODELS, list_all, name, log);
    }
    else if (0 == strcmp("simulator_models", argv[1]))
    {
      CI::listItems(KIM_SIMULATOR_MODELS, list_all, name, log);
    }
    else
    {
      returnVal = 1;  // unknown argument
    }
  }

  if (log)
  {
    KIM::Log::Destroy(&log);
    log = NULL;
  }

  return returnVal;
}

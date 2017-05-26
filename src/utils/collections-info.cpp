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
// Copyright (c) 2013--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <iostream>
#include <string>
#include <list>
#include <vector>
#include <cstring>
#include "old_KIM_API_DIRS.h"
using namespace OLD_KIM;

void usage(char const* const name)
{
  std::cerr << "usage: "
            << name
            << " <command> [<args>]\n"
            << "Where <command> is one of the below.\n"
            << "   env <models | model_drivers>\n"
            << "   config_file <name | models | model_drivers>\n"
            << "   models [find <name>]\n"
            << "   model_drivers [find <name>]\n";
  // note: this interface is likely to change in future kim-api releases
}

class collectionsInfo
{
 public:
  collectionsInfo() {};
  ~collectionsInfo() {};

  enum ENV_OPTIONS {E_MODELS, E_MODEL_DRIVERS};
  void env(ENV_OPTIONS const opt);

  enum CONFIG_FILE_OPTIONS {CF_NAME, CF_MODELS, CF_MODEL_DRIVERS};
  void configFile(CONFIG_FILE_OPTIONS const opt);

  void models(bool const list_all, std::string const& name);

  void drivers(bool const list_all, std::string const& name);
 private:
  void listItems(
      std::list<std::vector<std::string> > const& items,
      bool const list_all, std::string const& names);
};

void collectionsInfo::env(ENV_OPTIONS const opt)
{
  std::list<std::string> lst;
  switch (opt)
  {
    case E_MODELS:
      pushEnvDirs(KIM_MODELS_DIR, &lst);

      for (std::list<std::string>::const_iterator itr = lst.begin();
           itr != lst.end(); ++itr)
      {
        std::cout << *itr << std::endl;
      }
      break;
    case E_MODEL_DRIVERS:
      pushEnvDirs(KIM_MODEL_DRIVERS_DIR, &lst);
      for (std::list<std::string>::const_iterator itr = lst.begin();
           itr != lst.end(); ++itr)
      {
        std::cout << *itr << std::endl;
      }
      break;
  }
}

void collectionsInfo::configFile(CONFIG_FILE_OPTIONS const opt)
{
  std::vector<std::string> userDirs = getUserDirs();
  switch (opt)
  {
    case CF_NAME:
      std::cout << getConfigFileName() << std::endl;
      break;
    case CF_MODELS:
      std::cout << userDirs[1] << std::endl;
      break;
    case CF_MODEL_DRIVERS:
      std::cout << userDirs[0] << std::endl;
      break;
  }
}

void collectionsInfo::models(bool const list_all, std::string const& name)
{
  std::list<std::vector<std::string> > items;
  getAvailableItems(KIM_MODELS_DIR, items);
  listItems(items, list_all, name);
}

void collectionsInfo::drivers(bool list_all, std::string const& name)
{
  std::list<std::vector<std::string> > items;
  getAvailableItems(KIM_MODEL_DRIVERS_DIR, items);
  listItems(items, list_all, name);
}

void collectionsInfo::listItems(
    std::list<std::vector<std::string> > const& items,
    bool const list_all, std::string const& name)
{
  std::list<std::vector<std::string> >::const_iterator itr;
  for (itr = items.begin(); itr != items.end(); ++itr)
  {
    if (list_all)
    {
      std::cout << (*itr)[0] << " "
                << (*itr)[1] << " "
                << (*itr)[2] << std::endl;
    }
    else
    {
      if (name == (*itr)[0])
      {
        std::cout << (*itr)[0] << " "
                  << (*itr)[1] << " "
                  << (*itr)[2] << std::endl;
        break;
      }
    }
  }
}


int processEnv(int argc, char* argv[]);
int processConfigFile(int argc, char* argv[]);
int processItems(int argc, char* argv[]);

int main(int argc, char* argv[])
{
  int returnVal = 0;

  if (argc < 2)
  {
    returnVal = 1;
  }
  else
  {
    if (0 == strcmp("env", argv[1]))
    {
      returnVal = processEnv(argc, argv);
    }
    else if (0 == strcmp("config_file", argv[1]))
    {
      returnVal = processConfigFile(argc, argv);
    }
    else if ((0 == strcmp("models", argv[1])) ||
             (0 == strcmp("model_drivers", argv[1])))
    {
      returnVal = processItems(argc, argv);
    }
    else
    {
      returnVal = 1;
    }
  }

  if (returnVal != 0) usage(argv[0]);
  return returnVal;
}


int processEnv(int argc, char* argv[])
{
  int returnVal = 0;
  collectionsInfo::ENV_OPTIONS opt;
  if (argc != 3)
  {
    returnVal = 1;
  }
  else
  {
    if (0 == strcmp("models", argv[2]))
    {
      opt = collectionsInfo::E_MODELS;
    }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      opt = collectionsInfo::E_MODEL_DRIVERS;
    }
    else
    {
      returnVal = 1;
    }
  }

  if (0 == returnVal)
  {
    collectionsInfo col;
    col.env(opt);
  }

  return returnVal;
}

int processConfigFile(int argc, char* argv[])
{
  int returnVal = 0;
  collectionsInfo::CONFIG_FILE_OPTIONS opt;
  if (argc != 3)
  {
    returnVal = 1;
  }
  else
  {
    if (0 == strcmp("name", argv[2]))
    {
      opt = collectionsInfo::CF_NAME;
    }
    else if (0 == strcmp("models", argv[2]))
    {
      opt = collectionsInfo::CF_MODELS;
    }
    else if (0 == strcmp("model_drivers", argv[2]))
    {
      opt = collectionsInfo::CF_MODEL_DRIVERS;
    }
    else
    {
      returnVal = 1;
    }
  }

  if (0 == returnVal)
  {
    collectionsInfo col;
    col.configFile(opt);
  }

  return returnVal;
}

int processItems(int argc, char* argv[])
{
  int returnVal = 0;
  bool list_all = true;
  std::string name;
  if ((argc == 3) || (argc > 4))
  {
    returnVal = 1;
  }
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
    collectionsInfo col;
    if (0 == strcmp("models", argv[1]))
    {
      col.models(list_all, name);
    }
    else
    {
      col.drivers(list_all, name);
    }
  }

  return returnVal;
}

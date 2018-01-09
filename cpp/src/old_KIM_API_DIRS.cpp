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
// Copyright (c) 2013--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <dirent.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include "old_KIM_API_DIRS.h"
#include "KIM_LogVerbosity.hpp"

#define LINELEN 256

#ifndef PACKAGEDIR
#error
#endif
#ifndef KIMDIR
#error
#endif
#ifndef KIMLIBBUILD
#error
#endif
#ifndef MODELDRIVERSDIR
#error
#endif
#ifndef MODELSDIR
#error
#endif
#ifndef PACKAGENAME
#error
#endif
#ifndef USERROOT
#error
#endif
#ifndef USERCONFIGFILEROOTNAME
#error
#endif
#ifndef USERCONFIGFILEDIRNAME
#error
#endif
#ifndef VERSION_MAJOR
#error
#endif
#ifndef MODELLIBFILE
#error
#endif
#ifndef MODELDRIVERLIBFILE
#error
#endif


namespace OLD_KIM
{

void sanitizeString(std::string &str)
{
  std::string::iterator itr;
  for (itr=str.begin(); itr != str.end(); ++itr)
  {
    if (isalnum(*itr))
    {
      *itr = toupper(*itr);
    }
    else
    {
      *itr = '_';
    }
  }
}

std::vector<std::string> getConfigFileName()
{
  std::vector<std::string> configFileName(3);

  if (USERROOT)
  {
    configFileName[0] = USERCONFIGFILEROOTNAME;
  }
  else
  {
    configFileName[0] = getenv("HOME");
  }
  configFileName[0].append("/").append(USERCONFIGFILEDIRNAME);
  configFileName[0].append("/config-v").append(VERSION_MAJOR);

  std::string varName(PACKAGENAME);
  sanitizeString(varName);
  varName.append("_USER_CONFIG_FILE");
  configFileName[1] = varName;
  char const* const varVal = getenv(varName.c_str());
  if (NULL != varVal)
  {
    // ensure we have an absolute path
    if (varVal[0] != '/')
    {
      configFileName[2] = std::string(getenv("PWD"));
      configFileName[2].append("/");
      configFileName[2].append(varVal);
    }
    else
    {
      configFileName[2] = std::string(varVal);
    }
    configFileName[0] = varVal;
  }
  else
  {
    configFileName[2]=std::string("");
  }

  return configFileName;
}

std::string getSystemLibraryFileName()
{
  return std::string(PACKAGEDIR).append("/").append("lib" KIMLIBBUILD);
}

std::vector<std::string> getSystemDirs()
{
  std::vector<std::string> systemDirs(2);
  systemDirs[0] = std::string(PACKAGEDIR).append("/").append(MODELDRIVERSDIR);
  systemDirs[1] = std::string(PACKAGEDIR).append("/").append(MODELSDIR);

  return systemDirs;
}

std::vector<std::string> getUserDirs(KIM::Log * const log)
{
  std::vector<std::string> userDirs(2);
  std::vector<std::string> configFile(getConfigFileName());
  std::ifstream cfl;
  cfl.open(configFile[0].c_str(), std::ifstream::in);
  if (!cfl)
  {
    // unable to open file; create with default locations
    size_t const pos = configFile[0].find_last_of('/');
    std::string const path = configFile[0].substr(0,pos);
    std::string const name = configFile[0].substr(pos+1);
    std::ofstream fl;

    mkdir(path.c_str(), 0755);
    fl.open(configFile[0].c_str(), std::ofstream::out);
    fl << "model_drivers_dir = " << path << "/" << "model_drivers\n";
    fl << "models_dir = " << path << "/" << "models\n";
    fl.close();
    userDirs[0] = path + "/" + "model_drivers";
    mkdir(userDirs[0].c_str(), 0755);
    userDirs[1] = path + "/" + "models";
    mkdir(userDirs[1].c_str(), 0755);
  }
  else
  {
    char line[LINELEN];
    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp("model_drivers_dir", word))
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Unknown line in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        userDirs[0] = "";
        goto cleanUp;
      }
      word = strtok(NULL, sep);
      userDirs[0] = word;
      std::size_t found_home = userDirs[0].find("~/");
      std::size_t found_root = userDirs[0].find("/");
      if (found_home == 0)
      {
        userDirs[0].replace(0, 1, getenv("HOME"));
      }
      else if (found_root != 0)
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Invalid value in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        userDirs[0] = "";
        goto cleanUp;
      }
      else
      {
        // nothing to do
      }
    }

    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp("models_dir", word))
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Unknown line in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        userDirs[1] = "";
        goto cleanUp;
      }
      word = strtok(NULL, sep);
      userDirs[1] = word;
      std::size_t found_home = userDirs[1].find("~/");
      std::size_t found_root = userDirs[1].find("/");
      if (found_home == 0)
      {
        userDirs[1].replace(0, 1, getenv("HOME"));
      }
      else if (found_root != 0)
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Invalid value in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        userDirs[1] = "";
        goto cleanUp;
      }
      else
      {
        // nothing to do
      }
    }

 cleanUp:
    cfl.close();
  }

  return userDirs;
}

std::string pushEnvDirs(
    DirectoryPathType type,
    std::list<std::pair<std::string,std::string> >* const lst)
{
  std::string varName = PACKAGENAME;
  sanitizeString(varName);
  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      varName.append("_MODEL_DRIVERS_DIR");
      break;
    case KIM_MODELS_DIR:
      varName.append("_MODELS_DIR");
      break;
    default:
      break;
  }
  char const* const varVal = getenv(varName.c_str());
  if (NULL != varVal)
  {
    std::string varValString(varVal);
    std::istringstream iss(varValString);
    std::string token;
    while (std::getline(iss, token, ':'))
    {
      lst->push_back(std::make_pair(std::string("environment"),token));
    }
  }

  return varName;
}

void searchPaths(DirectoryPathType type,
                 std::list<std::pair<std::string,std::string> >* const lst,
                 KIM::Log * const log)
{
  std::vector<std::string> userDirs = getUserDirs(log);

  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::make_pair(std::string("CWD"), std::string(".")));
      pushEnvDirs(type,lst);
      if (0 != userDirs[0].compare(""))
      {
        lst->push_back(std::make_pair(std::string("user"), userDirs[0]));
      }
      lst->push_back(
          std::make_pair(
              std::string("system"),
              std::string(PACKAGEDIR).append("/").append(MODELDRIVERSDIR)));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::make_pair(std::string("CWD"), std::string(".")));
      pushEnvDirs(type,lst);
      if (0 != userDirs[1].compare(""))
      {
        lst->push_back(std::make_pair(std::string("user"), userDirs[1]));
      }
      lst->push_back(
          std::make_pair(
              std::string("system"),
              std::string(PACKAGEDIR).append("/").append(MODELSDIR)));
      break;
    default:
      break;
  }
  return;
}

void getSubDirectories(std::string const &dir, std::list<std::string> &list)
{
  list.clear();

  DIR* dirp = NULL;
  struct dirent* dp = NULL;

  if (NULL != (dirp = opendir(dir.c_str())))
  {
    do
    {
      std::string fullPath(dir);
      struct stat statBuf;
      if ((NULL != (dp = readdir(dirp))) &&
          (0 != strcmp(dp->d_name, ".")) && (0 != strcmp(dp->d_name, "..")))
      {
        fullPath.append("/").append(dp->d_name);
        if ((0 == stat(fullPath.c_str(), &statBuf)) &&
            (S_ISDIR(statBuf.st_mode)))
        {
          list.push_back(fullPath);
        }
      }
    }
    while (NULL != dp);
    closedir(dirp);
  }
}

// For sorting entries at the end of getAvailableItems
bool lessThan(std::vector<std::string> lhs, std::vector<std::string> rhs)
{
  return lhs[IE_NAME] < rhs[IE_NAME];
}

void getAvailableItems(DirectoryPathType type,
                       std::list<std::vector<std::string> > &list,
                       KIM::Log * const log)
{
  std::list<std::pair<std::string,std::string> > paths;
  searchPaths(type, &paths, log);

  std::list<std::pair<std::string,std::string> >::const_iterator itr;
  for (itr = paths.begin(); itr != paths.end(); ++itr)
  {
    std::list<std::string> items;
    getSubDirectories(itr->second, items);

    std::string collection = itr->first;
    std::list<std::string>::const_iterator itemItr;
    for (itemItr = items.begin(); itemItr != items.end(); ++itemItr)
    {
      std::vector<std::string> entry(4);
      entry[IE_COLLECTION] = collection;
      std::size_t split = itemItr->find_last_of("/");
      entry[IE_NAME] = itemItr->substr(split+1);
      entry[IE_DIR] = itemItr->substr(0,split);

      std::string lib = entry[IE_DIR] + "/" + entry[IE_NAME] + "/";
      switch (type)
      {
        case KIM_MODELS_DIR:
          lib.append(MODELLIBFILE);
          break;
        case KIM_MODEL_DRIVERS_DIR:
          lib.append(MODELDRIVERLIBFILE);
          break;
        default:
          break;
      }
      lib.append(".so");
      void* tmp_lib_handle = NULL;
      tmp_lib_handle = dlopen(lib.c_str(), RTLD_NOW);
      if (tmp_lib_handle != NULL)
      {
        std::string verSymbolName = entry[IE_NAME] + "_compiled_with_version";
        char const* const verSymbolPtr = (char const* const)
            dlsym(tmp_lib_handle, verSymbolName.c_str());
        char* dlsym_error = dlerror();
        if (dlsym_error)
        {
          if (log)
          {
            std::stringstream ss;
            ss << " Cannot load symbol: " << dlsym_error <<std::endl;
            log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                          __LINE__, __FILE__);
          }
          entry[IE_VER] = "unknown";
        }
        else
        {
          entry[IE_VER] = verSymbolPtr;
        }

        list.push_back(entry);
        dlclose(tmp_lib_handle);
      }
      else
      {
        if (log)
        {
          std::stringstream ss;
          ss << dlerror() <<std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::debug, ss,
                        __LINE__, __FILE__);
        }
      }
    }
  }

  list.sort(lessThan);
}

bool findItem(DirectoryPathType type, std::string const& name,
              std::vector<std::string>* const Item, KIM::Log * const log)
{
  bool success = false;
  std::list<std::vector<std::string> > list;
  getAvailableItems(type, list, log);

  for (std::list<std::vector<std::string> >::const_iterator
           itr = list.begin(); itr != list.end(); ++itr)
  {
    if ((*itr)[IE_NAME] == name)
    {
      *Item = *itr;
      success = true;
      break;
    }
  }

  return success;
}

}

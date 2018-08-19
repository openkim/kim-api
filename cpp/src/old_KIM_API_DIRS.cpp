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
#include <errno.h>
#include <dlfcn.h>
#include "old_KIM_API_DIRS.h"
#include "KIM_LogVerbosity.hpp"
#include "KIM_Version.hpp"
#include "KIM_Configuration.hpp"

#define LINELEN 256

// helper
#define SNUM( x ) static_cast<std::ostringstream &>(    \
    std::ostringstream() << std::dec << x).str()

namespace OLD_KIM
{

int makeDirWrapper(const char * const path, mode_t mode)
{
  if (mkdir(path, mode))
  {
    if (EEXIST == errno)
      return false;
    else
    {
      std::cerr << "Unable to make directory '" << path << "', exiting.\n";
      return true;
    }
  }
  else
    return false;
}

std::vector<std::string> getConfigFileName()
{
  std::vector<std::string> configFileName(3);

  configFileName[0] = KIM_USER_CONFIGURATION_FILE;

  if (configFileName[0][0] != '/')
  {
    // probably need a better way to get HOME
    configFileName[0] = std::string(getenv("HOME")).append("/")
        .append(configFileName[0]);
  }

  configFileName[1] = KIM_ENVIRONMENT_CONFIGURATION_FILE;
  char const* const varVal = getenv(KIM_ENVIRONMENT_CONFIGURATION_FILE);
  if (NULL != varVal)
  {
    // ensure we have an absolute path
    if (varVal[0] != '/')
    {
      // probably need a better way to get PWD
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
  return std::string(
      KIM_LIBDIR "/" KIM_SHARED_LIBRARY_PREFIX KIM_PROJECT_NAME
      "."
      KIM_VERSION_STRING KIM_SHARED_LIBRARY_SUFFIX);
}

std::vector<std::string> getSystemDirs()
{
  std::vector<std::string> systemDirs(2);
  systemDirs[0] = std::string(
      KIM_LIBDIR "/" KIM_PROJECT_NAME "/" KIM_MODEL_DRIVER_PLURAL_IDENTIFIER);
  systemDirs[1] = std::string(
      KIM_LIBDIR "/" KIM_PROJECT_NAME "/" KIM_MODEL_PLURAL_IDENTIFIER);

  return systemDirs;
}

std::string ProcessConfigFileDirectoryString(std::string const & dir)
{
  std::string returnString = dir;
  // must be absolute "/...." or home "~/..."
  std::size_t found_home = returnString.find("~/");
  std::size_t found_root = returnString.find("/");
  if (found_home == 0)
  {
    // probably need a better way to get HOME
    returnString.replace(0, 1, getenv("HOME"));
  }
  else if (found_root != 0)  // error
  {
    returnString = "";
  }
  else
  {
    // nothing to do
  }

  return returnString;  // "" indicated an error
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
    // std::string const name = configFile[0].substr(pos+1);  // NOT USED
    std::ofstream fl;

    if (makeDirWrapper(path.c_str(), 0755)) exit(1);
    userDirs[0] = ProcessConfigFileDirectoryString(
        KIM_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT);
    if (makeDirWrapper(userDirs[0].c_str(), 0755)) exit(1);
    userDirs[1] = ProcessConfigFileDirectoryString(
        KIM_USER_MODEL_PLURAL_DIR_DEFAULT);
    if (makeDirWrapper(userDirs[1].c_str(), 0755)) exit(1);

    fl.open(configFile[0].c_str(), std::ofstream::out);
    fl << KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER " = "
        KIM_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT "\n";
    fl << KIM_MODEL_PLURAL_DIR_IDENTIFIER " = "
        KIM_USER_MODEL_PLURAL_DIR_DEFAULT "\n";
    fl.close();
  }
  else
  {
    char line[LINELEN];
    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp(KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER, word))
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
      userDirs[0] = ProcessConfigFileDirectoryString(word);
      if (userDirs[0] == "")  // error
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Invalid value in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        goto cleanUp;
      }
    }

    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp(KIM_MODEL_PLURAL_DIR_IDENTIFIER, word))
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
      userDirs[1] = ProcessConfigFileDirectoryString(word);
      if (userDirs[1] == "")  // error
      {
        if (log)
        {
          std::stringstream ss;
          ss << "Invalid value in " << configFile[0] << " file: "
             << word << std::endl;
          log->LogEntry(KIM::LOG_VERBOSITY::error, ss,
                        __LINE__, __FILE__);
        }
        goto cleanUp;
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
  std::string varName;
  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      varName = KIM_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR;
      break;
    case KIM_MODELS_DIR:
      varName = KIM_ENVIRONMENT_MODEL_PLURAL_DIR;
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
  std::vector<std::string> systemDirs = getSystemDirs();

  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::make_pair(std::string("CWD"), std::string(".")));
      pushEnvDirs(type,lst);
      if (0 != userDirs[0].compare(""))
      {
        lst->push_back(std::make_pair(std::string("user"), userDirs[0]));
      }
      lst->push_back(std::make_pair(std::string("system"), systemDirs[0]));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::make_pair(std::string("CWD"), std::string(".")));
      pushEnvDirs(type,lst);
      if (0 != userDirs[1].compare(""))
      {
        lst->push_back(std::make_pair(std::string("user"), userDirs[1]));
      }
      lst->push_back(std::make_pair(std::string("system"), systemDirs[1]));
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

      std::string lib = entry[IE_DIR] + "/"
          + entry[IE_NAME]
          + "/" KIM_SHARED_MODULE_PREFIX
          + KIM_PROJECT_NAME "-";
      switch (type)
      {
        case KIM_MODELS_DIR:
          lib.append(KIM_MODEL_IDENTIFIER);
          break;
        case KIM_MODEL_DRIVERS_DIR:
          lib.append(KIM_MODEL_DRIVER_IDENTIFIER);
          break;
        default:
          break;
      }
      lib.append(KIM_SHARED_MODULE_SUFFIX);
      void* tmp_lib_handle = NULL;
      tmp_lib_handle = dlopen(lib.c_str(), RTLD_NOW);
      if (tmp_lib_handle != NULL)
      {
        std::string verSymbolName = entry[IE_NAME] + "_compiled_with_version";
        char const* const verSymbolPtr = (char const*)
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

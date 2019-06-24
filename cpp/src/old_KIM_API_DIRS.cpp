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


#include "old_KIM_API_DIRS.h"
#include "KIM_Configuration.hpp"
#include "KIM_LogVerbosity.hpp"
#include "KIM_SharedLibrary.hpp"
#include "KIM_Version.hpp"
#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <errno.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <sys/stat.h>

#define LINELEN 256

// helper
#define SNUM(x)                                                \
  static_cast<std::ostringstream const &>(std::ostringstream() \
                                          << std::dec << x)    \
      .str()

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
      std::cerr << "Unable to make directory '" << path << ".\n";
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
    configFileName[0]
        = std::string(getenv("HOME")).append("/").append(configFileName[0]);
  }

  configFileName[1] = KIM_ENVIRONMENT_CONFIGURATION_FILE;
  char const * const varVal = getenv(KIM_ENVIRONMENT_CONFIGURATION_FILE);
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
    configFileName[2] = std::string("");
  }

  return configFileName;
}

std::string getSystemLibraryFileName()
{
  return std::string(KIM_LIBDIR "/" KIM_SHARED_LIBRARY_PREFIX KIM_PROJECT_NAME
                                "."
                     + SNUM(KIM_VERSION_MAJOR) + KIM_SHARED_LIBRARY_SUFFIX);
}

std::map<CollectionItemType const, std::string> getSystemDirs()
{
  std::map<CollectionItemType const, std::string> systemDirs;
  systemDirs[KIM_MODEL_DRIVERS] = KIM_SYSTEM_MODEL_DRIVERS_DIR;
  systemDirs[KIM_PORTABLE_MODELS] = KIM_SYSTEM_PORTABLE_MODELS_DIR;
  systemDirs[KIM_SIMULATOR_MODELS] = KIM_SYSTEM_SIMULATOR_MODELS_DIR;
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

  std::size_t pos = 0;
  std::size_t colonLoc = 0;
  while ((colonLoc = returnString.find(":", pos)) != std::string::npos)
  {
    pos = colonLoc + 1;
    // must be absolute "/...." or home "~/..."
    std::size_t found_home = returnString.find("~/", pos);
    std::size_t found_root = returnString.find("/", pos);
    if (found_home == pos)
    {
      // probably need a better way to get HOME
      returnString.replace(pos, 1, getenv("HOME"));
    }
    else if (found_root != pos)  // error
    {
      returnString = "";
    }
    else
    {
      // nothing to do
    }
  }

  return returnString;  // "" indicated an error
}

int ProcessConfigFileLine(char * line,
                          std::string const & configFile,
                          char const * const identifier,
                          std::string & userDir,
                          KIM::Log * const log)
{
  char * word;
  char const * const sep = " \t=";

  word = strtok(line, sep);
  if (strcmp(identifier, word))
  {
    if (log)
    {
      std::stringstream ss;
      ss << "Unknown line in " << configFile << " file: " << word << std::endl;
      log->LogEntry(KIM::LOG_VERBOSITY::error, ss, __LINE__, __FILE__);
    }
    userDir = "";
    return true;
  }
  word = strtok(NULL, sep);
  userDir = ProcessConfigFileDirectoryString(word);
  if (userDir == "")  // error
  {
    if (log)
    {
      std::stringstream ss;
      ss << "Invalid value in " << configFile << " file: " << word << std::endl;
      log->LogEntry(KIM::LOG_VERBOSITY::error, ss, __LINE__, __FILE__);
    }
    return true;
  }

  return false;
}


int getUserDirs(
    std::map<CollectionItemType const, std::string> * const userDirs,
    KIM::Log * const log)
{
  std::vector<std::string> configFile(getConfigFileName());
  std::ifstream cfl;
  cfl.open(configFile[0].c_str(), std::ifstream::in);
  if (!cfl)
  {
    // unable to open file; create with default locations
    size_t const pos = configFile[0].find_last_of('/');
    std::string const path = configFile[0].substr(0, pos);
    // std::string const name = configFile[0].substr(pos+1);  // NOT USED
    std::ofstream fl;

    if (makeDirWrapper(path.c_str(), 0755)) return true;
    (*userDirs)[KIM_MODEL_DRIVERS] = ProcessConfigFileDirectoryString(
        KIM_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT);
    (*userDirs)[KIM_PORTABLE_MODELS] = ProcessConfigFileDirectoryString(
        KIM_USER_PORTABLE_MODEL_PLURAL_DIR_DEFAULT);
    (*userDirs)[KIM_SIMULATOR_MODELS] = ProcessConfigFileDirectoryString(
        KIM_USER_SIMULATOR_MODEL_PLURAL_DIR_DEFAULT);
    if (makeDirWrapper((*userDirs)[KIM_MODEL_DRIVERS].c_str(), 0755))
      return true;
    if (makeDirWrapper((*userDirs)[KIM_PORTABLE_MODELS].c_str(), 0755))
      return true;
    if (makeDirWrapper((*userDirs)[KIM_SIMULATOR_MODELS].c_str(), 0755))
      return true;

    fl.open(configFile[0].c_str(), std::ofstream::out);
    fl << KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER
        " = " KIM_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT "\n";
    fl << KIM_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER
        " = " KIM_USER_PORTABLE_MODEL_PLURAL_DIR_DEFAULT "\n";
    fl << KIM_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER
        " = " KIM_USER_SIMULATOR_MODEL_PLURAL_DIR_DEFAULT "\n";
    fl.close();
  }
  else
  {
    char line[LINELEN];
    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile[0],
                                  KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER,
                                  (*userDirs)[KIM_MODEL_DRIVERS],
                                  log)))
      goto cleanUp;

    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile[0],
                                  KIM_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER,
                                  (*userDirs)[KIM_PORTABLE_MODELS],
                                  log)))
      goto cleanUp;

    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile[0],
                                  KIM_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER,
                                  (*userDirs)[KIM_SIMULATOR_MODELS],
                                  log)))
      goto cleanUp;

  cleanUp:
    cfl.close();
  }

  return false;
}

int getEnvironmentVariableNames(
    std::map<CollectionItemType const, std::string> * const map)
{
  (*map)[KIM_MODEL_DRIVERS] = KIM_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR;
  (*map)[KIM_PORTABLE_MODELS] = KIM_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR;
  (*map)[KIM_SIMULATOR_MODELS] = KIM_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR;
  return 0;
}

int getDirList(CollectionType collectionType,
               CollectionItemType itemType,
               std::string * const dirList,
               KIM::Log * const log)
{
  switch (collectionType)
  {
    case KIM_CWD: *dirList = "."; break;
    case KIM_ENVIRONMENT:
    {
      std::map<CollectionItemType const, std::string> envVarNames;
      if (getEnvironmentVariableNames(&envVarNames)) return true;
      char const * const varVal = getenv(envVarNames[itemType].c_str());
      if (varVal == NULL) { *dirList = ""; }
      else
      {
        *dirList = varVal;
      }
      break;
    }
    case KIM_USER:
    {
      std::map<CollectionItemType const, std::string> userDirs;
      if (getUserDirs(&userDirs, log)) return true;
      *dirList = userDirs[itemType];
      break;
    }
    case KIM_SYSTEM:
    {
      std::map<CollectionItemType const, std::string> systemDirs
          = getSystemDirs();
      *dirList = systemDirs[itemType];
      break;
    }
  }
  return false;
}

int pushDirs(CollectionType collectionType,
             CollectionItemType itemType,
             std::list<std::pair<std::string, std::string> > * const lst,
             KIM::Log * const log)
{
  std::map<CollectionType, std::string> collectionStrings;
  collectionStrings[KIM_CWD] = "CWD";
  collectionStrings[KIM_ENVIRONMENT] = "environment";
  collectionStrings[KIM_USER] = "user";
  collectionStrings[KIM_SYSTEM] = "system";

  std::string dirList;
  if (getDirList(collectionType, itemType, &dirList, log)) return true;

  std::istringstream iss(dirList);
  std::string token;
  while (std::getline(iss, token, ':'))
  {
    lst->push_back(std::make_pair(collectionStrings[collectionType], token));
  }

  return false;
}

void searchPaths(CollectionItemType type,
                 std::list<std::pair<std::string, std::string> > * const lst,
                 KIM::Log * const log)
{
  pushDirs(KIM_CWD, type, lst, log);
  pushDirs(KIM_ENVIRONMENT, type, lst, log);
  pushDirs(KIM_USER, type, lst, log);
  pushDirs(KIM_SYSTEM, type, lst, log);

  return;
}

void getSubDirectories(std::string const & dir, std::list<std::string> & list)
{
  list.clear();

  DIR * dirp = NULL;
  struct dirent * dp = NULL;

  if (NULL != (dirp = opendir(dir.c_str())))
  {
    do
    {
      std::string fullPath(dir);
      struct stat statBuf;
      if ((NULL != (dp = readdir(dirp))) && (0 != strcmp(dp->d_name, "."))
          && (0 != strcmp(dp->d_name, "..")))
      {
        fullPath.append("/").append(dp->d_name);
        if ((0 == stat(fullPath.c_str(), &statBuf))
            && (S_ISDIR(statBuf.st_mode)))
        { list.push_back(fullPath); }
      }
    } while (NULL != dp);
    closedir(dirp);
  }
}

// For sorting entries at the end of getAvailableItems
bool lessThan(std::vector<std::string> lhs, std::vector<std::string> rhs)
{
  return lhs[IE_NAME] < rhs[IE_NAME];
}

void getAvailableItems(CollectionItemType type,
                       std::list<std::vector<std::string> > & list,
                       KIM::Log * const log)
{
  std::list<std::pair<std::string, std::string> > paths;
  searchPaths(type, &paths, log);

  std::list<std::pair<std::string, std::string> >::const_iterator itr;
  for (itr = paths.begin(); itr != paths.end(); ++itr)
  {
    std::list<std::string> items;
    getSubDirectories(itr->second, items);

    std::string collection = itr->first;
    std::list<std::string>::const_iterator itemItr;
    for (itemItr = items.begin(); itemItr != items.end(); ++itemItr)
    {
      std::vector<std::string> entry(5);
      entry[IE_COLLECTION] = collection;
      std::size_t split = itemItr->find_last_of("/");
      entry[IE_NAME] = itemItr->substr(split + 1);
      entry[IE_DIR] = itemItr->substr(0, split);

      std::string lib = entry[IE_DIR] + "/" + entry[IE_NAME]
                        + "/" KIM_SHARED_MODULE_PREFIX + KIM_PROJECT_NAME "-";
      switch (type)
      {
        case KIM_MODEL_DRIVERS: lib.append(KIM_MODEL_DRIVER_IDENTIFIER); break;
        case KIM_PORTABLE_MODELS:
          lib.append(KIM_PORTABLE_MODEL_IDENTIFIER);
          break;
        case KIM_SIMULATOR_MODELS:
          lib.append(KIM_SIMULATOR_MODEL_IDENTIFIER);
          break;
      }
      lib.append(KIM_SHARED_MODULE_SUFFIX);
      entry[IE_FULLPATH] = lib;

      KIM::SharedLibrary sharedLibrary(log);

      int error = sharedLibrary.Open(lib);

      if (!error)
      {
        std::string versionString;
        // error = sharedLibrary.GetCompiledWithVersion(&versionString);
        error = true;
        if (error) { entry[IE_VER] = "unknown"; }
        else
        {
          entry[IE_VER] = versionString;
        }

        list.push_back(entry);
        sharedLibrary.Close();
      }
    }
  }

  list.sort(lessThan);
}

bool findItem(CollectionItemType type,
              std::string const & name,
              std::vector<std::string> * const Item,
              KIM::Log * const log)
{
  bool success = false;
  std::list<std::vector<std::string> > list;
  getAvailableItems(type, list, log);

  for (std::list<std::vector<std::string> >::const_iterator itr = list.begin();
       itr != list.end();
       ++itr)
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

}  // namespace OLD_KIM

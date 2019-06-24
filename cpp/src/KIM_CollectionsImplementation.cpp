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


#include "KIM_Configuration.hpp"
#include "KIM_Version.hpp"
#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <sys/stat.h>

#define LINELEN 256

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_COLLECTIONS_IMPLEMENTATION_HPP_
#include "KIM_CollectionsImplementation.hpp"
#endif

// log helpers
#define SNUM(x)                                                \
  static_cast<std::ostringstream const &>(std::ostringstream() \
                                          << std::dec << x)    \
      .str()
#define SPTR(x)                                                            \
  static_cast<std::ostringstream const &>(std::ostringstream()             \
                                          << static_cast<void const *>(x)) \
      .str()
#define SFUNC(x)                                                    \
  static_cast<std::ostringstream const &>(                          \
      std::ostringstream() << reinterpret_cast<KIM::Function *>(x)) \
      .str()


namespace
{
typedef std::map<KIM::CollectionItemType,
                 std::string,
                 KIM::COLLECTION_ITEM_TYPE::Comparator>
    ItemTypeToStringMap;

std::string LibraryName(KIM::CollectionItemType const itemType,
                        std::string const & path,
                        std::string const & name)
{
  std::string libName(path);

  libName += "/" + name + "/" KIM_SHARED_MODULE_PREFIX + KIM_PROJECT_NAME + "-";

  {
    using namespace KIM::COLLECTION_ITEM_TYPE;
    if (itemType == modelDriver) { libName += KIM_MODEL_DRIVER_IDENTIFIER; }
    else if (itemType == portableModel)
    {
      libName += KIM_PORTABLE_MODEL_IDENTIFIER;
    }
    else if (itemType == simulatorModel)
    {
      libName += KIM_SIMULATOR_MODEL_IDENTIFIER;
    }
    else
    {
      libName += "UNKNOWN-COLLECTION-ITEM-TYPE";
    }
  }

  libName += KIM_SHARED_MODULE_SUFFIX;

  return libName;
}

void ParseColonSeparatedList(std::string const & listString,
                             std::list<std::string> & lst)
{
  std::istringstream iss(listString);
  std::string token;
  while (std::getline(iss, token, ':')) { lst.push_back(token); }
}

std::string CreateColonSeparatedStringFromList(std::list<std::string> const lst)
{
  std::string finalList;

  std::string separator("");
  std::list<std::string>::const_iterator itr;
  for (itr = lst.begin(); itr != lst.end(); ++itr)
  {
    finalList += separator + *itr;
    separator = ":";
  }

  return finalList;
}

void GetSubDirectories(std::string const & dir, std::list<std::string> & list)
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
        { list.push_back(dp->d_name); }
      }
    } while (NULL != dp);
    closedir(dirp);
  }
}

int MakeDirWrapper(std::string const & path, mode_t mode)
{
  if (mkdir(path.c_str(), mode))
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
                          KIM::Log * const log,
                          std::string & userDir)
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

void PrivateGetConfigurationFileEnvironmentVariable(std::string & name,
                                                    std::string & value)
{
  name = KIM_ENVIRONMENT_CONFIGURATION_FILE;

  char const * const varVal = getenv(name.c_str());
  value = std::string("");
  if (NULL != varVal) value = varVal;
}

void PrivateGetConfigurationFilePath(std::string & filePath)
{
  filePath = KIM_USER_CONFIGURATION_FILE;

  if (filePath[0] != '/')
  {
    // probably need a better way to get HOME
    filePath = std::string(getenv("HOME")).append("/").append(filePath);
  }

  std::string varName;
  std::string varVal;
  PrivateGetConfigurationFileEnvironmentVariable(varName, varVal);
  if (varVal != "")
  {
    // ensure we have an absolute path
    if (varVal[0] != '/')
    {
      // probably need a better way to get PWD
      filePath = std::string(getenv("PWD")).append("/").append(varVal);
    }
    else
    {
      filePath = varVal;
    }
  }
}

void PrivateGetEnvironmentVariableName(KIM::CollectionItemType const itemType,
                                       std::string & name)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  if (itemType == modelDriver)
  { name = KIM_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR; }
  else if (itemType == portableModel)
  {
    name = KIM_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR;
  }
  else if (itemType == simulatorModel)
  {
    name = KIM_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR;
  }
}

void PrivateGetProjectNameAndSemVer(std::string & projectName,
                                    std::string & semVer)
{
  projectName = KIM_PROJECT_NAME;
  semVer = KIM_VERSION_STRING;
}

void PrivateGetCWDDirs(ItemTypeToStringMap & dirsMap)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  dirsMap[modelDriver] = ".";
  dirsMap[portableModel] = ".";
  dirsMap[simulatorModel] = ".";
}

void PrivateGetEnvironmentDirs(ItemTypeToStringMap & dirsMap)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;
  {
    char const * const varVal = getenv(KIM_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR);
    if (varVal == NULL) { dirsMap[modelDriver] = ""; }
    else
    {
      dirsMap[modelDriver] = varVal;
    }
  }

  {
    char const * const varVal
        = getenv(KIM_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR);
    if (varVal == NULL) { dirsMap[portableModel] = ""; }
    else
    {
      dirsMap[portableModel] = varVal;
    }
  }

  {
    char const * const varVal
        = getenv(KIM_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR);
    if (varVal == NULL) { dirsMap[simulatorModel] = ""; }
    else
    {
      dirsMap[simulatorModel] = varVal;
    }
  }
}

int PrivateGetUserDirs(KIM::Log * log, ItemTypeToStringMap & dirsMap)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  std::string configFile;
  PrivateGetConfigurationFilePath(configFile);

  std::ifstream cfl;
  cfl.open(configFile.c_str(), std::ifstream::in);
  if (!cfl)
  {
    // unable to open file; create with default locations
    size_t const pos = configFile.find_last_of('/');
    std::string const path = configFile.substr(0, pos);
    // std::string const name = configFile->substr(pos+1);  // NOT USED
    std::ofstream fl;

    if (MakeDirWrapper(path.c_str(), 0755)) return true;
    dirsMap[modelDriver] = ProcessConfigFileDirectoryString(
        KIM_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT);
    dirsMap[portableModel] = ProcessConfigFileDirectoryString(
        KIM_USER_PORTABLE_MODEL_PLURAL_DIR_DEFAULT);
    dirsMap[simulatorModel] = ProcessConfigFileDirectoryString(
        KIM_USER_SIMULATOR_MODEL_PLURAL_DIR_DEFAULT);
    if (MakeDirWrapper(dirsMap[modelDriver].c_str(), 0755)) return true;
    if (MakeDirWrapper(dirsMap[portableModel].c_str(), 0755)) return true;
    if (MakeDirWrapper(dirsMap[simulatorModel].c_str(), 0755)) return true;

    fl.open(configFile.c_str(), std::ofstream::out);
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
    std::string val;
    char line[LINELEN];
    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile,
                                  KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER,
                                  log,
                                  val)))
      goto cleanUp;
    dirsMap[modelDriver] = val;

    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile,
                                  KIM_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER,
                                  log,
                                  val)))
      goto cleanUp;
    dirsMap[portableModel] = val;

    if ((!cfl.getline(line, LINELEN))
        || (ProcessConfigFileLine(line,
                                  configFile,
                                  KIM_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER,
                                  log,
                                  val)))
      goto cleanUp;
    dirsMap[simulatorModel] = val;

  cleanUp:
    cfl.close();
  }

  return false;
}

void PrivateGetSystemDirs(ItemTypeToStringMap & dirsMap)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  dirsMap[modelDriver] = KIM_SYSTEM_MODEL_DRIVERS_DIR;
  dirsMap[portableModel] = KIM_SYSTEM_PORTABLE_MODELS_DIR;
  dirsMap[simulatorModel] = KIM_SYSTEM_SIMULATOR_MODELS_DIR;
}

void PrivateGetListOfItemNamesByCollectionAndType(
    KIM::Collection const collection,
    KIM::CollectionItemType const itemType,
    KIM::Log * log,
    std::list<std::string> & names)
{
  ItemTypeToStringMap dirsMap;
  if (collection == KIM::COLLECTION::system)
    PrivateGetSystemDirs(dirsMap);
  else if (collection == KIM::COLLECTION::user)
    PrivateGetUserDirs(log, dirsMap);
  else if (collection == KIM::COLLECTION::environmentVariable)
    PrivateGetEnvironmentDirs(dirsMap);
  else if (collection == KIM::COLLECTION::currentWorkingDirectory)
    PrivateGetCWDDirs(dirsMap);
  std::list<std::string> listOfDirs;
  ParseColonSeparatedList(dirsMap[itemType], listOfDirs);

  std::list<std::string>::const_iterator dir;
  for (dir = listOfDirs.begin(); dir != listOfDirs.end(); ++dir)
  {
    std::list<std::string> subDirs;
    GetSubDirectories(*dir, subDirs);
    subDirs.sort();

    std::list<std::string>::const_iterator subDir;
    for (subDir = subDirs.begin(); subDir != subDirs.end(); ++subDir)
    {
      std::string const libName(LibraryName(itemType, *dir, *subDir));

      KIM::SharedLibrary sharedLibrary(log);
      int error = sharedLibrary.Open(libName);
      if (!error) { names.push_back(*subDir); }
    }
  }
}

int PrivateGetItemByCollectionAndType(KIM::Collection const collection,
                                      KIM::CollectionItemType const itemType,
                                      std::string const & itemName,
                                      KIM::Log * log,
                                      std::string * path,
                                      int * metadataExtent)
{
  namespace KC = KIM::COLLECTION;

  ItemTypeToStringMap dirsMap;
  if (collection == KC::system)
    PrivateGetSystemDirs(dirsMap);
  else if (collection == KC::user)
    PrivateGetUserDirs(log, dirsMap);
  else if (collection == KC::environmentVariable)
    PrivateGetEnvironmentDirs(dirsMap);
  else if (collection == KC::currentWorkingDirectory)
    PrivateGetCWDDirs(dirsMap);
  std::list<std::string> listOfDirs;
  ParseColonSeparatedList(dirsMap[itemType], listOfDirs);

  std::string libPath;
  KIM::SharedLibrary lib(log);
  std::list<std::string>::const_iterator dir;
  for (dir = listOfDirs.begin(); dir != listOfDirs.end(); ++dir)
  {
    int error = lib.Open(libPath = LibraryName(itemType, *dir, itemName));
    if (!error) break;
    libPath = "";
  }

  if (libPath == "") { return true; }

  if (path) *path = libPath;
  // currently libraries do not have any metadata files.
  if (metadataExtent) *metadataExtent = 0;

  return false;
}

int PrivateGetItem(KIM::CollectionItemType const itemType,
                   std::string const & itemName,
                   KIM::Log * const log,
                   std::string * const path,
                   int * const metadataExtent,
                   KIM::Collection * const collection)
{
  namespace KC = KIM::COLLECTION;

  std::string itemPath;
  int extent;
  KIM::Collection col;
  if (!PrivateGetItemByCollectionAndType(KC::currentWorkingDirectory,
                                         itemType,
                                         itemName,
                                         log,
                                         &itemPath,
                                         &extent))
  { col = KC::currentWorkingDirectory; }
  else if (!PrivateGetItemByCollectionAndType(KC::environmentVariable,
                                              itemType,
                                              itemName,
                                              log,
                                              &itemPath,
                                              &extent))
  {
    col = KC::environmentVariable;
  }
  else if (!PrivateGetItemByCollectionAndType(
               KC::user, itemType, itemName, log, &itemPath, &extent))
  {
    col = KC::user;
  }
  else if (!PrivateGetItemByCollectionAndType(
               KC::system, itemType, itemName, log, &itemPath, &extent))
  {
    col = KC::system;
  }
  else
  {
    return true;
  }

  if (path) *path = itemPath;
  if (metadataExtent) *metadataExtent = extent;
  if (collection) *collection = col;

  return false;
}  // namespace

int PrivateGetTypeOfItem(std::string const & itemName,
                         KIM::Log * const log,
                         KIM::CollectionItemType * const typeOfItem)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  KIM::CollectionItemType itemType;
  if (!PrivateGetItem(portableModel, itemName, log, NULL, NULL, NULL))
  { itemType = portableModel; }
  else if (!PrivateGetItem(simulatorModel, itemName, log, NULL, NULL, NULL))
  {
    itemType = simulatorModel;
  }
  else if (!PrivateGetItem(modelDriver, itemName, log, NULL, NULL, NULL))
  {
    itemType = modelDriver;
  }
  else
  {
    return true;
  }

  *typeOfItem = itemType;

  return false;
}
}  // namespace


#include "KIM_LogMacros.hpp"
#define KIM_LOGGER_OBJECT_NAME this
namespace KIM
{
int CollectionsImplementation::Create(
    CollectionsImplementation ** const collectionsImplementation)
{
  // error checking of arguments performed as part of CollectionsCreate()

  Log * pLog;
  int error = Log::Create(&pLog);
  if (error) { return true; }

  CollectionsImplementation * pCollectionsImplementation;
  pCollectionsImplementation = new CollectionsImplementation(pLog);
#if DEBUG_VERBOSITY
  std::string const callString
      = "Create(" + SPTR(collectionsImplementation) + ").";
  pCollectionsImplementation->LogEntry(
      LOG_VERBOSITY::debug,
      "Created Log and CollectionsImplementation objects after enter "
          + callString,
      __LINE__,
      __FILE__);
#endif

  *collectionsImplementation = pCollectionsImplementation;
#if DEBUG_VERBOSITY
  (*collectionsImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Exit 0=" + callString, __LINE__, __FILE__);
#endif
  return false;
}

void CollectionsImplementation::Destroy(
    CollectionsImplementation ** const collectionsImplementation)
{
#if DEBUG_VERBOSITY
  std::string callString = "Destroy(" + SPTR(collectionsImplementation) + ").";
  (*collectionsImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Enter  " + callString, __LINE__, __FILE__);
#endif

#if DEBUG_VERBOSITY
  (*collectionsImplementation)
      ->LogEntry(LOG_VERBOSITY::debug,
                 "Destroying CollectionsImplementation object and exit "
                     + callString,
                 __LINE__,
                 __FILE__);
#endif
  delete *collectionsImplementation;  // also deletes Log object
  *collectionsImplementation = NULL;
}

int CollectionsImplementation::GetTypeOfItem(
    std::string const & itemName, CollectionItemType * const typeOfItem) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetTypeOfItem(\"" + itemName + "\", " + SPTR(typeOfItem) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  CollectionItemType itemType;
  if (PrivateGetTypeOfItem(itemName, log_, &itemType))
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *typeOfItem = itemType;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItem(CollectionItemType const itemType,
                                       std::string const & itemName,
                                       std::string const ** const path,
                                       int * const metadataExtent,
                                       Collection * const collection) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetItem(" + itemType.ToString() + ", \""
                                 + itemName + "\", " + SPTR(path) + ", "
                                 + SPTR(metadataExtent) + ", "
                                 + SPTR(collection) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  int extent;
  Collection col;
  if (PrivateGetItem(itemType, itemName, log_, &getItemPath_, &extent, &col))
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (path) *path = &getItemPath_;
  if (metadataExtent) *metadataExtent = extent;
  if (collection) *collection = col;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemMetadata(
    CollectionItemType const itemType,
    std::string const & itemName,
    int const index,
    std::string const ** const metadataID,
    int * const metadataLength,
    unsigned char const ** const metadataRawData,
    int * const availableAsString,
    std::string const ** const metadataString) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemMetadata(" + itemType.ToString() + ", \"" + itemName + "\", "
        + SNUM(index) + ", " + SPTR(metadataID) + ", " + SPTR(metadataLength)
        + ", " + SPTR(metadataRawData) + ", " + SPTR(availableAsString) + ", "
        + SPTR(metadataString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  //@ @ @;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemNamesByType(
    CollectionItemType const itemType,
    std::string const ** const itemNames) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetItemNamesByType(" + itemType.ToString()
                                 + ", " + SPTR(itemNames) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::list<std::string> listOfNames;
  std::list<std::string> colListOfNames;
  {
    namespace KC = KIM::COLLECTION;
    PrivateGetListOfItemNamesByCollectionAndType(
        KC::system, itemType, log_, colListOfNames);
    listOfNames.merge(colListOfNames);

    PrivateGetListOfItemNamesByCollectionAndType(
        KC::user, itemType, log_, colListOfNames);
    listOfNames.merge(colListOfNames);

    PrivateGetListOfItemNamesByCollectionAndType(
        KC::environmentVariable, itemType, log_, colListOfNames);
    listOfNames.merge(colListOfNames);

    PrivateGetListOfItemNamesByCollectionAndType(
        KC::currentWorkingDirectory, itemType, log_, colListOfNames);
    listOfNames.merge(colListOfNames);
  }
  listOfNames.unique();

  getItemNamesByType_ = CreateColonSeparatedStringFromList(listOfNames);
  *itemNames = &getItemNamesByType_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemNamesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const ** const itemNames) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemNamesByCollectionAndType(" + collection.ToString() + ", "
        + itemType.ToString() + ", " + SPTR(itemNames) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::list<std::string> listOfNames;
  PrivateGetListOfItemNamesByCollectionAndType(
      collection, itemType, log_, listOfNames);

  getItemNamesByCollectionAndType_
      = CreateColonSeparatedStringFromList(listOfNames);
  *itemNames = &getItemNamesByCollectionAndType_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    std::string const ** const path,
    int * const metadataExtent) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemByCollectionAndType(" + collection.ToString() + ", "
        + itemType.ToString() + ", \"" + itemName + "\", " + SPTR(path) + ", "
        + SPTR(metadataExtent) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((!collection.Known()) || (!itemType.Known()))
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int extent;
  int error
      = PrivateGetItemByCollectionAndType(collection,
                                          itemType,
                                          itemName,
                                          log_,
                                          &getItemByCollectionAndTypePath_,
                                          &extent);
  if (error)
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (path) *path = &getItemByCollectionAndTypePath_;
  if (metadataExtent) *metadataExtent = extent;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemMetadataByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    int const index,
    std::string const ** const metadataID,
    int * const metadataLength,
    unsigned char const ** const metadataRawData,
    int * const availableAsString,
    std::string const ** const metadataString) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemMetadataByCollectionAndType(" + collection.ToString() + ", "
        + itemType.ToString() + ", \"" + itemName + "\", " + SNUM(index) + ", "
        + SPTR(metadataID) + ", " + SPTR(metadataLength) + ", "
        + SPTR(metadataRawData) + ", " + SPTR(availableAsString) + ", "
        + SPTR(metadataString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  // @ @ @;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void CollectionsImplementation::GetProjectNameAndSemVer(
    std::string const ** const projectName,
    std::string const ** const semVer) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetProjectNameAndSemVer(" + SPTR(projectName)
                                 + ", " + SPTR(semVer) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  PrivateGetProjectNameAndSemVer(getProjectNameAndSemVerProjectName_,
                                 getProjectNameAndSemVerSemVer_);

  if (projectName) *projectName = &getProjectNameAndSemVerProjectName_;
  if (semVer) *semVer = &getProjectNameAndSemVerSemVer_;

  LOG_DEBUG("Exit   " + callString);
}

int CollectionsImplementation::GetEnvironmentVariableName(
    CollectionItemType const itemType, std::string const ** const name) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetEnvironmentVariableName("
                                 + itemType.ToString() + ", " + SPTR(name)
                                 + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (!itemType.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  PrivateGetEnvironmentVariableName(itemType, getEnvironmentVariableName_);

  *name = &getEnvironmentVariableName_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void CollectionsImplementation::GetConfigurationFileEnvironmentVariable(
    std::string const ** const name, std::string const ** const value) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetConfigurationFileEnvironmentVariable("
                                 + SPTR(name) + ", " + SPTR(value) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  PrivateGetConfigurationFileEnvironmentVariable(
      getConfigurationFileEnvironmentVariableName_,
      getConfigurationFileEnvironmentVariableValue_);

  if (name) *name = &getConfigurationFileEnvironmentVariableName_;
  if (value) *value = &getConfigurationFileEnvironmentVariableValue_;

  LOG_DEBUG("Exit   " + callString);
}

void CollectionsImplementation::GetConfigurationFilePath(
    std::string const ** const filePath) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetConfigurationFilePath(" + SPTR(filePath) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  PrivateGetConfigurationFilePath(getConfigurationFilePath_);

  *filePath = &getConfigurationFilePath_;

  LOG_DEBUG("Exit   " + callString);
}

int CollectionsImplementation::GetDirectories(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const ** const directories) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetDirectories(" + collection.ToString()
                                 + ", " + itemType.ToString() + ", "
                                 + SPTR(directories) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  ItemTypeToStringMap dirsMap;
  if (!collection.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else if (collection == COLLECTION::system)
  {
    PrivateGetSystemDirs(dirsMap);
  }
  else if (collection == COLLECTION::user)
  {
    if (PrivateGetUserDirs(log_, dirsMap))
    {
      LOG_ERROR("Unable to update user collection directories.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }
  else if (collection == COLLECTION::environmentVariable)
  {
    PrivateGetEnvironmentDirs(dirsMap);
  }
  else if (collection == COLLECTION::currentWorkingDirectory)
  {
    PrivateGetCWDDirs(dirsMap);
  }

  if (!itemType.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    getDirectories_ = dirsMap[itemType];
  }

  *directories = &getDirectories_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void CollectionsImplementation::SetLogID(std::string const & logID)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetLogID('" + logID + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->SetID(logID);

  LOG_DEBUG("Exit   " + callString);
}

void CollectionsImplementation::PushLogVerbosity(
    LogVerbosity const logVerbosity)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "PushLogVerbosity(" + logVerbosity.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->PushVerbosity(logVerbosity);

  LOG_DEBUG("Exit   " + callString);
}

void CollectionsImplementation::PopLogVerbosity()
{
#if DEBUG_VERBOSITY
  std::string const callString = "PopLogVerbosity().";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->PopVerbosity();

  LOG_DEBUG("Exit   " + callString);
}

void CollectionsImplementation::LogEntry(LogVerbosity const logVerbosity,
                                         std::string const & message,
                                         int const lineNumber,
                                         std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void CollectionsImplementation::LogEntry(LogVerbosity const logVerbosity,
                                         std::stringstream const & message,
                                         int const lineNumber,
                                         std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

CollectionsImplementation::CollectionsImplementation(Log * const log) :
    log_(log)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "CollectionsImplementation(" + SPTR(log) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  // nothing to do

  LOG_DEBUG("Exit   " + callString);
}

CollectionsImplementation::~CollectionsImplementation()
{
#if DEBUG_VERBOSITY
  std::string const callString = "~CollectionsImplementation().";
#endif
  LOG_DEBUG("Enter  " + callString);

  // nothing to do

  LOG_DEBUG("Destroying Log object and exit " + callString);
  Log::Destroy(&log_);
}
}  // namespace KIM

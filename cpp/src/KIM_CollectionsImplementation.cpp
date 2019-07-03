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
#include <errno.h>
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

int ProcessConfigFileLine(char const * const line,
                          std::string const & configFile,
                          char const * const identifier,
                          KIM::Log * const log,
                          std::string & userDir)
{
  char linecpy[LINELEN];
  char * word;
  char const * const sep = " \t=";

  strncpy(linecpy, line, LINELEN - 1);
  linecpy[LINELEN - 1] = '\0';

  word = strtok(linecpy, sep);
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

void PrivateGetConfigurationFileName(std::string & fileName)
{
  fileName = KIM_USER_CONFIGURATION_FILE;

  if (fileName[0] != '/')
  {
    // probably need a better way to get HOME
    fileName = std::string(getenv("HOME")).append("/").append(fileName);
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
      fileName = std::string(getenv("PWD")).append("/").append(varVal);
    }
    else
    {
      fileName = varVal;
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
  PrivateGetConfigurationFileName(configFile);

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
    cfl.getline(line, LINELEN);
    if ((cfl.fail())
        || (ProcessConfigFileLine(line,
                                  configFile,
                                  KIM_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER,
                                  log,
                                  val)))
    {
      cfl.close();
      return true;
    }
    else
      dirsMap[modelDriver] = val;

    cfl.getline(line, LINELEN);
    if (!cfl.fail())
    {
      if (ProcessConfigFileLine(line,
                                configFile,
                                KIM_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER,
                                log,
                                val))
      {
        if (ProcessConfigFileLine(
                line,
                configFile,
                "models-dir",  // Accept old format for this line
                log,
                val))
        {
          cfl.close();
          return true;
        }
      }
    }
    else
    {
      cfl.close();
      return true;
    }

    dirsMap[portableModel] = val;

    cfl.getline(line, LINELEN);
    if ((cfl.fail())
        || (ProcessConfigFileLine(line,
                                  configFile,
                                  KIM_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER,
                                  log,
                                  val)))
    {
      cfl.close();
      return true;
    }
    else
      dirsMap[simulatorModel] = val;

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
  names.clear();

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

int PrivateGetItemLibraryFileNameByCollectionAndType(
    KIM::Collection const collection,
    KIM::CollectionItemType const itemType,
    std::string const & itemName,
    KIM::Log * log,
    std::string * fileName)
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

  if (fileName) *fileName = libPath;

  return false;
}

int PrivateGetListOfItemMetadataFilesByCollectionAndType(
    KIM::Collection const collection,
    KIM::CollectionItemType const itemType,
    std::string const & itemName,
    KIM::Log * const log,
    std::vector<std::string> & fileNames,
    std::vector<int> & availableAsStrings,
    std::vector<std::string> & fileStrings)
{
  fileNames.clear();
  availableAsStrings.clear();
  fileStrings.clear();

  std::string path;
  int error = PrivateGetItemLibraryFileNameByCollectionAndType(
      collection, itemType, itemName, log, &path);

  if (error) { return true; }

  fileNames.clear();
  availableAsStrings.clear();
  fileStrings.clear();

  KIM::SharedLibrary lib(log);
  error = lib.Open(path);
  if (error) { return true; }
  int extent;
  error = lib.GetNumberOfMetadataFiles(&extent);
  if (error) { return true; }

  for (int i = 0; i < extent; ++i)
  {
    std::string flnm;
    unsigned int length;
    unsigned char const * data;
    error = lib.GetMetadataFile(i, &flnm, &length, &data);
    if (error) { return true; }

    fileNames.push_back(flnm);
    fileStrings.push_back("");
    fileStrings.rbegin()->assign(reinterpret_cast<char const *>(data), length);
    availableAsStrings.push_back(
        (strlen(fileStrings.rbegin()->c_str()) == length) ? true : false);
  }

  return false;
}

int PrivateGetItemLibraryFileNameAndCollection(
    KIM::CollectionItemType const itemType,
    std::string const & itemName,
    KIM::Log * const log,
    std::string * const fileName,
    KIM::Collection * const collection)
{
  namespace KC = KIM::COLLECTION;

  std::string itemPath;
  KIM::Collection col;
  if (!PrivateGetItemLibraryFileNameByCollectionAndType(
          KC::currentWorkingDirectory, itemType, itemName, log, &itemPath))
  { col = KC::currentWorkingDirectory; }
  else if (!PrivateGetItemLibraryFileNameByCollectionAndType(
               KC::environmentVariable, itemType, itemName, log, &itemPath))
  {
    col = KC::environmentVariable;
  }
  else if (!PrivateGetItemLibraryFileNameByCollectionAndType(
               KC::user, itemType, itemName, log, &itemPath))
  {
    col = KC::user;
  }
  else if (!PrivateGetItemLibraryFileNameByCollectionAndType(
               KC::system, itemType, itemName, log, &itemPath))
  {
    col = KC::system;
  }
  else
  {
    return true;
  }

  if (fileName) *fileName = itemPath;
  if (collection) *collection = col;

  return false;
}  // namespace

int PrivateGetListOfItemMetadataFiles(KIM::CollectionItemType const itemType,
                                      std::string const & itemName,
                                      KIM::Log * const log,
                                      std::vector<std::string> & fileNames,
                                      std::vector<int> & availableAsStrings,
                                      std::vector<std::string> & fileStrings)
{
  fileNames.clear();
  availableAsStrings.clear();
  fileStrings.clear();

  KIM::Collection collection;
  int error = PrivateGetItemLibraryFileNameAndCollection(
      itemType, itemName, log, NULL, &collection);

  if (error) { return true; }

  return PrivateGetListOfItemMetadataFilesByCollectionAndType(
      collection,
      itemType,
      itemName,
      log,
      fileNames,
      availableAsStrings,
      fileStrings);
}

int PrivateGetItemType(std::string const & itemName,
                       KIM::Log * const log,
                       KIM::CollectionItemType * const itemType)
{
  using namespace KIM::COLLECTION_ITEM_TYPE;

  KIM::CollectionItemType it;
  if (!PrivateGetItemLibraryFileNameAndCollection(
          portableModel, itemName, log, NULL, NULL))
  { it = portableModel; }
  else if (!PrivateGetItemLibraryFileNameAndCollection(
               simulatorModel, itemName, log, NULL, NULL))
  {
    it = simulatorModel;
  }
  else if (!PrivateGetItemLibraryFileNameAndCollection(
               modelDriver, itemName, log, NULL, NULL))
  {
    it = modelDriver;
  }
  else
  {
    return true;
  }

  *itemType = it;

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

int CollectionsImplementation::GetItemType(
    std::string const & itemName, CollectionItemType * const itemType) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemType(\"" + itemName + "\", " + SPTR(itemType) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  CollectionItemType it;
  if (PrivateGetItemType(itemName, log_, &it))
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *itemType = it;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemLibraryFileNameAndCollection(
    CollectionItemType const itemType,
    std::string const & itemName,
    std::string const ** const fileName,
    Collection * const collection) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemLibraryFileNameAndCollection(" + itemType.ToString() + ", \""
        + itemName + "\", " + SPTR(fileName) + ", " + SPTR(collection) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (!itemType.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  Collection col;
  if (PrivateGetItemLibraryFileNameAndCollection(
          itemType,
          itemName,
          log_,
          &getItemLibraryFileNameAndCollection_FileName_,
          &col))
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (fileName) *fileName = &getItemLibraryFileNameAndCollection_FileName_;
  if (collection) *collection = col;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::CacheListOfItemMetadataFiles(
    CollectionItemType const itemType,
    std::string const & itemName,
    int * const extent)
{
#if DEBUG_VERBOSITY
  std::string const callString = "CacheListOfItemMetadataFiles("
                                 + itemType.ToString() + ", \"" + itemName
                                 + "\", " + SPTR(extent) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *extent = 0;
  cacheListOfItemMetadataFiles_Names_.clear();
  cacheListOfItemMetadataFiles_availableAsString_.clear();
  cacheListOfItemMetadataFiles_RawData_.clear();

#if ERROR_VERBOSITY
  if (!itemType.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  int error = PrivateGetListOfItemMetadataFiles(
      itemType,
      itemName,
      log_,
      cacheListOfItemMetadataFiles_Names_,
      cacheListOfItemMetadataFiles_availableAsString_,
      cacheListOfItemMetadataFiles_RawData_);

  if (error)
  {
    LOG_ERROR("Unable to cache item metadata files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *extent = cacheListOfItemMetadataFiles_Names_.size();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemMetadataFile(
    int const index,
    std::string const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    std::string const ** const fileString) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemMetadataFile(" + SNUM(index) + ", " + SPTR(fileName) + ", "
        + SPTR(fileLength) + ", " + SPTR(fileRawData) + ", "
        + SPTR(availableAsString) + ", " + SPTR(fileString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((index < 0)
      || (size_t(index) > cacheListOfItemMetadataFiles_Names_.size()))
  {
    LOG_ERROR("Invalid metadata file index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (fileName) *fileName = &(cacheListOfItemMetadataFiles_Names_[index]);
  if (fileLength)
    *fileLength = (cacheListOfItemMetadataFiles_RawData_[index].length() - 1);
  if (fileRawData)
    *fileRawData = reinterpret_cast<unsigned char const *>(
        cacheListOfItemMetadataFiles_RawData_[index].c_str());
  if (availableAsString)
    *availableAsString = cacheListOfItemMetadataFiles_availableAsString_[index];
  if (fileString)
  {
    if (cacheListOfItemMetadataFiles_availableAsString_[index])
      *fileString = &(cacheListOfItemMetadataFiles_RawData_[index]);
    else
      *fileString = NULL;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::CacheListOfItemNamesByType(
    CollectionItemType const itemType, int * const extent)
{
#if DEBUG_VERBOSITY
  std::string const callString = "CacheListOfItemNamesByType("
                                 + itemType.ToString() + ", " + SPTR(extent)
                                 + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *extent = 0;
  cacheListOfItemNamesByType_.clear();

#if ERROR_VERBOSITY
  if (!itemType.Known())
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

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
  cacheListOfItemNamesByType_.assign(listOfNames.begin(), listOfNames.end());

  *extent = cacheListOfItemNamesByType_.size();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemNameByType(
    int const index, std::string const ** const itemName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemNameByType(" + SNUM(index) + ", " + SPTR(itemName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((index < 0) || (size_t(index) > cacheListOfItemNamesByType_.size()))
  {
    LOG_ERROR("Invalid item name index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *itemName = &(cacheListOfItemNamesByType_[index]);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::CacheListOfItemNamesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    int * const extent)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "CacheListOfItemNamesByCollectionAndType(" + collection.ToString()
        + ", " + itemType.ToString() + ", " + SPTR(extent) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *extent = 0;
  cacheListOfItemNamesByCollectionAndType_.clear();

#if ERROR_VERBOSITY
  if ((!collection.Known()) || (!itemType.Known()))
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  std::list<std::string> lst;
  PrivateGetListOfItemNamesByCollectionAndType(collection, itemType, log_, lst);
  cacheListOfItemNamesByCollectionAndType_.assign(lst.begin(), lst.end());

  *extent = cacheListOfItemNamesByCollectionAndType_.size();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemNameByCollectionAndType(
    int const index, std::string const ** const itemName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetItemNameByCollectionAndType(" + SNUM(index)
                                 + ", " + SPTR(itemName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((index < 0)
      || (size_t(index) > cacheListOfItemNamesByCollectionAndType_.size()))
  {
    LOG_ERROR("Invalid item name index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *itemName = &(cacheListOfItemNamesByCollectionAndType_[index]);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemLibraryFileNameByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    std::string const ** const fileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetItemLibraryFileNameByCollectionAndType("
                                 + collection.ToString() + ", "
                                 + itemType.ToString() + ", \"" + itemName
                                 + "\", " + SPTR(fileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((!collection.Known()) || (!itemType.Known()))
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  int error = PrivateGetItemLibraryFileNameByCollectionAndType(
      collection,
      itemType,
      itemName,
      log_,
      &getItemLibraryFileNameByCollectionAndType_);
  if (error)
  {
    LOG_ERROR("Unable to find item.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *fileName = &getItemLibraryFileNameByCollectionAndType_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::CacheListOfItemMetadataFilesByCollectionAndType(
    Collection const collection,
    CollectionItemType const itemType,
    std::string const & itemName,
    int * const extent)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "CacheListOfItemMetadataFilesByCollectionAndType("
        + collection.ToString() + ", " + itemType.ToString() + ", \"" + itemName
        + "\", " + SPTR(extent) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *extent = 0;
  cacheListOfItemMetadataFilesByCollectionAndType_FileNames_.clear();
  cacheListOfItemMetadataFilesByCollectionAndType_AvailableAsString_.clear();
  cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_.clear();

#if ERROR_VERBOSITY
  if ((!collection.Known()) || (!itemType.Known()))
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  int error = PrivateGetListOfItemMetadataFilesByCollectionAndType(
      collection,
      itemType,
      itemName,
      log_,
      cacheListOfItemMetadataFilesByCollectionAndType_FileNames_,
      cacheListOfItemMetadataFilesByCollectionAndType_AvailableAsString_,
      cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_);

  if (error)
  {
    LOG_ERROR("Unable to cache item metadata files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *extent = cacheListOfItemMetadataFilesByCollectionAndType_FileNames_.size();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetItemMetadataFileByCollectionAndType(
    int const index,
    std::string const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    std::string const ** const fileString) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetItemMetadataFileByCollectionAndType(" + SNUM(index) + ", "
        + SPTR(fileName) + ", " + SPTR(fileLength) + ", " + SPTR(fileRawData)
        + ", " + SPTR(availableAsString) + ", " + SPTR(fileString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((index < 0)
      || (size_t(index)
          > cacheListOfItemMetadataFilesByCollectionAndType_FileNames_.size()))
  {
    LOG_ERROR("Invalid metadata file index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (fileName)
    *fileName
        = &(cacheListOfItemMetadataFilesByCollectionAndType_FileNames_[index]);
  if (fileLength)
    *fileLength
        = (cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_[index]
               .length()
           - 1);
  if (fileRawData)
    *fileRawData = reinterpret_cast<unsigned char const *>(
        cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_[index]
            .c_str());
  if (availableAsString)
    *availableAsString
        = cacheListOfItemMetadataFilesByCollectionAndType_AvailableAsString_
            [index];
  if ((*availableAsString) && (fileString))
    *fileString = &(
        cacheListOfItemMetadataFilesByCollectionAndType_FileRawData_[index]);

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

  PrivateGetProjectNameAndSemVer(getProjectNameAndSemVer_ProjectName_,
                                 getProjectNameAndSemVer_SemVer_);

  if (projectName) *projectName = &getProjectNameAndSemVer_ProjectName_;
  if (semVer) *semVer = &getProjectNameAndSemVer_SemVer_;

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
      getConfigurationFileEnvironmentVariable_Name_,
      getConfigurationFileEnvironmentVariable_Value_);

  if (name) *name = &getConfigurationFileEnvironmentVariable_Name_;
  if (value) *value = &getConfigurationFileEnvironmentVariable_Value_;

  LOG_DEBUG("Exit   " + callString);
}

void CollectionsImplementation::GetConfigurationFileName(
    std::string const ** const fileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetConfigurationFileName(" + SPTR(fileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  PrivateGetConfigurationFileName(getConfigurationFileName_);

  *fileName = &getConfigurationFileName_;

  LOG_DEBUG("Exit   " + callString);
}

int CollectionsImplementation::CacheListOfDirectoryNames(
    Collection const collection,
    CollectionItemType const itemType,
    int * const extent)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "CacheListOfDirectoryNames(" + collection.ToString() + ", "
        + itemType.ToString() + ", " + SPTR(extent) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *extent = 0;
  cacheListOfDirectoryNames_.clear();

#if ERROR_VERBOSITY
  if ((!collection.Known()) || (!itemType.Known()))
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  ItemTypeToStringMap dirsMap;
  if (collection == COLLECTION::system) { PrivateGetSystemDirs(dirsMap); }
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

  std::list<std::string> lst;
  ParseColonSeparatedList(dirsMap[itemType], lst);
  cacheListOfDirectoryNames_.assign(lst.begin(), lst.end());

  *extent = cacheListOfDirectoryNames_.size();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int CollectionsImplementation::GetDirectoryName(
    int const index, std::string const ** const directoryName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetDirectoryName(" + SNUM(index) + ", " + SPTR(directoryName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if ((index < 0) || (size_t(index) > cacheListOfDirectoryNames_.size()))
  {
    LOG_ERROR("Invalid directory index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *directoryName = &(cacheListOfDirectoryNames_[index]);

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

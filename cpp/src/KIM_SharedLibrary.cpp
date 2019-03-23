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

#include <dlfcn.h>
#include <sstream>

#ifndef KIM_SHARED_LIBRARY_HPP_
#include "KIM_SharedLibrary.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_SHARED_LIBRARY_SCHEMA_HPP_
#include "KIM_SharedLibrarySchema.hpp"
#endif

namespace KIM
{
// log helpers
#define SNUM(x) \
  static_cast<std::ostringstream &>(std::ostringstream() << std::dec << x).str()
#define SPTR(x)                                                      \
  static_cast<std::ostringstream &>(std::ostringstream()             \
                                    << static_cast<void const *>(x)) \
      .str()
#define SFUNCP(x)                                                   \
  static_cast<std::ostringstream &>(std::ostringstream()            \
                                    << static_cast<Function **>(x)) \
      .str()

#include "KIM_LogMacros.hpp"
#define KIM_LOGGER_OBJECT_NAME this
SharedLibrary::SharedLibrary(Log * const log) :
    sharedLibraryHandle_(NULL),
    sharedLibrarySchemaVersion_(NULL),
    sharedLibrarySchema_(NULL),
    log_(log)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SharedLibrary(" + SPTR(log) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  LOG_DEBUG("Exit   " + callString);
}

SharedLibrary::~SharedLibrary()
{
#if DEBUG_VERBOSITY
  std::string const callString = "~SharedLibrary().";
#endif
  LOG_DEBUG("Enter  " + callString);

  Close();

  LOG_DEBUG("Exit   " + callString);
}

int SharedLibrary::Open(std::string const & sharedLibraryName)
{
#if DEBUG_VERBOSITY
  std::string const callString = "Open('" + sharedLibraryName + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ != NULL)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // already open
  }

  sharedLibraryName_ = sharedLibraryName;

  sharedLibraryHandle_ = dlopen(sharedLibraryName_.c_str(), RTLD_NOW);
  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Unable to open '" + sharedLibraryName_ + "'.");
    LOG_ERROR(dlerror());
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }


  sharedLibrarySchemaVersion_ = reinterpret_cast<int const *>(
      dlsym(sharedLibraryHandle_, "kim_shared_library_schema_version"));
  if (sharedLibrarySchemaVersion_ == NULL)
  {
    LOG_ERROR(dlerror());
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    sharedLibrarySchema_ = reinterpret_cast<
        SHARED_LIBRARY_SCHEMA::SharedLibrarySchemaV1 const *>(
        dlsym(sharedLibraryHandle_, "kim_shared_library_schema"));
    if (sharedLibrarySchema_ == NULL)
    {
      LOG_ERROR(dlerror());
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::Close()
{
#if DEBUG_VERBOSITY
  std::string const callString = "Close().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("SharedLibrary not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  sharedLibraryName_ = "";
  int error = dlclose(sharedLibraryHandle_);
  if (error)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    sharedLibraryHandle_ = NULL;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetName(std::string * const name) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetName(" + SPTR(name) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *name = sharedLibrarySchema_->itemName;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetType(ITEM_TYPE * const type) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetType(" + SPTR(type) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *type = SIMULATOR_MODEL;  // dummy value
  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    using namespace KIM::SHARED_LIBRARY_SCHEMA;
    SharedLibrarySchemaV1::ITEM_TYPE const KIM_ItemType
        = sharedLibrarySchema_->itemType;

    if (KIM_ItemType == SharedLibrarySchemaV1::STAND_ALONE_MODEL)
    { *type = STAND_ALONE_MODEL; }
    else if (KIM_ItemType == SharedLibrarySchemaV1::PARAMETERIZED_MODEL)
    {
      *type = PARAMETERIZED_MODEL;
    }
    else if (KIM_ItemType == SharedLibrarySchemaV1::SIMULATOR_MODEL)
    {
      *type = SIMULATOR_MODEL;
    }
    else if (KIM_ItemType == SharedLibrarySchemaV1::MODEL_DRIVER)
    {
      *type = MODEL_DRIVER;
    }
    else
    {
      LOG_ERROR("SHOULD NEVER GET HERE.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetCreateFunctionPointer(
    LanguageName * const languageName, Function ** const functionPointer) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetCreateFunctionPointer("
                                 + SPTR(languageName) + ", "
                                 + SFUNCP(functionPointer) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    *languageName = sharedLibrarySchema_->createLanguageName;
    *functionPointer = sharedLibrarySchema_->createRoutine;
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfParameterFiles(" + SPTR(numberOfParameterFiles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  { *numberOfParameterFiles = sharedLibrarySchema_->numberOfParameterFiles; }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetParameterFile(
    int const index,
    std::string * const parameterFileName,
    unsigned int * const parameterFileLength,
    unsigned char const ** const parameterFileData) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterFile(" + SNUM(index) + ", "
                                 + SPTR(parameterFileName) + ", "
                                 + SPTR(parameterFileLength) + ", "
                                 + SPTR(parameterFileData) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  ITEM_TYPE itemType;
  GetType(&itemType);
  if ((itemType != PARAMETERIZED_MODEL) && (itemType != SIMULATOR_MODEL))
  {
    LOG_ERROR("This item type does not have parameter files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int numberOfParameterFiles;
  GetNumberOfParameterFiles(&numberOfParameterFiles);
  if ((index < 0) || index >= numberOfParameterFiles)
  {
    LOG_ERROR("Invalid parameter file index.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    if (parameterFileName)
      *parameterFileName
          = (sharedLibrarySchema_->parameterFiles[index]).fileName;
    if (parameterFileLength)
      *parameterFileLength
          = (sharedLibrarySchema_->parameterFiles[index]).fileLength;
    if (parameterFileData)
      *parameterFileData
          = (sharedLibrarySchema_->parameterFiles[index]).filePointer;
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetMetadataFile(
    std::string * const metadataFileName,
    unsigned int * const metadataFileLength,
    unsigned char const ** const metadataFileData) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetMetadataFile(, " + SPTR(metadataFileName)
                                 + ", " + SPTR(metadataFileLength) + ", "
                                 + SPTR(metadataFileData) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  ITEM_TYPE itemType;
  GetType(&itemType);
  if (itemType != SIMULATOR_MODEL)
  {
    LOG_ERROR("This item type does not have a metadata file.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    if (metadataFileName)
      *metadataFileName = (sharedLibrarySchema_->metadataFile)->fileName;
    if (metadataFileLength)
      *metadataFileLength = (sharedLibrarySchema_->metadataFile)->fileLength;
    if (metadataFileData)
      *metadataFileData = (sharedLibrarySchema_->metadataFile)->filePointer;
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetDriverName(std::string * const driverName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetDriverName(" + SPTR(driverName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  {
    ITEM_TYPE itemType;
    GetType(&itemType);
    if (itemType != PARAMETERIZED_MODEL)
    {
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }

    *driverName = sharedLibrarySchema_->driverName;
  }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetCompiledWithVersion(
    std::string * const versionString) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetCompiledWithVersion(" + SPTR(versionString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (*sharedLibrarySchemaVersion_ == 1)
  { *versionString = sharedLibrarySchema_->compiledWithVersion; }
  else
  {
    LOG_ERROR("Unknown KIM::SharedLibrarySchema version.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 1=" + callString);
  return false;
}

void SharedLibrary::LogEntry(LogVerbosity const logVerbosity,
                             std::string const & message,
                             int const lineNumber,
                             std::string const & fileName) const
{
  if (log_) log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

}  // namespace KIM

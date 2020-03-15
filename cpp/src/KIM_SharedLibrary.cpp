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

#include <cstring>
#include <dlfcn.h>
#include <sstream>

#ifndef KIM_SHARED_LIBRARY_HPP_
#include "KIM_SharedLibrary.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_SHARED_LIBRARY_SCHEMA_HPP_
#include "KIM_SharedLibrarySchema.hpp"
#endif

namespace KIM
{
// log helpers
#define SNUM(x)                                                \
  static_cast<std::ostringstream const &>(std::ostringstream() \
                                          << std::dec << x)    \
      .str()
#define SPTR(x)                                                            \
  static_cast<std::ostringstream const &>(std::ostringstream()             \
                                          << static_cast<void const *>(x)) \
      .str()
#define SFUNCP(x)                                                         \
  static_cast<std::ostringstream const &>(std::ostringstream()            \
                                          << static_cast<Function **>(x)) \
      .str()

#include "KIM_LogMacros.hpp"
#define KIM_LOGGER_OBJECT_NAME this
SharedLibrary::SharedLibrary::EmbeddedFile::EmbeddedFile() :
    fileName(NULL), fileLength(0), filePointer(NULL)
{
}

SharedLibrary::SharedLibrary(Log * const log) :
    sharedLibraryHandle_(NULL),
    sharedLibrarySchemaVersion_(NULL),
    itemName_(""),
    createRoutine_(NULL),
    numberOfParameterFiles_(0),
    numberOfMetadataFiles_(0),
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


  if (*sharedLibrarySchemaVersion_ == 2)
  {
    using namespace SHARED_LIBRARY_SCHEMA;
    SharedLibrarySchemaV2 const * const schemaV2
        = reinterpret_cast<SharedLibrarySchemaV2 const *>(
            dlsym(sharedLibraryHandle_, "kim_shared_library_schema"));
    if (schemaV2 == NULL)
    {
      LOG_ERROR(dlerror());
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }

    itemType_ = schemaV2->itemType;
    createLanguageName_ = schemaV2->createLanguageName;
    createRoutine_ = schemaV2->createRoutine;
    driverName_ = ((schemaV2->driverName) ? schemaV2->driverName : "");

    if (schemaV2->simulatorModelSpecificationFile != NULL)
    {
      simulatorModelSpecificationFile_.fileName
          = schemaV2->simulatorModelSpecificationFile->fileName;
      simulatorModelSpecificationFile_.fileLength
          = schemaV2->simulatorModelSpecificationFile->fileLength;
      simulatorModelSpecificationFile_.filePointer
          = schemaV2->simulatorModelSpecificationFile->filePointer;
    }

    numberOfParameterFiles_ = schemaV2->numberOfParameterFiles;
    for (int i = 0; i < numberOfParameterFiles_; ++i)
    {
      EmbeddedFile fl;
      fl.fileName = schemaV2->parameterFiles[i].fileName;
      fl.fileLength = schemaV2->parameterFiles[i].fileLength;
      fl.filePointer = schemaV2->parameterFiles[i].filePointer;

      parameterFiles_.push_back(fl);
    }
    numberOfMetadataFiles_ = schemaV2->numberOfMetadataFiles;
    for (int i = 0; i < numberOfMetadataFiles_; ++i)
    {
      EmbeddedFile fl;
      fl.fileName = schemaV2->metadataFiles[i].fileName;
      fl.fileLength = schemaV2->metadataFiles[i].fileLength;
      fl.filePointer = schemaV2->metadataFiles[i].filePointer;

      metadataFiles_.push_back(fl);
    }
  }
  else if (*sharedLibrarySchemaVersion_ == 1)
  {
    using namespace SHARED_LIBRARY_SCHEMA;
    SharedLibrarySchemaV1 const * const schemaV1
        = reinterpret_cast<SharedLibrarySchemaV1 const *>(
            dlsym(sharedLibraryHandle_, "kim_shared_library_schema"));
    if (schemaV1 == NULL)
    {
      LOG_ERROR(dlerror());
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }

    if (schemaV1->itemType == SharedLibrarySchemaV1::STAND_ALONE_MODEL)
    { itemType_ = COLLECTION_ITEM_TYPE::portableModel; }
    else if (schemaV1->itemType == SharedLibrarySchemaV1::PARAMETERIZED_MODEL)
    {
      itemType_ = COLLECTION_ITEM_TYPE::portableModel;
      // differentiated from above by driverName_
    }
    else if (schemaV1->itemType == SharedLibrarySchemaV1::SIMULATOR_MODEL)
    {
      itemType_ = COLLECTION_ITEM_TYPE::simulatorModel;
    }
    else if (schemaV1->itemType == SharedLibrarySchemaV1::MODEL_DRIVER)
    {
      itemType_ = COLLECTION_ITEM_TYPE::modelDriver;
    }
    else
    {
      LOG_ERROR("SHOULD NEVER GET HERE.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }

    createLanguageName_ = schemaV1->createLanguageName;
    createRoutine_ = schemaV1->createRoutine;
    driverName_ = ((schemaV1->driverName) ? schemaV1->driverName : "");

    if (schemaV1->metadataFile != NULL)
    {
      simulatorModelSpecificationFile_.fileName
          = schemaV1->metadataFile->fileName;
      simulatorModelSpecificationFile_.fileLength
          = schemaV1->metadataFile->fileLength;
      simulatorModelSpecificationFile_.filePointer
          = schemaV1->metadataFile->filePointer;
    }

    numberOfParameterFiles_ = schemaV1->numberOfParameterFiles;
    for (int i = 0; i < numberOfParameterFiles_; ++i)
    {
      EmbeddedFile fl;
      fl.fileName = schemaV1->parameterFiles[i].fileName;
      fl.fileLength = schemaV1->parameterFiles[i].fileLength;
      fl.filePointer = schemaV1->parameterFiles[i].filePointer;

      parameterFiles_.push_back(fl);
    }
    numberOfMetadataFiles_ = 1;
    EmbeddedFile fl;
    fl.fileName = "compiled-with-version.txt";
    fl.fileLength = strlen(schemaV1->compiledWithVersion);
    fl.filePointer = reinterpret_cast<unsigned char const *>(
        schemaV1->compiledWithVersion);
    metadataFiles_.push_back(fl);
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
  sharedLibrarySchemaVersion_ = 0;
  itemName_ = "";
  createRoutine_ = NULL;
  driverName_ = "";
  simulatorModelSpecificationFile_.fileName = NULL;
  simulatorModelSpecificationFile_.fileLength = 0;
  simulatorModelSpecificationFile_.filePointer = NULL;
  numberOfParameterFiles_ = 0;
  parameterFiles_.clear();
  numberOfMetadataFiles_ = 0;
  metadataFiles_.clear();
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

  *name = itemName_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetType(CollectionItemType * const type) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetType(" + SPTR(type) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  *type = itemType_;

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

  if (languageName != NULL) *languageName = createLanguageName_;
  if (functionPointer != NULL) *functionPointer = createRoutine_;

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

  *numberOfParameterFiles = numberOfParameterFiles_;

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

  if (((itemType_ == COLLECTION_ITEM_TYPE::portableModel)
       && (driverName_ == ""))
      || (itemType_ == COLLECTION_ITEM_TYPE::modelDriver))
  {
    LOG_ERROR("This item type does not have parameter files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((index < 0) || index >= numberOfParameterFiles_)
  {
    LOG_ERROR("Invalid parameter file index.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (parameterFileName != NULL)
    *parameterFileName = (parameterFiles_[index]).fileName;
  if (parameterFileLength != NULL)
    *parameterFileLength = (parameterFiles_[index]).fileLength;
  if (parameterFileData != NULL)
    *parameterFileData = (parameterFiles_[index]).filePointer;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetNumberOfMetadataFiles(
    int * const numberOfMetadataFiles) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfMetadataFiles(" + SPTR(numberOfMetadataFiles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  *numberOfMetadataFiles = numberOfMetadataFiles_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetMetadataFile(
    int const index,
    std::string * const metadataFileName,
    unsigned int * const metadataFileLength,
    unsigned char const ** const metadataFileData) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetMetadataFile(" + SNUM(index) + ", " + SPTR(metadataFileName) + ", "
        + SPTR(metadataFileLength) + ", " + SPTR(metadataFileData) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if ((index < 0) || index >= numberOfMetadataFiles_)
  {
    LOG_ERROR("Invalid metadata file index.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (metadataFileName != NULL)
    *metadataFileName = (metadataFiles_[index]).fileName;
  if (metadataFileLength != NULL)
    *metadataFileLength = (metadataFiles_[index]).fileLength;
  if (metadataFileData != NULL)
    *metadataFileData = (metadataFiles_[index]).filePointer;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SharedLibrary::GetSimulatorModelSpecificationFile(
    std::string * const specFileName,
    unsigned int * const specFileLength,
    unsigned char const ** const specFileData) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSimulatorModelSpecificationFile(, " + SPTR(specFileName) + ", "
        + SPTR(specFileLength) + ", " + SPTR(specFileData) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (sharedLibraryHandle_ == NULL)
  {
    LOG_ERROR("Library not open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  if (itemType_ != COLLECTION_ITEM_TYPE::simulatorModel)
  {
    LOG_ERROR(
        "This item type does not have a simulator model specification file.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (specFileName != NULL)
    *specFileName = (simulatorModelSpecificationFile_).fileName;
  if (specFileLength != NULL)
    *specFileLength = (simulatorModelSpecificationFile_).fileLength;
  if (specFileData != NULL)
    *specFileData = (simulatorModelSpecificationFile_).filePointer;

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

  if ((itemType_ != COLLECTION_ITEM_TYPE::portableModel) && (driverName_ != ""))
  {
    LOG_ERROR("This item type does not have an associated  model driver.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *driverName = driverName_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}  // namespace KIM

void SharedLibrary::LogEntry(LogVerbosity const logVerbosity,
                             std::string const & message,
                             int const lineNumber,
                             std::string const & fileName) const
{
  if (log_ != NULL) log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

}  // namespace KIM

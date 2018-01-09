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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <iostream>
#include <vector>
#include <sstream>

#include <dlfcn.h>

#include "old_KIM_API_DIRS.h"

#ifndef KIM_MODEL_LIBRARY_HPP_
#include "KIM_ModelLibrary.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef MODELLIBFILE
#error
#endif
#ifndef MODELDRIVERLIBFILE
#error
#endif

namespace KIM
{

#include "KIM_ModelLibraryLogMacros.hpp"
ModelLibrary::ModelLibrary(Log * const log) :
    libraryHandle_(0),
    log_(log)
{
  LOG_DEBUG("Enter ModelLibrary().");
  LOG_DEBUG("Exit ModelLibrary().");
}

ModelLibrary::~ModelLibrary()
{
  LOG_DEBUG("Enter ~ModelLibrary().");

  Close();

  LOG_DEBUG("Exit ~ModelLibrary().");
}

int ModelLibrary::Open(bool const typeIsModel, std::string const & modelName)
{
  LOG_DEBUG("Enter Open().");

  if (libraryHandle_ != 0) return true;  // already open


  modelName_ = modelName;

  std::vector<std::string> item;
  bool accessible = findItem(
      (typeIsModel ? OLD_KIM::KIM_MODELS_DIR : OLD_KIM::KIM_MODEL_DRIVERS_DIR),
      modelName_, &item, log_);
  if (!accessible) return true;  // cannot find modelName
  libraryPath_ = item[OLD_KIM::IE_DIR] + "/" + item[OLD_KIM::IE_NAME] + "/"
      + (typeIsModel ? MODELLIBFILE : MODELDRIVERLIBFILE) + ".so";

  libraryHandle_ = dlopen(libraryPath_.c_str(), RTLD_NOW);
  if (libraryHandle_ == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit Open().");
    return true;
  }

  LOG_DEBUG("Exit Open().");
  return false;
}

int ModelLibrary::Close()
{
  LOG_DEBUG("Enter Close().");

  if (libraryHandle_ == 0) return true;  // not open

  modelName_ = "";
  int error = dlclose(libraryHandle_);
  if (error)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit Close().");
    return true;
  }
  else
  {
    libraryHandle_ = 0;
  }

  LOG_DEBUG("Exit Close().");
  return false;
}

int ModelLibrary::GetModelType(ITEM_TYPE * const modelType) const
{
  LOG_DEBUG("Enter GetModelType().");

  *modelType = SIMULATOR_MODEL;  // dummy value
  if (libraryHandle_ == 0) return true;  // not open

  char const * const KIM_ItemType
      = static_cast<char const * const>(dlsym(libraryHandle_, "kim_item_type"));
  if (KIM_ItemType == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit GetModelType().");
    return true;
  }

  if (std::string(KIM_ItemType) == "stand-alone-model")
    *modelType = STAND_ALONE_MODEL;
  else if (std::string(KIM_ItemType) == "parameterized-model")
    *modelType = PARAMETERIZED_MODEL;
  else if (std::string(KIM_ItemType) == "simulator-model")
    *modelType = SIMULATOR_MODEL;
  else if (std::string(KIM_ItemType) == "model-driver")
    *modelType = MODEL_DRIVER;
  else
  {
    std::cout << "unknown kim_item_type" << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit GetModelType().");
    return true;
  }

  LOG_DEBUG("Exit GetModelType().");
  return false;
}

int ModelLibrary::GetModelCreateFunctionPointer(
    LanguageName * const languageName, func ** const functionPointer) const
{
  LOG_DEBUG("Enter GetModelCreateFunctionPointer().");

  if (libraryHandle_ == 0) return true;  // not open

  std::string languageSymbol(modelName_ + "_language");
  char const * const languageNameString
      = reinterpret_cast<char const *>(dlsym(libraryHandle_,
                                             languageSymbol.c_str()));
  if (languageNameString == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetModelCreateFunctionPointer().");
    return true;
  }
  else
  {
    *languageName = LanguageName(std::string(languageNameString));
  }

  std::string createFunctionSymbol(modelName_ + "_create_pointer");
  func ** pointerToFunctionPointer
      = reinterpret_cast<func **>(dlsym(libraryHandle_,
                                        createFunctionSymbol.c_str()));

  if (pointerToFunctionPointer == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetModelCreateFunctionPointer().");
    return true;
  }

  *functionPointer = *(pointerToFunctionPointer);
  LOG_DEBUG("Exit GetModelCreateFunctionPointer().");
  return false;
}

int ModelLibrary::GetNumberOfParameterFiles(int * const numberOfParameterFiles)
    const
{
  LOG_DEBUG("Enter GetNumberOfParameterFiles().");

  *numberOfParameterFiles = 0;  // default value
  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL) return true;

  int const * const numParamFiles = static_cast<int const * const>(
      dlsym(libraryHandle_, "number_of_parameter_files"));
  if (numParamFiles == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetNumberOfParameterFiles().");
    return true;
  }

  *numberOfParameterFiles = *numParamFiles;

  LOG_DEBUG("Exit GetNumberOfParameterFiles().");
  return false;
}

int ModelLibrary::GetParameterFileString(
    int const index,
    unsigned int * const parameterFileStringLength,
    unsigned char const ** const parameterFileString) const
{
  LOG_DEBUG("Enter GetParameterFileString().");

  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL)
  {
    LOG_DEBUG("Exit GetParameterFileString().");
    return true;
  }

  int numberOfParameterFiles;
  GetNumberOfParameterFiles(&numberOfParameterFiles);
  if ((index < 0) || index >= numberOfParameterFiles)
  {
    LOG_DEBUG("Exit GetParameterFileString().");
    return true;
  }

  std::stringstream paramFileStringSymbol;
  //@@@@@@ should we make the file numbering in Makefile start from zero? @@@@
  paramFileStringSymbol << "parameter_file_" << (index+1);
  unsigned char const * const paramFileString
      = static_cast<unsigned char const * const>(
          dlsym(libraryHandle_, paramFileStringSymbol.str().c_str()));
  if (paramFileString == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetParameterFileString().");
    return true;
  }
  std::stringstream paramFileStringLengthSymbol;
  paramFileStringLengthSymbol << paramFileStringSymbol.str() << "_len";
  unsigned int const * const paramFileStringLength
      = static_cast<unsigned int const *>(
          dlsym(libraryHandle_, paramFileStringLengthSymbol.str().c_str()));
  if (paramFileStringLength == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetParameterFileString().");
    return true;
  }

  *parameterFileString = paramFileString;
  *parameterFileStringLength = *paramFileStringLength;

  LOG_DEBUG("Exit GetParameterFileString().");
  return false;
}

int ModelLibrary::GetModelDriverName(std::string * const modelDriverName) const
{
  LOG_DEBUG("Enter GetModelDriverName().");

  if (libraryHandle_ == 0)
  {
    LOG_DEBUG("Exit GetModelDriverName().");
    return true;  // not open
  }

  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL)
  {
    LOG_DEBUG("Exit GetModelDriverName().");
    return true;
  }

  std::string modelDriverNameSymbol(modelName_ + "_driver_name");
  char const * const modelDriverNameString
      = static_cast<char const * const>(
          dlsym(libraryHandle_, modelDriverNameSymbol.c_str()));
  if (modelDriverNameString == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetModelDriverName().");
    return true;
  }

  *modelDriverName = modelDriverNameString;

  LOG_DEBUG("Exit GetModelDriverName().");
  return false;
}

int ModelLibrary::GetModelCompiledWithVersion(
    std::string * const versionString) const
{
  LOG_DEBUG("Enter GetModelCompiledWithVersion().");

  if (libraryHandle_ == 0)
  {
    LOG_DEBUG("Exit GetModelCompiledWithVersion().");
    return true;  // not open
  }

  std::string versionSymbol(modelName_ + "_compiled_with_version");
  char const * versionCharString
      = static_cast<char const *>(dlsym(libraryHandle_, versionSymbol.c_str()));
  if (versionCharString == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit GetModelCompiledWithVersion().");
    return true;
  }

  *versionString = versionCharString;
  LOG_DEBUG("Exit GetModelCompiledWithVersion().");
  return false;
}

void ModelLibrary::LogEntry(LogVerbosity const logVerbosity,
                            std::string const & message,
                            int const lineNumber,
                            std::string const & fileName) const
{
  if (log_)
    log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

}  // namespace KIM

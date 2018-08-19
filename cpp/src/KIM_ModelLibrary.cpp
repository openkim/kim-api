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
#include "KIM_Configuration.hpp"

#ifndef KIM_MODEL_LIBRARY_HPP_
#include "KIM_ModelLibrary.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

namespace KIM
{
// log helpers
#define SNUM( x ) static_cast<std::ostringstream &>(    \
    std::ostringstream() << std::dec << x).str()
#define SPTR( x ) static_cast<std::ostringstream &>(                    \
    std::ostringstream() << static_cast<void const *>(x) ).str()
#define SFUNCP( x ) static_cast<std::ostringstream &>(           \
    std::ostringstream() << static_cast<func **>(x)).str()
#define SBOOL( x ) std::string((x ? "true" : "false"))

#include "KIM_ModelLibraryLogMacros.hpp"
ModelLibrary::ModelLibrary(Log * const log) :
    libraryHandle_(NULL),
    log_(log)
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelLibrary("
      + SPTR(log) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  LOG_DEBUG("Exit   " + callString);
}

ModelLibrary::~ModelLibrary()
{
#if DEBUG_VERBOSITY
  std::string const callString = "~ModelLibrary().";
#endif
  LOG_DEBUG("Enter  " + callString);

  Close();

  LOG_DEBUG("Exit   " + callString);
}

int ModelLibrary::Open(bool const typeIsModel, std::string const & modelName)
{
#if DEBUG_VERBOSITY
  std::string const callString = "Open("
      + SBOOL(typeIsModel) + ", '" + modelName + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (libraryHandle_ != NULL)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // already open
  }

  modelName_ = modelName;

  std::vector<std::string> item;
  bool accessible = findItem(
      (typeIsModel ? OLD_KIM::KIM_MODELS_DIR : OLD_KIM::KIM_MODEL_DRIVERS_DIR),
      modelName_, &item, log_);
  if (!accessible)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // cannot find modelName
  }
  libraryPath_ = item[OLD_KIM::IE_DIR] + "/" + item[OLD_KIM::IE_NAME]
      + "/" KIM_SHARED_MODULE_PREFIX  KIM_PROJECT_NAME "-"
      + (typeIsModel ? KIM_MODEL_IDENTIFIER : KIM_MODEL_DRIVER_IDENTIFIER)
      + KIM_SHARED_MODULE_SUFFIX;

  libraryHandle_ = dlopen(libraryPath_.c_str(), RTLD_NOW);
  if (libraryHandle_ == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::Close()
{
#if DEBUG_VERBOSITY
  std::string const callString = "Close().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (libraryHandle_ == NULL) return true;  // not open

  modelName_ = "";
  int error = dlclose(libraryHandle_);
  if (error)
  {
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    libraryHandle_ = NULL;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetModelType(ITEM_TYPE * const modelType) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetModelType("
      + SPTR(modelType) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *modelType = SIMULATOR_MODEL;  // dummy value
  if (libraryHandle_ == NULL) return true;  // not open

  char const * const KIM_ItemType
      = static_cast<char const *>(dlsym(libraryHandle_, "kim_item_type"));
  if (KIM_ItemType == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (std::string(KIM_ItemType) == KIM_STAND_ALONE_MODEL_IDENTIFIER)
    *modelType = STAND_ALONE_MODEL;
  else if (std::string(KIM_ItemType) == KIM_PARAMETERIZED_MODEL_IDENTIFIER)
    *modelType = PARAMETERIZED_MODEL;
  else if (std::string(KIM_ItemType) == KIM_SIMULATOR_MODEL_IDENTIFIER)
    *modelType = SIMULATOR_MODEL;
  else if (std::string(KIM_ItemType) == KIM_MODEL_DRIVER_IDENTIFIER)
    *modelType = MODEL_DRIVER;
  else
  {
    std::cout << "unknown kim_item_type" << std::endl;
    LOG_ERROR("");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetModelCreateFunctionPointer(
    LanguageName * const languageName, func ** const functionPointer) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetModelCreateFunctionPointer("
      + SPTR(languageName) + ", " + SFUNCP(functionPointer) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (libraryHandle_ == NULL) return true;  // not open

  std::string languageSymbol(modelName_ + "_language");
  char const * const languageNameString
      = reinterpret_cast<char const *>(dlsym(libraryHandle_,
                                             languageSymbol.c_str()));
  if (languageNameString == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
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

  if (pointerToFunctionPointer == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *functionPointer = *(pointerToFunctionPointer);
  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetNumberOfParameterFiles(int * const numberOfParameterFiles)
    const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetNumberOfParameterFiles("
      + SPTR(numberOfParameterFiles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *numberOfParameterFiles = 0;  // default value
  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL) return true;

  int const * const numParamFiles = static_cast<int const *>(
      dlsym(libraryHandle_, "number_of_parameter_files"));
  if (numParamFiles == 0)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *numberOfParameterFiles = *numParamFiles;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetParameterFileString(
    int const index,
    unsigned int * const parameterFileStringLength,
    unsigned char const ** const parameterFileString) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterFileString("
      + SNUM(index) + ", " + SPTR(parameterFileStringLength)
      + ", " + SPTR(parameterFileString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int numberOfParameterFiles;
  GetNumberOfParameterFiles(&numberOfParameterFiles);
  if ((index < 0) || index >= numberOfParameterFiles)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::stringstream paramFileStringSymbol;
  paramFileStringSymbol << "parameter_file_" << (index+1);
  unsigned char const * const paramFileString
      = static_cast<unsigned char const *>(
          dlsym(libraryHandle_, paramFileStringSymbol.str().c_str()));
  if (paramFileString == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
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
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *parameterFileString = paramFileString;
  *parameterFileStringLength = *paramFileStringLength;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetModelDriverName(std::string * const modelDriverName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetModelDriverName("
      + SPTR(modelDriverName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (libraryHandle_ == NULL)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;  // not open
  }

  ITEM_TYPE itemType;
  GetModelType(&itemType);
  if (itemType != PARAMETERIZED_MODEL)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::string modelDriverNameSymbol(modelName_ + "_driver_name");
  char const * const modelDriverNameString
      = static_cast<char const *>(
          dlsym(libraryHandle_, modelDriverNameSymbol.c_str()));
  if (modelDriverNameString == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *modelDriverName = modelDriverNameString;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelLibrary::GetModelCompiledWithVersion(
    std::string * const versionString) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetModelCompiledWithVersion("
      + SPTR(versionString) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (libraryHandle_ == NULL)
  {
    LOG_DEBUG("Exit GetModelCompiledWithVersion().");
    return true;  // not open
  }

  std::string versionSymbol(modelName_ + "_compiled_with_version");
  char const * versionCharString
      = static_cast<char const *>(dlsym(libraryHandle_, versionSymbol.c_str()));
  if (versionCharString == NULL)
  {
    std::cout << dlerror() << std::endl;
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  *versionString = versionCharString;
  LOG_DEBUG("Exit 1=" + callString);
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

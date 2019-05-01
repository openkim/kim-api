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

#include <fstream>
#include <iomanip>
#include <sstream>

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIMHDR_OLD_KIM_API_DIRS_H
#include "old_KIM_API_DIRS.h"
#endif

#ifndef KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_
#include "KIM_SimulatorModelImplementation.hpp"
#endif

#include "json/json.h"
#include <iostream>

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


#include "KIM_LogMacros.hpp"
#define KIM_LOGGER_OBJECT_NAME this
namespace KIM
{
int SimulatorModelImplementation::Create(
    std::string const & simulatorModelName,
    SimulatorModelImplementation ** const simulatorModelImplementation)
{
  // error checking of arguments performed as part of SimulatorModelCreate()

  Log * pLog;
  int error = Log::Create(&pLog);
  if (error) { return true; }

  SimulatorModelImplementation * pSimulatorModelImplementation;
  pSimulatorModelImplementation
      = new SimulatorModelImplementation(new SharedLibrary(pLog), pLog);
#if DEBUG_VERBOSITY
  std::string const callString = "Create('" + simulatorModelName + "', "
                                 + SPTR(simulatorModelImplementation) + ").";
  pSimulatorModelImplementation->LogEntry(
      LOG_VERBOSITY::debug,
      "Created Log and SimulatorModelImplementation objects after enter "
          + callString,
      __LINE__,
      __FILE__);
#endif

  error = pSimulatorModelImplementation->Initialize(simulatorModelName);
  if (error)
  {
#if DEBUG_VERBOSITY
    pSimulatorModelImplementation->LogEntry(
        LOG_VERBOSITY::debug,
        "Destroying SimulatorModelImplementation object and exit " + callString,
        __LINE__,
        __FILE__);
#endif
    delete pSimulatorModelImplementation;  // also deletes pLog

    return true;
  }

  *simulatorModelImplementation = pSimulatorModelImplementation;
#if DEBUG_VERBOSITY
  (*simulatorModelImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Exit 0=" + callString, __LINE__, __FILE__);
#endif
  return false;
}

void SimulatorModelImplementation::Destroy(
    SimulatorModelImplementation ** const simulatorModelImplementation)
{
#if DEBUG_VERBOSITY
  std::string callString
      = "Destroy(" + SPTR(simulatorModelImplementation) + ").";
  (*simulatorModelImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Enter  " + callString, __LINE__, __FILE__);
#endif

#if DEBUG_VERBOSITY
  (*simulatorModelImplementation)
      ->LogEntry(LOG_VERBOSITY::debug,
                 "Destroying SimulatorModelImplementation object and exit "
                     + callString,
                 __LINE__,
                 __FILE__);
#endif
  delete *simulatorModelImplementation;  // also deletes Log object
  *simulatorModelImplementation = NULL;
}

void SimulatorModelImplementation::GetSimulatorName(
    std::string const ** const simulatorName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSimulatorName(" + SPTR(simulatorName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *simulatorName = &simulatorName_;

  LOG_DEBUG("Exit 0=" + callString);
}

void SimulatorModelImplementation::GetSimulatorVersion(
    std::string const ** const simulatorVersion) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSimulatorVersion(" + SPTR(simulatorVersion) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *simulatorVersion = &simulatorVersion_;

  LOG_DEBUG("Exit 0=" + callString);
}

void SimulatorModelImplementation::GetNumberOfSupportedSpecies(
    int * const numberOfSupportedSpecies) const
{
  *numberOfSupportedSpecies = simulatorSupportedSpecies_.size();
}

int SimulatorModelImplementation::GetSupportedSpecies(
    int const index, std::string const ** const speciesName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSupportedSpecies(" + SNUM(index) + ", " + SPTR(speciesName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((index < 0)
      || (static_cast<size_t>(index) >= simulatorSupportedSpecies_.size()))
  {
    LOG_ERROR("Invalid index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *speciesName = &(simulatorSupportedSpecies_[index]);

  LOG_DEBUG("Exit 0=" + callString);
  return true;
}

void SimulatorModelImplementation::ClearTemplateMap()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ClearTemplateMap().";
#endif
  LOG_DEBUG("Enter  " + callString);

  templateMapOpen_ = true;

  // clear data
  templateMap_.clear();
  simulatorFields_.clear();
  AddStandardTemplatesToMap();

  LOG_DEBUG("Exit 0=" + callString);
}

int SimulatorModelImplementation::AddTemplateMap(std::string const & key,
                                                 std::string const & value)
{
#if DEBUG_VERBOSITY
  std::string const callString = "AddTemplateMap().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (!templateMapOpen_)
  {
    LOG_ERROR("Template map is closed.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  char allowedCharacters[] = "abcdefghijklmnopqrstuvwxyz"
                             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             "-0123456789";
  if (key.find_first_not_of(allowedCharacters) == std::string::npos)
  { templateMap_[key] = value; }
  else
  {
    LOG_ERROR("Invalid template key, '" + key + "'.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::CloseTemplateMap()
{
#if DEBUG_VERBOSITY
  std::string const callString = "CloseTemplateMap().";
#endif
  LOG_DEBUG("Enter  " + callString);

  templateMapOpen_ = false;
  ProcessSimulatorFields();

  LOG_DEBUG("Exit 0=" + callString);
}

void SimulatorModelImplementation::AddStandardTemplatesToMap()
{
#if DEBUG_VERBOSITY
  std::string const callString = "AddStandardTemplatesToMap().";
#endif
  LOG_DEBUG("Enter  " + callString);

  for (size_t i = 0; i < parameterFileNames_.size(); ++i)
  {
    std::string key = "parameter-file-" + SNUM(i + 1);
    templateMap_[key] = parameterFileNames_[i];
  }

  LOG_DEBUG("Exit 0=" + callString);
}

int SimulatorModelImplementation::ProcessSimulatorFields()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ProcessSimulatorFields().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (templateMapOpen_)
  {
    LOG_ERROR("Template map is open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  for (size_t i = 0; i < originalSimulatorFields_.size(); ++i)
  {
    std::vector<std::string> lines;
    for (size_t j = 0; j < originalSimulatorFields_[i].size(); ++j)
    {
      std::string line(originalSimulatorFields_[i][j]);
      for (std::map<std::string, std::string>::const_iterator itr
           = templateMap_.begin();
           itr != templateMap_.end();
           ++itr)
      {
        std::string const key = "@<" + itr->first + ">@";
        size_t pos = 0;
        while ((pos = line.find(key, pos)) != std::string::npos)
        {
          line.replace(pos, key.length(), itr->second);
          pos += (itr->second).length();
        }
      }
      lines.push_back(line);
    }
    simulatorFields_.push_back(lines);
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::GetNumberOfSimulatorFields(
    int * const numberOfSimulatorFields) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfSimulatorFields(" + SPTR(numberOfSimulatorFields) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *numberOfSimulatorFields = simulatorFields_.size();

  LOG_DEBUG("Exit 0=" + callString);
}

int SimulatorModelImplementation::GetSimulatorFieldMetadata(
    int const fieldIndex,
    int * const extent,
    std::string const ** const fieldName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetSimulatorFieldMetadata(" + SNUM(fieldIndex)
                                 + ", " + SPTR(extent) + ", " + SPTR(fieldName)
                                 + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((fieldIndex < 0)
      || (static_cast<size_t>(fieldIndex) >= simulatorFields_.size()))
  {
    LOG_ERROR("Invalid simulator field index, " + SNUM(fieldIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  if (extent) *extent = simulatorFields_[fieldIndex].size();
  if (fieldName) *fieldName = &simulatorFieldNames_[fieldIndex];

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::GetSimulatorFieldLine(
    int const fieldIndex,
    int const lineIndex,
    std::string const ** const lineValue) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetSimulatorFieldLine(" + SNUM(fieldIndex)
                                 + ", " + SNUM(lineIndex) + ", "
                                 + SPTR(lineValue) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((fieldIndex < 0)
      || (static_cast<size_t>(fieldIndex) >= simulatorFields_.size()))
  {
    LOG_ERROR("Invalid simulator field index, " + SNUM(fieldIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if ((lineIndex < 0)
      || (static_cast<size_t>(lineIndex)
          >= simulatorFields_[fieldIndex].size()))
  {
    LOG_ERROR("Invalid simulator field line index, " + SNUM(lineIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *lineValue = &(simulatorFields_[fieldIndex][lineIndex]);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::GetMetadataFileName(
    std::string const ** const originalMetadataFileName,
    std::string const ** const metadataFileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetMetadataFileName("
                                 + SPTR(originalMetadataFileName) + ", "
                                 + SPTR(metadataFileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (originalMetadataFileName)
  { *originalMetadataFileName = &originalMetadataFileName_; }
  if (metadataFileName) { *metadataFileName = &metadataFileName_; }

  LOG_DEBUG("Exit 0=" + callString);
}

void SimulatorModelImplementation::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfParameterFiles(" + SPTR(numberOfParameterFiles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *numberOfParameterFiles = numberOfParameterFiles_;

  LOG_DEBUG("Exit 0=" + callString);
}

int SimulatorModelImplementation::GetParameterFileName(
    int const index,
    std::string const ** const originalParameterFileName,
    std::string const ** const parameterFileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterFileName(" + SNUM(index) + ", "
                                 + SPTR(originalParameterFileName) + ", "
                                 + SPTR(parameterFileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((index < 0) || (index >= numberOfParameterFiles_))
  {
    LOG_ERROR("Invalid parameter file index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  if (originalParameterFileName)
  { *originalParameterFileName = &originalParameterFileNames_[index]; }
  if (parameterFileName) { *parameterFileName = &parameterFileNames_[index]; }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::SetSimulatorBufferPointer(void * const ptr)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetSimulatorBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  simulatorBuffer_ = ptr;

  LOG_DEBUG("Exit   " + callString);
}

void SimulatorModelImplementation::GetSimulatorBufferPointer(
    void ** const ptr) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSimulatorBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *ptr = simulatorBuffer_;

  LOG_DEBUG("Exit   " + callString);
}

void SimulatorModelImplementation::SetLogID(std::string const & logID)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetLogID('" + logID + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->SetID(logID);

  LOG_DEBUG("Exit   " + callString);
}

void SimulatorModelImplementation::PushLogVerbosity(
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

void SimulatorModelImplementation::PopLogVerbosity()
{
#if DEBUG_VERBOSITY
  std::string const callString = "PopLogVerbosity().";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->PopVerbosity();

  LOG_DEBUG("Exit   " + callString);
}

void SimulatorModelImplementation::LogEntry(LogVerbosity const logVerbosity,
                                            std::string const & message,
                                            int const lineNumber,
                                            std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void SimulatorModelImplementation::LogEntry(LogVerbosity const logVerbosity,
                                            std::stringstream const & message,
                                            int const lineNumber,
                                            std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & SimulatorModelImplementation::ToString() const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ToString().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::stringstream ss;
  ss << std::setprecision(10) << std::scientific << std::left;
  ss << "===================================================================="
        "============\n\n";

  ss << "SimulatorModel object\n"
     << "------------\n\n";
  ss << "SimulatorModel Name : " << simulatorModelName_ << "\n";
  ss << "Log ID : " << log_->GetID() << "\n";
  ss << "\n";

  ss << "Metadata file\n"
     << "\tOriginal Name : " << originalMetadataFileName_ << "\n"
     << "\tOn-disk Name  : " << metadataFileName_ << "\n\n";

  ss << "Simulator Name    : " << simulatorName_ << "\n"
     << "Simulator Version : " << simulatorVersion_ << "\n"
     << "Number of supported species : " << simulatorSupportedSpecies_.size()
     << "\n";
  for (size_t i = 0; i < simulatorSupportedSpecies_.size(); ++i)
  {
    ss << "\tindex : " << i << "\t'" << simulatorSupportedSpecies_[i] << "'\n";
  }
  ss << "\n";

  ss << "Number of parameter files : " << numberOfParameterFiles_ << "\n"
     << "Parameter files :\n";
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  {
    ss << "\t"
       << "index  : " << i << "\n"
       << "\t  "
       << "Original Name : " << originalParameterFileNames_[i] << "\n"
       << "\t  "
       << "On-disk Name  : " << parameterFileNames_[i] << "\n\n";
  }

  ss << "Original simulator fields :\n";
  for (size_t i = 0; i < originalSimulatorFields_.size(); ++i)
  {
    ss << "\t" << simulatorFieldNames_[i] << " :\n";
    for (size_t j = 0; j < originalSimulatorFields_[i].size(); ++j)
    { ss << "\t  * '" << originalSimulatorFields_[i][j] << "'\n"; }
  }
  ss << "\n";

  ss << "Template Map is : " << ((templateMapOpen_) ? "open" : "closed")
     << "\n";
  ss << "Template Map contents :\n";
  for (std::map<std::string, std::string>::const_iterator itr
       = templateMap_.begin();
       itr != templateMap_.end();
       ++itr)
  {
    ss << "\t" << std::setw(20) << itr->first << " -> " << std::setw(20)
       << itr->second << "\n";
  }
  ss << "\n";

  ss << "Simulator fields :\n";
  for (size_t i = 0; i < simulatorFields_.size(); ++i)
  {
    ss << "\t" << simulatorFieldNames_[i] << " :\n";
    for (size_t j = 0; j < simulatorFields_[i].size(); ++j)
    { ss << "\t  * '" << simulatorFields_[i][j] << "'\n"; }
  }
  ss << "\n";


  ss << "Simulator buffer: " << SPTR(simulatorBuffer_) << "\n\n";

  ss << "===================================================================="
        "============\n";

  string_ = ss.str();
  LOG_DEBUG("Exit   " + callString);
  return string_;
}


SimulatorModelImplementation::SimulatorModelImplementation(
    SharedLibrary * const sharedLibrary, Log * const log) :
    simulatorModelName_(""),
    sharedLibrary_(sharedLibrary),
    log_(log),
    originalMetadataFileName_(""),
    metadataFileName_(""),
    simulatorName_(""),
    simulatorVersion_(""),
    numberOfParameterFiles_(0),
    templateMapOpen_(true),
    simulatorBuffer_(NULL),
    string_("")
{
#if DEBUG_VERBOSITY
  std::string const callString = "SimulatorModelImplementation("
                                 + SPTR(sharedLibrary) + ", " + SPTR(log)
                                 + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  LOG_DEBUG("Exit   " + callString);
}

SimulatorModelImplementation::~SimulatorModelImplementation()
{
#if DEBUG_VERBOSITY
  std::string const callString = "~SimulatorModelImplementation().";
#endif
  LOG_DEBUG("Enter  " + callString);

  RemoveMetadataAndParameterFiles();

  delete sharedLibrary_;

  LOG_DEBUG("Destroying Log object and exit " + callString);
  Log::Destroy(&log_);
}

int SimulatorModelImplementation::Initialize(
    std::string const & simulatorModelName)
{
#if DEBUG_VERBOSITY
  std::string const callString = "Initialize(" + simulatorModelName + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  simulatorModelName_ = simulatorModelName;

  std::vector<std::string> sharedLibraryList;
  if (!findItem(OLD_KIM::KIM_SIMULATOR_MODELS,
                simulatorModelName,
                &sharedLibraryList,
                NULL))
  {
    LOG_ERROR("Could not find simulator model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int error = sharedLibrary_->Open(sharedLibraryList[OLD_KIM::IE_FULLPATH]);
  if (error)
  {
    LOG_ERROR("Could not open simulator model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  SharedLibrary::ITEM_TYPE itemType;
  error = sharedLibrary_->GetType(&itemType);
  if (error)
  {
    LOG_ERROR("Unable to get shared library type.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  switch (itemType)
  {
    case SharedLibrary::SIMULATOR_MODEL:
      LOG_DEBUG("Initializing a simulator model.");
      // fall through to continue
      break;
    case SharedLibrary::STAND_ALONE_MODEL:
      LOG_ERROR("Creation of a stand alone model is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
    case SharedLibrary::PARAMETERIZED_MODEL:
      LOG_ERROR("Creation of a parameterized model is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
    case SharedLibrary::MODEL_DRIVER:
      LOG_ERROR("Creation of a model driver is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
    default:
      LOG_ERROR("Creation of an unknown model type is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
  }

  error = WriteMetadataAndParameterFiles();
  if (error)
  {
    LOG_ERROR("Could not write parameter files to scratch space.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::ifstream ifs;
  ifs.open(metadataFileName_, std::ifstream::in);
  Json::Reader reader;
  Json::Value val;
  reader.parse(ifs, val);
  ifs.close();

  for (Json::Value::const_iterator itr = val.begin(); itr != val.end(); ++itr)
  {
    if (itr.key() == "simulator-version")
    { simulatorVersion_ = (*itr).asString(); }
    else if (itr.key() == "simulator-name")
    {
      simulatorName_ = (*itr).asString();
    }
    else if (itr.key() == "supported-species")
    {
      std::stringstream ss((*itr).asString());
      while (ss)
      {
        std::string species;
        ss >> species;
        if (species != "") simulatorSupportedSpecies_.push_back(species);
      }
    }
    else if (itr.key() == "extended-id")
    {
      // no-op
    }
    else
    {
      simulatorFieldNames_.push_back(itr.key().asString());
      std::vector<std::string> lines;
      if (simulatorFieldNames_.back() == "units")
      { lines.push_back(itr->asString()); }
      else
      {
        for (Json::Value::const_iterator field = itr->begin();
             field != itr->end();
             ++field)
        { lines.push_back(field->asString()); }
      }
      originalSimulatorFields_.push_back(lines);
    }
  }
  // check for all required data
  if (simulatorName_ == "")
  {
    LOG_ERROR("Required metadata field 'simulator-name' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (simulatorVersion_ == "")
  {
    LOG_ERROR("Required metadata field 'simulator-version' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (simulatorSupportedSpecies_.size() == 0)
  {
    LOG_ERROR("Required metadata field 'supported-species' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // close model and open driver
  error = sharedLibrary_->Close();
  if (error)
  {
    LOG_ERROR("Could not close model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  ClearTemplateMap();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::WriteMetadataAndParameterFiles()
{
#if DEBUG_VERBOSITY
  std::string const callString = "WriteParameterFiles().";
#endif
  LOG_DEBUG("Enter  " + callString);

  int error;

  unsigned int len;
  unsigned char const * metadataData;
  error = sharedLibrary_->GetMetadataFile(
      &originalMetadataFileName_, &len, &metadataData);
  if (error)
  {
    LOG_ERROR("Unable to get metadata file.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  static char const metadataFileNameString[]
      = "kim-simulator-model-metadata-file-XXXXXXXXXXXX";
  std::stringstream templateString;
  templateString << P_tmpdir
                 << ((*(--(std::string(P_tmpdir).end())) == '/') ? "" : "/")
                 << metadataFileNameString;
  char * cstr = strdup(templateString.str().c_str());
  int fileid = mkstemp(cstr);
  if (fileid == -1)
  {
    free(cstr);
    LOG_ERROR("Could not create a secure temporary file.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  metadataFileName_ = cstr;
  free(cstr);
  FILE * fl = fdopen(fileid, "w");
  fwrite(metadataData, sizeof(unsigned char), len, fl);
  fclose(fl);  // also closed the fileid
  fl = NULL;


  sharedLibrary_->GetNumberOfParameterFiles(&numberOfParameterFiles_);
  std::vector<unsigned char const *> parameterFileStrings;
  std::vector<unsigned int> parameterFileStringLengths;
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  {
    std::string parameterFileName;
    unsigned char const * strPtr;
    unsigned int length;
    error = sharedLibrary_->GetParameterFile(
        i, &parameterFileName, &length, &strPtr);
    if (error)
    {
      LOG_ERROR("Could not get parameter file data.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    originalParameterFileNames_.push_back(parameterFileName);
    parameterFileStrings.push_back(strPtr);
    parameterFileStringLengths.push_back(length);
  }

  static char const fileNameString[]
      = "kim-simulator-model-parameter-file-XXXXXXXXXXXX";
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  {
    templateString.str("");
    templateString << P_tmpdir
                   << ((*(--(std::string(P_tmpdir).end())) == '/') ? "" : "/")
                   << fileNameString;
    cstr = strdup(templateString.str().c_str());
    fileid = mkstemp(cstr);
    if (fileid == -1)
    {
      free(cstr);
      LOG_ERROR("Could not create a secure temporary file.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    parameterFileNames_.push_back(cstr);
    free(cstr);

    fl = fdopen(fileid, "w");
    fwrite(parameterFileStrings[i],
           sizeof(unsigned char),
           parameterFileStringLengths[i],
           fl);
    fclose(fl);  // also closed the fileid
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::RemoveMetadataAndParameterFiles()
{
  // remove metadata file
  if (metadataFileName_ != "") remove(metadataFileName_.c_str());
  metadataFileName_.clear();

  // remove parameter files
  for (size_t i = 0; i < parameterFileNames_.size(); ++i)
  {
    if (parameterFileNames_[i] != "") remove(parameterFileNames_[i].c_str());
  }
  // clear out parameter file stuff
  numberOfParameterFiles_ = -1;
  parameterFileNames_.clear();
}
}  // namespace KIM

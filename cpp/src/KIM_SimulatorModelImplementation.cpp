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


#include "edn-cpp/edn.hpp"
#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <streambuf>
#include <unistd.h>


#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

#ifndef KIM_COLLECTIONS_HPP_
#include "KIM_Collections.hpp"
#endif

#ifndef KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_
#include "KIM_SimulatorModelImplementation.hpp"
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
  if (error)
  {
    *simulatorModelImplementation = NULL;
    return true;
  }

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

  Collections * col;
  error = Collections::Create(&col);
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
    *simulatorModelImplementation = NULL;
    return true;
  }
  col->SetLogID(pLog->GetID() + "_Collections");

  pSimulatorModelImplementation->collections_ = col;

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
    delete pSimulatorModelImplementation;  // also deletes Log object and
                                           // collections object
    *simulatorModelImplementation = NULL;
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
  delete *simulatorModelImplementation;  // also deletes Log object and
                                         // collections object
  *simulatorModelImplementation = NULL;
}

void SimulatorModelImplementation::GetSimulatorNameAndVersion(
    std::string const ** const simulatorName,
    std::string const ** const simulatorVersion) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetSimulatorNameAndVersion("
                                 + SPTR(simulatorName) + ", "
                                 + SPTR(simulatorVersion) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (simulatorName != NULL) *simulatorName = &simulatorName_;
  if (simulatorVersion != NULL) *simulatorVersion = &simulatorVersion_;

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
  return false;
}

void SimulatorModelImplementation::OpenAndInitializeTemplateMap()
{
#if DEBUG_VERBOSITY
  std::string const callString = "OpenAndInitializeTemplateMap().";
#endif
  LOG_DEBUG("Enter  " + callString);

  templateMapOpen_ = true;

  // clear data
  templateMap_.clear();
  simulatorFields_.clear();
  AddStandardTemplatesToMap();

  LOG_DEBUG("Exit 0=" + callString);
}

int SimulatorModelImplementation::TemplateMapIsOpen() const
{
#if DEBUG_VERBOSITY
  std::string const callString = "TemplateMapIsOpen().";
#endif
  LOG_DEBUG("Enter  " + callString);

  int result = templateMapOpen_;

  LOG_DEBUG("Exit " + SNUM(result) + "=" + callString);
  return result;
}

int SimulatorModelImplementation::AddTemplateMap(std::string const & key,
                                                 std::string const & value)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "AddTemplateMap('" + key + "', '" + value + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (!templateMapOpen_)
  {
    LOG_ERROR("Template map is closed.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  char allowedCharacters[] = "abcdefghijklmnopqrstuvwxyz"
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

  AddTemplateMap("parameter-file-dir", parameterFileDirectoryName_);

  for (size_t i = 0; i < parameterFileNames_.size(); ++i)
  {
    AddTemplateMap("parameter-file-basename-" + SNUM(i + 1),
                   parameterFileNames_[i]);

    AddTemplateMap("parameter-file-" + SNUM(i + 1),
                   parameterFileDirectoryName_ + "/" + parameterFileNames_[i]);
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

  simulatorFields_.clear();
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

  *numberOfSimulatorFields = originalSimulatorFields_.size();

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
      || (static_cast<size_t>(fieldIndex) >= originalSimulatorFields_.size()))
  {
    LOG_ERROR("Invalid simulator field index, " + SNUM(fieldIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  if (extent != NULL) *extent = originalSimulatorFields_[fieldIndex].size();
  if (fieldName != NULL) *fieldName = &simulatorFieldNames_[fieldIndex];

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
  if (templateMapOpen_)
  {
    LOG_ERROR("Simulator field lines are not available while the template map "
              "is open.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
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

void SimulatorModelImplementation::GetParameterFileDirectoryName(
    std::string const ** const directoryName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetParameterFileDirectoryName(" + SPTR(directoryName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *directoryName = &parameterFileDirectoryName_;

  LOG_DEBUG("Exit 0=" + callString);
}

void SimulatorModelImplementation::GetSpecificationFileName(
    std::string const ** const specificationFileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSpecificationFileName(" + SPTR(specificationFileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *specificationFileName = &specificationFileName_;

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
    int const index, std::string const ** const parameterFileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterFileName(" + SNUM(index) + ", "
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

  *parameterFileName = &parameterFileNames_[index];

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

  ss << "Parameter file directory : " << parameterFileDirectoryName_ << "\n";

  ss << "Specification file name : " << specificationFileName_ << "\n\n";

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
       << "index : " << i << "\n"
       << "\t  "
       << "name : " << parameterFileNames_[i] << "\n\n";
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
    collections_(NULL),
    simulatorModelName_(""),
    sharedLibrary_(sharedLibrary),
    log_(log),
    parameterFileDirectoryName_(""),
    specificationFileName_(""),
    schemaVersion_(0),
    modelName_(""),
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

  RemoveParameterFileDirectory();

  delete sharedLibrary_;

  if (collections_ != NULL) Collections::Destroy(&collections_);

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

  std::string const * itemFilePath;
  int error = collections_->GetItemLibraryFileNameAndCollection(
      COLLECTION_ITEM_TYPE::simulatorModel,
      simulatorModelName,
      &itemFilePath,
      NULL);
  if (error)
  {
    LOG_ERROR("Could not find simulator model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  error = sharedLibrary_->Open(*itemFilePath);
  if (error)
  {
    LOG_ERROR("Could not open simulator model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  CollectionItemType itemType;
  error = sharedLibrary_->GetType(&itemType);
  if (error)
  {
    LOG_ERROR("Unable to get shared library type.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  {
    using namespace COLLECTION_ITEM_TYPE;

    if (itemType == simulatorModel)
    { LOG_DEBUG("Initializing a simulator model."); }
    else if (itemType == portableModel)
    {
      LOG_ERROR("Creation of a portable model is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    else if (itemType == modelDriver)
    {
      LOG_ERROR("Creation of a model driver is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    else
    {
      LOG_ERROR("Creation of an unknown item type is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }

  error = WriteParameterFileDirectory();
  if (error)
  {
    LOG_ERROR("Could not write parameter file directory to scratch space.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (GetSchemaVersion())
  {
    // GetSchemaVersion() logs error
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (schemaVersion_ == 1)
  {
    if (ReadEdnSchemaV1())
    {
      // ReadEdnSchemaV1() logs error
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }
  else
  {
    LOG_ERROR("Shouldn't ever get here.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // check for all required data
  if (modelName_ == "")
  {
    LOG_ERROR("Required specification field 'model-name' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else if (modelName_ != simulatorModelName_)
  {
    LOG_ERROR(
        "Specificaiton field 'model-name' not equal to simulator model name.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (simulatorName_ == "")
  {
    LOG_ERROR("Required specification field 'simulator-name' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (simulatorVersion_ == "")
  {
    LOG_ERROR("Required specification field 'simulator-version' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (simulatorSupportedSpecies_.size() == 0)
  {
    LOG_ERROR("Required specification field 'supported-species' not found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  OpenAndInitializeTemplateMap();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::ParseEdn(edn::EdnNode & node) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ParseEdn().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::string const filePath
      = parameterFileDirectoryName_ + "/" + specificationFileName_;
  std::ifstream ifs;
  ifs.open(filePath.c_str(), std::ifstream::in);
  if (!ifs.is_open())
  {
    LOG_ERROR("Unable to open simulator model metatdata file.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  std::string ednString((std::istreambuf_iterator<char>(ifs)),
                        std::istreambuf_iterator<char>());
  ifs.close();

  try
  {
    node = edn::read(ednString);
  }
  catch (std::string e)
  {
    LOG_ERROR("Unable to parse EDN file: " + e + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::GetSchemaVersion()
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetSchemaVersion().";
#endif
  LOG_DEBUG("Enter  " + callString);

  edn::EdnNode node;
  if (ParseEdn(node))
  {
    // ParseEdn() logs error
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Read " + edn::typeToString(node.type) + ": " + node.value + ".");
  if (node.type == edn::EdnMap)
  {
    std::list<edn::EdnNode>::const_iterator itr;
    for (itr = node.values.begin(); itr != node.values.end(); ++itr)
    {
      // find key
      if (itr->type == edn::EdnString)
      {
        std::string key(itr->value);
        LOG_DEBUG("Read " + edn::typeToString(itr->type) + ": " + itr->value
                  + ".");

        // get value
        ++itr;
        LOG_DEBUG("Read " + edn::typeToString(itr->type) + ": " + itr->value
                  + ".");
        if (key == "kim-api-sm-schema-version")
        {
          if (itr->type == edn::EdnInt)
          { std::istringstream(itr->value) >> schemaVersion_; }
          else
          {
            LOG_ERROR("Expecting 'EdnInt'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
      }
    }
  }
  else
  {
    LOG_ERROR("Expecting 'EdnMap'.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (schemaVersion_ != 1)
  {
    LOG_ERROR("Unsupported schema version found.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::ReadEdnSchemaV1()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ReadEdnSchemaV1().";
#endif
  LOG_DEBUG("Enter  " + callString);

  edn::EdnNode node;
  if (ParseEdn(node))
  {
    // ParseEdn() logs error
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Read " + edn::typeToString(node.type) + ": " + node.value + ".");
  if (node.type == edn::EdnMap)
  {
    std::list<edn::EdnNode>::const_iterator itr;
    for (itr = node.values.begin(); itr != node.values.end(); ++itr)
    {
      // find key
      if (itr->type == edn::EdnString)
      {
        std::string key(itr->value);
        LOG_DEBUG("Read " + edn::typeToString(itr->type) + ": " + itr->value
                  + ".");

        // get value
        ++itr;
        LOG_DEBUG("Read " + edn::typeToString(itr->type) + ": " + itr->value
                  + ".");
        if (key == "kim-api-sm-schema-version")
        {
          if (itr->type == edn::EdnInt)
          { std::istringstream(itr->value) >> schemaVersion_; }
          else
          {
            LOG_ERROR("Expecting 'EdnInt'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
        else if (key == "simulator-version")
        {
          if (itr->type == edn::EdnString) { simulatorVersion_ = itr->value; }
          else
          {
            LOG_ERROR("Expecting 'EdnString'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
        else if (key == "simulator-name")
        {
          if (itr->type == edn::EdnString) { simulatorName_ = itr->value; }
          else
          {
            LOG_ERROR("Expecting 'EdnString'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
        else if (key == "supported-species")
        {
          if (itr->type == edn::EdnString)
          {
            std::stringstream ss(itr->value);
            while (ss)
            {
              std::string species;
              ss >> species;
              if (species != "") simulatorSupportedSpecies_.push_back(species);
            }
          }
          else
          {
            LOG_ERROR("Expecting 'EdnString'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
        else if (key == "model-name")
        {
          if (itr->type == edn::EdnString) { modelName_ = itr->value; }
          else
          {
            LOG_ERROR("Expecting 'EdnString'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }
        }
        else  // simulator field
        {
          simulatorFieldNames_.push_back(key);
          std::vector<std::string> lines;

          if (itr->type == edn::EdnVector)
          {
            std::list<edn::EdnNode>::const_iterator vec;
            for (vec = itr->values.begin(); vec != itr->values.end(); ++vec)
            {
              LOG_DEBUG("Read " + edn::typeToString(vec->type) + ": "
                        + vec->value + ".");
              if (vec->type == edn::EdnString) { lines.push_back(vec->value); }
              else
              {
                LOG_ERROR("Expecting 'EdnString'.");
                LOG_DEBUG("Exit 1=" + callString);
                return true;
              }
            }
          }
          else if (itr->type == edn::EdnString)
          {
            lines.push_back(itr->value);
          }
          else
          {
            LOG_ERROR("Expecting 'EdnVector' or 'EdnString'.");
            LOG_DEBUG("Exit 1=" + callString);
            return true;
          }

          originalSimulatorFields_.push_back(lines);
        }
      }
    }
  }
  else
  {
    LOG_ERROR("Expecting 'EdnMap'.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int SimulatorModelImplementation::WriteParameterFileDirectory()
{
#if DEBUG_VERBOSITY
  std::string const callString = "WriteParameterFileDirectory().";
#endif
  LOG_DEBUG("Enter  " + callString);

  int error;

  static char const parameterFileDirectoryNameString[]
      = "kim-simulator-model-parameter-file-directory-XXXXXXXXXXXX";
  std::stringstream templateString;
  templateString << P_tmpdir
                 << ((*(--(std::string(P_tmpdir).end())) == '/') ? "" : "/")
                 << parameterFileDirectoryNameString;
  char * cstr = strdup(templateString.str().c_str());
  char * tmpdir = mkdtemp(cstr);
  if (NULL == tmpdir)
  {
    free(cstr);
    LOG_ERROR("Could not create a secure temporary directory.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  parameterFileDirectoryName_ = tmpdir;
  free(cstr);

  {
    unsigned int len;
    unsigned char const * specificationData;
    error = sharedLibrary_->GetSimulatorModelSpecificationFile(
        &specificationFileName_, &len, &specificationData);
    if (error)
    {
      LOG_ERROR("Unable to get specification file.");
      RemoveParameterFileDirectory();
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    std::string const specificationFilePathName
        = parameterFileDirectoryName_ + "/" + specificationFileName_;
    FILE * fl = fopen(specificationFilePathName.c_str(), "w");
    if (NULL == fl)
    {
      LOG_ERROR("Unable to get write parameter file.");
      RemoveParameterFileDirectory();
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    fwrite(specificationData, sizeof(unsigned char), len, fl);
    fclose(fl);
    fl = NULL;
  }

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
    parameterFileNames_.push_back(parameterFileName);

    std::string const parameterFilePathName
        = parameterFileDirectoryName_ + "/" + parameterFileName;
    FILE * fl = fopen(parameterFilePathName.c_str(), "w");
    if (NULL == fl)
    {
      LOG_ERROR("Unable to get write parameter file.");
      RemoveParameterFileDirectory();
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    fwrite(strPtr, sizeof(unsigned char), length, fl);
    fclose(fl);
    fl = NULL;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void SimulatorModelImplementation::RemoveParameterFileDirectory()
{
  if (parameterFileDirectoryName_ != "")
  {
    int error;
    struct dirent * dp = NULL;
    DIR * dir = NULL;
    dir = opendir(parameterFileDirectoryName_.c_str());
    while ((dp = readdir(dir)))
    {
      // assuming no subdirectories, just files
      if ((0 != strcmp(dp->d_name, ".")) && (0 != strcmp(dp->d_name, "..")))
      {
        std::string filePath = parameterFileDirectoryName_ + "/" + dp->d_name;
        error = remove(filePath.c_str());
        if (error)
        {
          LOG_ERROR("Unable to remove simulator model file '" + filePath
                    + "'.");
        }
      }
    }
    error = remove(parameterFileDirectoryName_.c_str());
    if (error)
    {
      LOG_ERROR("Unable to remove simulator model parameter file directory '"
                + parameterFileDirectoryName_ + "'.");
    }
    closedir(dir);

    // clear out directory and file stuff
    parameterFileDirectoryName_ = "";
    specificationFileName_ = "";
    numberOfParameterFiles_ = -1;
    parameterFileNames_.clear();
  }
}
}  // namespace KIM

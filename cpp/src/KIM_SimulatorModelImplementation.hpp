//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_
#define KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_

#include <map>
#include <sstream>
#include <string>
#include <vector>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_FILESYSTEM_PATH_HPP_
#include "KIM_FilesystemPath.hpp"
#endif

namespace edn
{
// Forward declarations
struct EdnNode;
}  // namespace edn

namespace KIM
{
// Forward declarations
class Log;
class SharedLibrary;

class SimulatorModelImplementation
{
 public:
  static int
  Create(std::string const & simulatorModelName,
         SimulatorModelImplementation ** const simulatorModelImplementation);

  static void
  Destroy(SimulatorModelImplementation ** const simulatorModelImplementation);

  void
  GetSimulatorNameAndVersion(std::string const ** const simulatorString,
                             std::string const ** const simulatorVersion) const;

  void GetNumberOfSupportedSpecies(int * const numberOfSupportedSpecies) const;
  int GetSupportedSpecies(int const index,
                          std::string const ** const speciesName) const;

  void OpenAndInitializeTemplateMap();
  int TemplateMapIsOpen() const;
  int AddTemplateMap(std::string const & key, std::string const & value);
  void CloseTemplateMap();

  void GetNumberOfSimulatorFields(int * const numberOfSimulatorFields) const;

  int GetSimulatorFieldMetadata(int const fieldIndex,
                                int * const extent,
                                std::string const ** const fieldName) const;

  int GetSimulatorFieldLine(int const fieldIndex,
                            int const lineIndex,
                            std::string const ** const lineValue) const;

  void
  GetParameterFileDirectoryName(std::string const ** const directoryName) const;

  void GetSpecificationFileName(
      std::string const ** const specificationFileName) const;

  void GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;

  int GetParameterFileName(int const index,
                           std::string const ** const parameterFileName) const;
  int GetParameterFileBasename(
      int const index, std::string const ** const parameterFileBasename) const;

  void SetSimulatorBufferPointer(void * const ptr);

  void GetSimulatorBufferPointer(void ** const ptr) const;

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

  std::string const & ToString() const;

  void SetLogID(std::string const & logID);

  void PushLogVerbosity(LogVerbosity const logVerbosity);

  void PopLogVerbosity();

 private:
  // do not allow copy constructor or operator=
  SimulatorModelImplementation(SimulatorModelImplementation const &);
  void operator=(SimulatorModelImplementation const &);

  SimulatorModelImplementation(SharedLibrary * const sharedLibrary,
                               Log * const log);
  ~SimulatorModelImplementation();

  std::string simulatorModelName_;

  SharedLibrary * sharedLibrary_;

  Log * log_;

  int ParseEdn(edn::EdnNode & node) const;
  int GetSchemaVersion();
  int ReadEdnSchemaV1();
  int Initialize(std::string const & simulatorModelName);

  FILESYSTEM::Path parameterFileDirectoryName_;
  std::string parameterFileDirectoryNameString_;
  std::string specificationFileName_;
  int schemaVersion_;
  std::string modelName_;

  std::string simulatorName_;
  std::string simulatorVersion_;
  std::vector<std::string> simulatorSupportedSpecies_;

  std::vector<std::string> simulatorFieldNames_;
  std::vector<std::vector<std::string> > originalSimulatorFields_;
  std::vector<std::vector<std::string> > simulatorFields_;

  int numberOfParameterFiles_;
  std::vector<std::string> parameterFileBasenames_;

  bool templateMapOpen_;
  std::map<std::string, std::string> templateMap_;

  void AddStandardTemplatesToMap();
  int ProcessSimulatorFields();

  void * simulatorBuffer_;

  mutable std::string string_;

};  // class SimulatorModelImplementation
}  // namespace KIM
#endif  // KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_

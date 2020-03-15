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


#ifndef KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_
#define KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_

#include <map>
#include <sstream>
#include <string>
#include <vector>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
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
class Collections;
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

  Collections * collections_;
  std::string simulatorModelName_;

  SharedLibrary * sharedLibrary_;

  Log * log_;

  int ParseEdn(edn::EdnNode & node) const;
  int GetSchemaVersion();
  int ReadEdnSchemaV1();
  int Initialize(std::string const & simulatorModelName);
  int WriteParameterFileDirectory();
  void RemoveParameterFileDirectory();

  std::string parameterFileDirectoryName_;
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
  std::vector<std::string> parameterFileNames_;

  bool templateMapOpen_;
  std::map<std::string, std::string> templateMap_;

  void AddStandardTemplatesToMap();
  int ProcessSimulatorFields();

  void * simulatorBuffer_;

  mutable std::string string_;

};  // class SimulatorModelImplementation
}  // namespace KIM
#endif  // KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_

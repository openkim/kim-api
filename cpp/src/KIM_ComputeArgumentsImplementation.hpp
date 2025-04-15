//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
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
// Release: This file is part of the kim-api-2.4.1 package.
//


#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#define KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_

#include <map>
#include <sstream>
#include <string>
#include <vector>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif


namespace KIM
{
// Forward declaration
class Log;


class ComputeArgumentsImplementation
{
 public:
  friend class ModelImplementation;

  static int Create(
      std::string const & modelName,
      std::string const & modelLogID,
      Numbering const modelNumbering,
      Numbering const simulatorNumbering,
      int const numberingOffset,
      ComputeArgumentsImplementation ** const computeArgumentsImplementation);
  static void Destroy(
      ComputeArgumentsImplementation ** const computeArgumentsImplementation);

  int SetArgumentSupportStatus(ComputeArgumentName const computeArgumentName,
                               SupportStatus const supportStatus);
  int GetArgumentSupportStatus(ComputeArgumentName const computeArgumentName,
                               SupportStatus * const supportStatus) const;


  int SetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus const supportStatus);
  int GetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus * const supportStatus) const;

  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int const * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double const * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double * const ptr);
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int const ** const ptr) const;
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int ** const ptr) const;
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double const ** const ptr) const;
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double ** const ptr) const;


  int SetCallbackPointer(ComputeCallbackName const computeCallbackName,
                         LanguageName const languageName,
                         Function * const fptr,
                         void * const dataObject);
  int IsCallbackPresent(ComputeCallbackName const computeCallbackName,
                        int * const present) const;

  void AreAllRequiredArgumentsAndCallbacksPresent(int * const result) const;

  int GetNeighborList(int const neighborListIndex,
                      int const particleNumber,
                      int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle) const;

  int ProcessDEDrTerm(double const de,
                      double const r,
                      double const * const dx,
                      int const i,
                      int const j) const;

  int ProcessD2EDr2Term(double const de,
                        double const * const r,
                        double const * const dx,
                        int const * const i,
                        int const * const j) const;


  void SetModelBufferPointer(void * const ptr);
  void GetModelBufferPointer(void ** const ptr) const;


  void SetSimulatorBufferPointer(void * const ptr);
  void GetSimulatorBufferPointer(void ** const ptr) const;

  std::string const & ToString() const;

  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();
  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

 private:
  // do not allow copy constructor or operator=
  ComputeArgumentsImplementation(ComputeArgumentsImplementation const &);
  void operator=(ComputeArgumentsImplementation const &);

  ComputeArgumentsImplementation(std::string const & modelName,
                                 Numbering const modelNumbering,
                                 Numbering const simulatorNumbering,
                                 int const numberingOffset,
                                 Log * const log);
  ~ComputeArgumentsImplementation();


  int Validate(ComputeArgumentName const computeArgumentName) const;
  int Validate(ComputeCallbackName const computeCallbackName) const;
  int Validate(LanguageName const languageName) const;
  int Validate(SupportStatus const supportStatus) const;


  std::string modelName_;

  Log * log_;

  Numbering const modelNumbering_;
  Numbering const simulatorNumbering_;
  int const numberingOffset_;

  mutable bool inModelComputeRoutine_;
  mutable int numberOfNeighborLists_;
  mutable double const * cutoffs_;

  std::map<ComputeArgumentName const,
           SupportStatus,
           COMPUTE_ARGUMENT_NAME::Comparator>
      computeArgumentSupportStatus_;
  std::map<ComputeArgumentName const, void *, COMPUTE_ARGUMENT_NAME::Comparator>
      computeArgumentPointer_;


  std::map<ComputeCallbackName const,
           SupportStatus,
           COMPUTE_CALLBACK_NAME::Comparator>
      computeCallbackSupportStatus_;
  std::map<ComputeCallbackName const,
           LanguageName,
           COMPUTE_CALLBACK_NAME::Comparator>
      computeCallbackLanguage_;
  std::map<ComputeCallbackName const,
           Function *,
           COMPUTE_CALLBACK_NAME::Comparator>
      computeCallbackFunctionPointer_;
  std::map<ComputeCallbackName const, void *, COMPUTE_CALLBACK_NAME::Comparator>
      computeCallbackDataObjectPointer_;

  mutable std::vector<std::vector<int> > getNeighborListStorage_;


  void * modelBuffer_;
  void * simulatorBuffer_;

  mutable std::string string_;

};  // class ComputeArgumentsImplementation
}  // namespace KIM
#endif  // KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_

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


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class SupportStatus;
class ComputeArgumentName;
class ComputeCallbackName;
class ModelComputeArgumentsCreateImplementation;


class ModelComputeArgumentsCreate
{
 public:
  int SetArgumentSupportStatus(ComputeArgumentName const clomputeArgumentName,
                               SupportStatus const supportStatus);

  int SetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus const supportStatus);

  void SetModelBufferPointer(void * const ptr);

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

  std::string const & String() const;

 private:
  // do not allow copy constructor or operator=
  ModelComputeArgumentsCreate(ModelComputeArgumentsCreate const &);
  void operator=(ModelComputeArgumentsCreate const &);

  ModelComputeArgumentsCreate();
  ~ModelComputeArgumentsCreate();

  ModelComputeArgumentsCreateImplementation * pimpl;
};  // class ModelComputeArgumentsCreate
}  // namespace KIM

#endif  // KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_

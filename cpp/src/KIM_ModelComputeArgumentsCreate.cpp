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
#include "KIM_ModelComputeArgumentsCreate.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

#define CONVERT_POINTER ComputeArgumentsImplementation *pImpl   \
  = reinterpret_cast<ComputeArgumentsImplementation *>(pimpl)


namespace KIM
{
int ModelComputeArgumentsCreate::SetArgumentSupportStatus(
    ComputeArgumentName const computeArgumentName,
    SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pImpl->SetArgumentSupportStatus(computeArgumentName, supportStatus);
}

int ModelComputeArgumentsCreate::SetCallbackSupportStatus(
    ComputeCallbackName const computeCallbackName,
    SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pImpl->SetCallbackSupportStatus(computeCallbackName, supportStatus);
}

void ModelComputeArgumentsCreate::SetModelBufferPointer(void * const ptr)
{
  CONVERT_POINTER;

  pImpl->SetModelBufferPointer(ptr);
}

void ModelComputeArgumentsCreate::LogEntry(LogVerbosity const logVerbosity,
                                           std::string const & message,
                                           int const lineNumber,
                                           std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelComputeArgumentsCreate::LogEntry(LogVerbosity const logVerbosity,
                                           std::stringstream const & message,
                                           int const lineNumber,
                                           std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelComputeArgumentsCreate::String() const
{
  CONVERT_POINTER;

  return pImpl->String();
}

ModelComputeArgumentsCreate::ModelComputeArgumentsCreate() : pimpl(0)
{
}

ModelComputeArgumentsCreate::~ModelComputeArgumentsCreate()
{
}

}  // namespace KIM

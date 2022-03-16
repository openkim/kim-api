//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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
// Release: This file is part of the kim-api.git repository.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
#include "KIM_ModelComputeArgumentsCreate.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

#define CONVERT_POINTER                  \
  ComputeArgumentsImplementation * pImpl \
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

std::string const & ModelComputeArgumentsCreate::ToString() const
{
  CONVERT_POINTER;

  return pImpl->ToString();
}

ModelComputeArgumentsCreate::ModelComputeArgumentsCreate() : pimpl(NULL) {}

ModelComputeArgumentsCreate::~ModelComputeArgumentsCreate() {}

}  // namespace KIM

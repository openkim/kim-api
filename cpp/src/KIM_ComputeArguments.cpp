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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ComputeArguments.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

namespace KIM
{
int ComputeArguments::GetArgumentSupportStatus(
    ComputeArgumentName const computeArgumentName,
    SupportStatus * const supportStatus) const
{
  return pimpl->GetArgumentSupportStatus(computeArgumentName, supportStatus);
}

int ComputeArguments::GetCallbackSupportStatus(
    ComputeCallbackName const computeCallbackName,
    SupportStatus * const supportStatus) const
{
  return pimpl->GetCallbackSupportStatus(computeCallbackName, supportStatus);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int const * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName, double const * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName, double * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetCallbackPointer(
    ComputeCallbackName const computeCallbackName,
    LanguageName const languageName,
    Function * const fptr,
    void * const dataObject)
{
  return pimpl->SetCallbackPointer(
      computeCallbackName, languageName, fptr, dataObject);
}

void ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent(
    int * const result) const
{
  pimpl->AreAllRequiredArgumentsAndCallbacksPresent(result);
}

void ComputeArguments::SetSimulatorBufferPointer(void * const ptr)
{
  pimpl->SetSimulatorBufferPointer(ptr);
}

void ComputeArguments::GetSimulatorBufferPointer(void ** const ptr) const
{
  pimpl->GetSimulatorBufferPointer(ptr);
}

std::string const & ComputeArguments::ToString() const
{
  return pimpl->ToString();
}

void ComputeArguments::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void ComputeArguments::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void ComputeArguments::PopLogVerbosity() { pimpl->PopLogVerbosity(); }

ComputeArguments::ComputeArguments() : pimpl(NULL) {}

ComputeArguments::~ComputeArguments() {}

}  // namespace KIM

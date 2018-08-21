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
    ComputeArgumentName const computeArgumentName,
    int const * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName,
    int * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName,
    double const * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetArgumentPointer(
    ComputeArgumentName const computeArgumentName,
    double * const ptr)
{
  return pimpl->SetArgumentPointer(computeArgumentName, ptr);
}

int ComputeArguments::SetCallbackPointer(
    ComputeCallbackName const computeCallbackName,
    LanguageName const languageName,
    func * const fptr,
    void * const dataObject)
{
  return pimpl->SetCallbackPointer(computeCallbackName, languageName, fptr,
                                   dataObject);
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

std::string const & ComputeArguments::String() const
{
  return pimpl->String();
}

void ComputeArguments::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void ComputeArguments::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void ComputeArguments::PopLogVerbosity()
{
  pimpl->PopLogVerbosity();
}

ComputeArguments::ComputeArguments() : pimpl(NULL)
{
}

ComputeArguments::~ComputeArguments()
{
}

}  // namespace KIM

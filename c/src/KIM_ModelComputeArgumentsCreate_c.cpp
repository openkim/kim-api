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


#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C" {
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif
extern "C" {
#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_ComputeArgumentName.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif
extern "C" {
#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#include "KIM_ComputeCallbackName.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
#include "KIM_ModelComputeArgumentsCreate.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_
#include "KIM_ModelComputeArgumentsCreate.h"
#endif
}  // extern "C"


struct KIM_ModelComputeArgumentsCreate
{
  void * p;
};

#define CONVERT_POINTER                                           \
  KIM::ModelComputeArgumentsCreate * pModelComputeArgumentsCreate \
      = reinterpret_cast<KIM::ModelComputeArgumentsCreate *>(     \
          modelComputeArgumentsCreate->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}

KIM::SupportStatus makeSupportStatusCpp(KIM_SupportStatus const supportStatus)
{
  return KIM::SupportStatus(supportStatus.supportStatusID);
}

KIM::ComputeArgumentName
makeComputeArgumentNameCpp(KIM_ComputeArgumentName const computeArgumentName)
{
  return KIM::ComputeArgumentName(computeArgumentName.computeArgumentNameID);
}

KIM::ComputeCallbackName
makeComputeCallbackNameCpp(KIM_ComputeCallbackName const computeCallbackName)
{
  return KIM::ComputeCallbackName(computeCallbackName.computeCallbackNameID);
}
}  // namespace

extern "C" {
int KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsCreate->SetArgumentSupportStatus(
      makeComputeArgumentNameCpp(computeArgumentName),
      makeSupportStatusCpp(supportStatus));
}

int KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsCreate->SetCallbackSupportStatus(
      makeComputeCallbackNameCpp(computeCallbackName),
      makeSupportStatusCpp(supportStatus));
}

void KIM_ModelComputeArgumentsCreate_SetModelBufferPointer(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    void * const ptr)
{
  CONVERT_POINTER;

  pModelComputeArgumentsCreate->SetModelBufferPointer(ptr);
}


void KIM_ModelComputeArgumentsCreate_LogEntry(
    KIM_ModelComputeArgumentsCreate const * const modelComputeArgumentsCreate,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelComputeArgumentsCreate->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const * KIM_ModelComputeArgumentsCreate_ToString(
    KIM_ModelComputeArgumentsCreate const * const modelComputeArgumentsCreate)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsCreate->ToString().c_str();
}

}  // extern "C"

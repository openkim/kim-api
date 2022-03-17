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


#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C" {
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
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

#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ComputeArguments.hpp"
#endif
extern "C" {
#ifndef KIM_COMPUTE_ARGUMENTS_H_
#include "KIM_ComputeArguments.h"
#endif
}  // extern "C"


struct KIM_ComputeArguments
{
  void * p;
};

#define CONVERT_POINTER                     \
  KIM::ComputeArguments * pComputeArguments \
      = reinterpret_cast<KIM::ComputeArguments *>(computeArguments->p)

namespace
{
KIM::LanguageName makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageNameID);
}

KIM_SupportStatus makeSupportStatusC(KIM::SupportStatus supportStatus)
{
  KIM_SupportStatus supportStatusC = {supportStatus.supportStatusID};
  return supportStatusC;
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

KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C" {
int KIM_ComputeArguments_GetArgumentSupportStatus(
    KIM_ComputeArguments const * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_SupportStatus * const supportStatus)
{
  CONVERT_POINTER;
  KIM::SupportStatus supportStatusCpp;

  int error = pComputeArguments->GetArgumentSupportStatus(
      makeComputeArgumentNameCpp(computeArgumentName), &supportStatusCpp);
  if (error)
    return true;
  else
  {
    *supportStatus = makeSupportStatusC(supportStatusCpp);
    return false;
  }
}

int KIM_ComputeArguments_GetCallbackSupportStatus(
    KIM_ComputeArguments const * const computeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_SupportStatus * const supportStatus)
{
  CONVERT_POINTER;
  KIM::SupportStatus supportStatusCpp;

  int error = pComputeArguments->GetCallbackSupportStatus(
      makeComputeCallbackNameCpp(computeCallbackName), &supportStatusCpp);
  if (error)
    return true;
  else
  {
    *supportStatus = makeSupportStatusC(supportStatusCpp);
    return false;
  }
}

int KIM_ComputeArguments_SetArgumentPointerInteger(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    int const * const ptr)
{
  CONVERT_POINTER;
  KIM::ComputeArgumentName argN
      = makeComputeArgumentNameCpp(computeArgumentName);

  return pComputeArguments->SetArgumentPointer(argN, ptr);
}

int KIM_ComputeArguments_SetArgumentPointerDouble(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    double const * const ptr)
{
  CONVERT_POINTER;
  KIM::ComputeArgumentName argN
      = makeComputeArgumentNameCpp(computeArgumentName);

  return pComputeArguments->SetArgumentPointer(argN, ptr);
}

int KIM_ComputeArguments_SetCallbackPointer(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_LanguageName const languageName,
    KIM_Function * const fptr,
    void * const dataObject)
{
  CONVERT_POINTER;
  KIM::ComputeCallbackName computeCallbackNameCpp
      = makeComputeCallbackNameCpp(computeCallbackName);
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);

  return pComputeArguments->SetCallbackPointer(
      computeCallbackNameCpp, langN, fptr, dataObject);
}

void KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent(
    KIM_ComputeArguments const * const computeArguments, int * const result)
{
  CONVERT_POINTER;

  return pComputeArguments->AreAllRequiredArgumentsAndCallbacksPresent(result);
}

void KIM_ComputeArguments_SetSimulatorBufferPointer(
    KIM_ComputeArguments * const computeArguments, void * const ptr)
{
  CONVERT_POINTER;

  pComputeArguments->SetSimulatorBufferPointer(ptr);
}

void KIM_ComputeArguments_GetSimulatorBufferPointer(
    KIM_ComputeArguments const * const computeArguments, void ** const ptr)
{
  CONVERT_POINTER;

  pComputeArguments->GetSimulatorBufferPointer(ptr);
}

char const * KIM_ComputeArguments_ToString(
    KIM_ComputeArguments const * const computeArguments)
{
  CONVERT_POINTER;

  return pComputeArguments->ToString().c_str();
}

void KIM_ComputeArguments_SetLogID(
    KIM_ComputeArguments * const computeArguments, char const * const logID)
{
  CONVERT_POINTER;

  pComputeArguments->SetLogID(logID);
}

void KIM_ComputeArguments_PushLogVerbosity(
    KIM_ComputeArguments * const computeArguments,
    KIM_LogVerbosity const logVerbosity)
{
  CONVERT_POINTER;

  pComputeArguments->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_ComputeArguments_PopLogVerbosity(
    KIM_ComputeArguments * const computeArguments)
{
  CONVERT_POINTER;

  pComputeArguments->PopLogVerbosity();
}

}  // extern "C"

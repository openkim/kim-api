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

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ModelComputeArguments.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_H_
#include "KIM_ModelComputeArguments.h"
#endif
}  // extern "C"


struct KIM_ModelComputeArguments
{
  void * p;
};

#define CONVERT_POINTER                                 \
  KIM::ModelComputeArguments * pModelComputeArguments   \
      = reinterpret_cast<KIM::ModelComputeArguments *>( \
          modelComputeArguments->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
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
int KIM_ModelComputeArguments_GetNeighborList(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle)
{
  CONVERT_POINTER;

  return pModelComputeArguments->GetNeighborList(neighborListIndex,
                                                 particleNumber,
                                                 numberOfNeighbors,
                                                 neighborsOfParticle);
}

int KIM_ModelComputeArguments_ProcessDEDrTerm(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de,
    double const r,
    double const * const dx,
    int const i,
    int const j)
{
  CONVERT_POINTER;

  return pModelComputeArguments->ProcessDEDrTerm(de, r, dx, i, j);
}

int KIM_ModelComputeArguments_ProcessD2EDr2Term(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de,
    double const * const r,
    double const * const dx,
    int const * const i,
    int const * const j)
{
  CONVERT_POINTER;

  return pModelComputeArguments->ProcessD2EDr2Term(de, r, dx, i, j);
}

int KIM_ModelComputeArguments_GetArgumentPointerInteger(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    int ** const ptr)
{
  CONVERT_POINTER;

  return pModelComputeArguments->GetArgumentPointer(
      makeComputeArgumentNameCpp(computeArgumentName), ptr);
}

int KIM_ModelComputeArguments_GetArgumentPointerDouble(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    double ** const ptr)
{
  CONVERT_POINTER;

  return pModelComputeArguments->GetArgumentPointer(
      makeComputeArgumentNameCpp(computeArgumentName), ptr);
}

int KIM_ModelComputeArguments_IsCallbackPresent(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    int * const present)
{
  CONVERT_POINTER;

  KIM::ComputeCallbackName computeCallbackNameC
      = makeComputeCallbackNameCpp(computeCallbackName);
  return pModelComputeArguments->IsCallbackPresent(computeCallbackNameC,
                                                   present);
}

void KIM_ModelComputeArguments_SetModelBufferPointer(
    KIM_ModelComputeArguments * const modelComputeArguments, void * const ptr)
{
  CONVERT_POINTER;

  pModelComputeArguments->SetModelBufferPointer(ptr);
}

void KIM_ModelComputeArguments_GetModelBufferPointer(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    void ** const ptr)
{
  CONVERT_POINTER;

  pModelComputeArguments->GetModelBufferPointer(ptr);
}

void KIM_ModelComputeArguments_LogEntry(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelComputeArguments->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const * KIM_ModelComputeArguments_ToString(
    KIM_ModelComputeArguments const * const modelComputeArguments)
{
  CONVERT_POINTER;

  return pModelComputeArguments->ToString().c_str();
}

}  // extern "C"

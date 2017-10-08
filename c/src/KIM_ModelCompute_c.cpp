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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_ARGUMENT_NAME_H_
#include "KIM_ArgumentName.h"
#endif
}  // extern "C"

#ifndef KIM_CALLBACK_NAME_HPP_
#include "KIM_CallbackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALLBACK_NAME_H_
#include "KIM_CallbackName.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_COMPUTE_HPP_
#include "KIM_ModelCompute.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_COMPUTE_H_
#include "KIM_ModelCompute.h"
#endif
}

#define CONVERT_POINTER KIM::ModelCompute *pModelCompute        \
  = reinterpret_cast<KIM::ModelCompute *>(modelCompute->p)


namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}

KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  return KIM::ArgumentName(argumentName.argumentNameID);
}

KIM::CallbackName makeCallbackNameCpp(KIM_CallbackName const callbackName)
{
  return KIM::CallbackName(callbackName.callbackNameID);
}
}  // namespace


extern "C"
{
int KIM_ModelCompute_GetNeighborList(
    KIM_ModelCompute const * const modelCompute,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle)
{
  CONVERT_POINTER;

  return pModelCompute->GetNeighborList(neighborListIndex, particleNumber,
                                        numberOfNeighbors,
                                        neighborsOfParticle);
}

int KIM_ModelCompute_ProcessDEDrTerm(
    KIM_ModelCompute const * const modelCompute,
    double const de, double const r,
    double const * const dx, int const i,
    int const j)
{
  CONVERT_POINTER;

  return pModelCompute->ProcessDEDrTerm(de, r, dx, i, j);
}

int KIM_ModelCompute_ProcessD2EDr2Term(
    KIM_ModelCompute const * const modelCompute,
    double const de, double const * const r,
    double const * const dx,
    int const * const i,
    int const * const j)
{
  CONVERT_POINTER;

  return pModelCompute->ProcessD2EDr2Term(de, r, dx, i, j);
}

// *data functions
int KIM_ModelCompute_GetArgumentPointerInteger(
    KIM_ModelCompute const * const modelCompute,
    KIM_ArgumentName const argumentName,
    int ** const ptr)
{
  CONVERT_POINTER;

  return pModelCompute->GetArgumentPointer(makeArgumentNameCpp(argumentName),
                                           ptr);
}

int KIM_ModelCompute_GetArgumentPointerDouble(
    KIM_ModelCompute const * const modelCompute,
    KIM_ArgumentName const argumentName, double ** const ptr)
{
  CONVERT_POINTER;

  return pModelCompute->GetArgumentPointer(makeArgumentNameCpp(argumentName),
                                           ptr);
}

int KIM_ModelCompute_IsCallbackPresent(
    KIM_ModelCompute const * const modelCompute,
    KIM_CallbackName const callbackName, int * const present)
{
  CONVERT_POINTER;

  KIM::CallbackName callbackNameC = makeCallbackNameCpp(callbackName);
  return pModelCompute->IsCallbackPresent(callbackNameC, present);
}

void KIM_ModelCompute_GetModelBufferPointer(
    KIM_ModelCompute const * const modelCompute, void ** const ptr)
{
  CONVERT_POINTER;

  pModelCompute->GetModelBufferPointer(ptr);
}

void KIM_ModelCompute_Log(
    KIM_ModelCompute const * const modelCompute,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelCompute->Log(makeLogVerbosityCpp(logVerbosity), message, lineNumber,
                     fileName);
}

char const * const KIM_ModelCompute_String(
    KIM_ModelCompute const * const modelCompute)
{
  CONVERT_POINTER;

  return (pModelCompute->String()).c_str();
}

}  // extern "C"

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

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALL_BACK_NAME_H_
#include "KIM_CallBackName.h"
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

KIM::CallBackName makeCallBackNameCpp(KIM_CallBackName const callBackName)
{
  return KIM::CallBackName(callBackName.callBackNameID);
}
}  // namespace


extern "C"
{
int KIM_ModelCompute_get_neigh(KIM_ModelCompute const * const modelCompute,
                               int const neighborListIndex,
                               int const particleNumber,
                               int * const numberOfNeighbors,
                               int const ** const neighborsOfParticle)
{
  CONVERT_POINTER;

  return pModelCompute->get_neigh(neighborListIndex, particleNumber,
                                  numberOfNeighbors, neighborsOfParticle);
}

int KIM_ModelCompute_process_dEdr(KIM_ModelCompute const * const modelCompute,
                                  double const de, double const r,
                                  double const * const dx, int const i,
                                  int const j)
{
  CONVERT_POINTER;

  return pModelCompute->process_dEdr(de, r, dx, i, j);
}

int KIM_ModelCompute_process_d2Edr2(KIM_ModelCompute const * const modelCompute,
                                    double const de, double const * const r,
                                    double const * const dx,
                                    int const * const i,
                                    int const * const j)
{
  CONVERT_POINTER;

  return pModelCompute->process_d2Edr2(de, r, dx, i, j);
}

// *data functions
int KIM_ModelCompute_get_data_int(KIM_ModelCompute const * const modelCompute,
                                  KIM_ArgumentName const argumentName,
                                  int ** const ptr)
{
  CONVERT_POINTER;

  return pModelCompute->get_data(makeArgumentNameCpp(argumentName), ptr);
}

int KIM_ModelCompute_get_data_double(
    KIM_ModelCompute const * const modelCompute,
    KIM_ArgumentName const argumentName, double ** const ptr)
{
  CONVERT_POINTER;

  return pModelCompute->get_data(makeArgumentNameCpp(argumentName), ptr);
}

int KIM_ModelCompute_is_call_back_present(
    KIM_ModelCompute const * const modelCompute,
    KIM_CallBackName const callBackName, int * const present)
{
  CONVERT_POINTER;

  KIM::CallBackName callBackNameC = makeCallBackNameCpp(callBackName);
  return pModelCompute->is_call_back_present(callBackNameC, present);
}

void KIM_ModelCompute_get_model_buffer(
    KIM_ModelCompute const * const modelCompute, void ** const ptr)
{
  CONVERT_POINTER;

  pModelCompute->get_model_buffer(ptr);
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

char const * const KIM_ModelCompute_string(
    KIM_ModelCompute const * const modelCompute)
{
  CONVERT_POINTER;

  return (pModelCompute->string()).c_str();
}

}  // extern "C"

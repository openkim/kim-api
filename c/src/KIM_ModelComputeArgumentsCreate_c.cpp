/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C"
{
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_ComputeArgumentName.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif
extern "C"
{
#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#include "KIM_ComputeCallbackName.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
#include "KIM_ModelComputeArgumentsCreate.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_
#include "KIM_ModelComputeArgumentsCreate.h"
#endif
}  // extern "C"


struct KIM_ModelComputeArgumentsCreate
{
  void * p;
};

#define CONVERT_POINTER KIM::ModelComputeArgumentsCreate *              \
  pModelComputeArgumentsCreate                                          \
  = reinterpret_cast<KIM::ModelComputeArgumentsCreate *>                \
      (modelComputeArgumentsCreate->p)

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

KIM::ComputeArgumentName makeComputeArgumentNameCpp(
    KIM_ComputeArgumentName const computeArgumentName)
{
  return KIM::ComputeArgumentName(computeArgumentName.computeArgumentNameID);
}

KIM::ComputeCallbackName makeComputeCallbackNameCpp(
    KIM_ComputeCallbackName const computeCallbackName)
{
  return KIM::ComputeCallbackName(computeCallbackName.computeCallbackNameID);
}
}  // namespace

extern "C"
{
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
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelComputeArgumentsCreate->LogEntry(makeLogVerbosityCpp(logVerbosity),
                                         message, lineNumber, fileName);
}

char const * KIM_ModelComputeArgumentsCreate_String(
    KIM_ModelComputeArgumentsCreate const * const modelComputeArgumentsCreate)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsCreate->String().c_str();
}

}  // extern "C"

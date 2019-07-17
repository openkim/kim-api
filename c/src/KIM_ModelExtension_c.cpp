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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.1.0 package.
//

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_EXTENSION_HPP_
#include "KIM_ModelExtension.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_EXTENSION_H_
#include "KIM_ModelExtension.h"
#endif
}  // extern "C"


struct KIM_ModelExtension
{
  void * p;
};

#define CONVERT_POINTER                 \
  KIM::ModelExtension * pModelExtension \
      = reinterpret_cast<KIM::ModelExtension *>(modelExtension->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C" {
void KIM_ModelExtension_GetExtensionID(
    KIM_ModelExtension const * const modelExtension,
    char const ** const extensionID)
{
  CONVERT_POINTER;

  std::string const * pStrExtensionID;
  pModelExtension->GetExtensionID(&pStrExtensionID);
  *extensionID = pStrExtensionID->c_str();
}

KIM_Model *
KIM_ModelExtension_ToModel(KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_Model *>(modelExtension);
}

KIM_ModelCompute *
KIM_ModelExtension_ToModelCompute(KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelCompute *>(modelExtension);
}

KIM_ModelCreate *
KIM_ModelExtension_ToModelCreate(KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelCreate *>(modelExtension);
}

KIM_ModelDestroy *
KIM_ModelExtension_ToModelDestroy(KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelDestroy *>(modelExtension);
}

KIM_ModelDriverCreate * KIM_ModelExtension_ToModelDriverCreate(
    KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelDriverCreate *>(modelExtension);
}

KIM_ModelRefresh *
KIM_ModelExtension_ToModelRefresh(KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelRefresh *>(modelExtension);
}

KIM_ModelWriteParameterizedModel *
KIM_ModelExtension_ToModelWriteParameterizedModel(
    KIM_ModelExtension * const modelExtension)
{
  return reinterpret_cast<KIM_ModelWriteParameterizedModel *>(modelExtension);
}

KIM_ModelComputeArguments * KIM_ModelExtension_ToModelComputeArguments(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments)
{
  (void) modelExtension;  // Avoid unused parameter warning
  return reinterpret_cast<KIM_ModelComputeArguments *>(computeArguments);
}

KIM_ModelComputeArgumentsCreate *
KIM_ModelExtension_ToModelComputeArgumentsCreate(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments)
{
  (void) modelExtension;  // Avoid unused parameter warning
  return reinterpret_cast<KIM_ModelComputeArgumentsCreate *>(computeArguments);
}

KIM_ModelComputeArgumentsDestroy *
KIM_ModelExtension_ToModelComputeArgumentsDestroy(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments)
{
  (void) modelExtension;  // Avoid unused parameter warning
  return reinterpret_cast<KIM_ModelComputeArgumentsDestroy *>(computeArguments);
}

void KIM_ModelExtension_GetModelBufferPointer(
    KIM_ModelExtension const * const modelExtension, void ** const ptr)
{
  CONVERT_POINTER;

  pModelExtension->GetModelBufferPointer(ptr);
}

void KIM_ModelExtension_LogEntry(
    KIM_ModelExtension const * const modelExtension,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelExtension->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const *
KIM_ModelExtension_ToString(KIM_ModelExtension const * const modelExtension)
{
  CONVERT_POINTER;

  return pModelExtension->ToString().c_str();
}

}  // extern "C"

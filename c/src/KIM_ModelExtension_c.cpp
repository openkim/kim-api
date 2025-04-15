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
// Release: This file is part of the kim-api-2.4.1 package.
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

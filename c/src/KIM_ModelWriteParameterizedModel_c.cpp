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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.2.0 package.
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

#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_HPP_
#include "KIM_ModelWriteParameterizedModel.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_
#include "KIM_ModelWriteParameterizedModel.h"
#endif
}  // extern "C"


struct KIM_ModelWriteParameterizedModel
{
  void * p;
};

#define CONVERT_POINTER                                             \
  KIM::ModelWriteParameterizedModel * pModelWriteParameterizedModel \
      = reinterpret_cast<KIM::ModelWriteParameterizedModel *>(      \
          modelWriteParameterizedModel->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C" {
void KIM_ModelWriteParameterizedModel_GetPath(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const path)
{
  CONVERT_POINTER;

  std::string const * pStrPath;
  pModelWriteParameterizedModel->GetPath(&pStrPath);
  *path = pStrPath->c_str();
}

void KIM_ModelWriteParameterizedModel_GetModelName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const modelName)
{
  CONVERT_POINTER;

  std::string const * pStrModelName;
  pModelWriteParameterizedModel->GetModelName(&pStrModelName);
  *modelName = pStrModelName->c_str();
}

void KIM_ModelWriteParameterizedModel_SetParameterFileName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelWriteParameterizedModel->SetParameterFileName(fileName);
}

void KIM_ModelWriteParameterizedModel_GetModelBufferPointer(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    void ** const ptr)
{
  CONVERT_POINTER;

  pModelWriteParameterizedModel->GetModelBufferPointer(ptr);
}

void KIM_ModelWriteParameterizedModel_LogEntry(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelWriteParameterizedModel->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const * KIM_ModelWriteParameterizedModel_ToString(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel)
{
  CONVERT_POINTER;

  return pModelWriteParameterizedModel->ToString().c_str();
}

}  // extern "C"

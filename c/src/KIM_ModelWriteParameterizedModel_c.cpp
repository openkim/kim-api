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

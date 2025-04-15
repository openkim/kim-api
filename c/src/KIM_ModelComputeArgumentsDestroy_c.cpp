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

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_HPP_
#include "KIM_ModelComputeArgumentsDestroy.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_
#include "KIM_ModelComputeArgumentsDestroy.h"
#endif
}  // extern "C"


struct KIM_ModelComputeArgumentsDestroy
{
  void * p;
};

#define CONVERT_POINTER                                             \
  KIM::ModelComputeArgumentsDestroy * pModelComputeArgumentsDestroy \
      = reinterpret_cast<KIM::ModelComputeArgumentsDestroy *>(      \
          modelComputeArgumentsDestroy->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C" {
void KIM_ModelComputeArgumentsDestroy_GetModelBufferPointer(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    void ** const ptr)
{
  CONVERT_POINTER;

  pModelComputeArgumentsDestroy->GetModelBufferPointer(ptr);
}


void KIM_ModelComputeArgumentsDestroy_LogEntry(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelComputeArgumentsDestroy->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const * KIM_ModelComputeArgumentsDestroy_ToString(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsDestroy->ToString().c_str();
}

}  // extern "C"

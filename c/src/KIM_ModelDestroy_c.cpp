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

#ifndef KIM_MODEL_DESTROY_HPP_
#include "KIM_ModelDestroy.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_DESTROY_H_
#include "KIM_ModelDestroy.h"
#endif
}  // extern "C"


struct KIM_ModelDestroy
{
  void * p;
};

#define CONVERT_POINTER             \
  KIM::ModelDestroy * pModelDestroy \
      = reinterpret_cast<KIM::ModelDestroy *>(modelDestroy->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C" {
void KIM_ModelDestroy_GetModelBufferPointer(
    KIM_ModelDestroy const * const modelDestroy, void ** const ptr)
{
  CONVERT_POINTER;

  pModelDestroy->GetModelBufferPointer(ptr);
}

void KIM_ModelDestroy_LogEntry(KIM_ModelDestroy const * const modelDestroy,
                               KIM_LogVerbosity const logVerbosity,
                               char const * const message,
                               int const lineNumber,
                               char const * const fileName)
{
  CONVERT_POINTER;

  pModelDestroy->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const *
KIM_ModelDestroy_ToString(KIM_ModelDestroy const * const modelDestroy)
{
  CONVERT_POINTER;

  return pModelDestroy->ToString().c_str();
}

}  // extern "C"

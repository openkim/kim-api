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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_DESTROY_HPP_
#include "KIM_ModelDestroy.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_DESTROY_H_
#include "KIM_ModelDestroy.h"
#endif
}  // extern "C"


struct KIM_ModelDestroy
{
  void * p;
};

#define CONVERT_POINTER KIM::ModelDestroy * pModelDestroy       \
  = reinterpret_cast<KIM::ModelDestroy *>(modelDestroy->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C"
{
void KIM_ModelDestroy_GetModelBufferPointer(
    KIM_ModelDestroy const * const modelDestroy, void ** const ptr)
{
  CONVERT_POINTER;

  pModelDestroy->GetModelBufferPointer(ptr);
}

void KIM_ModelDestroy_LogEntry(
    KIM_ModelDestroy const * const modelDestroy,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelDestroy->LogEntry(makeLogVerbosityCpp(logVerbosity), message,
                          lineNumber, fileName);
}

char const * KIM_ModelDestroy_String(
    KIM_ModelDestroy const * const modelDestroy)
{
  CONVERT_POINTER;

  return pModelDestroy->String().c_str();
}

}  // extern "C"

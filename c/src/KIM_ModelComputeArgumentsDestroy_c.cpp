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

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_HPP_
#include "KIM_ModelComputeArgumentsDestroy.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_
#include "KIM_ModelComputeArgumentsDestroy.h"
#endif
}  // extern "C"


struct KIM_ModelComputeArgumentsDestroy
{
  void * p;
};

#define CONVERT_POINTER KIM::ModelComputeArgumentsDestroy *             \
  pModelComputeArgumentsDestroy                                         \
  = reinterpret_cast<KIM::ModelComputeArgumentsDestroy *>               \
      (modelComputeArgumentsDestroy->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C"
{
void KIM_ModelComputeArgumentsDestroy_GetModelBufferPointer(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    void ** const ptr)
{
  CONVERT_POINTER;

  pModelComputeArgumentsDestroy->GetModelBufferPointer(ptr);
}


void KIM_ModelComputeArgumentsDestroy_LogEntry(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelComputeArgumentsDestroy->LogEntry(makeLogVerbosityCpp(logVerbosity),
                                          message, lineNumber, fileName);
}

char const * KIM_ModelComputeArgumentsDestroy_String(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy)
{
  CONVERT_POINTER;

  return pModelComputeArgumentsDestroy->String().c_str();
}

}  // extern "C"

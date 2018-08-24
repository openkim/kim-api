//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
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

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_H_
#include "KIM_Log.h"
#endif
}  // extern "C"


struct KIM_Log
{
  void * p;
};

#define CONVERT_POINTER KIM::Log * pLog         \
  = reinterpret_cast<KIM::Log *>(log->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C"
{
int KIM_Log_Create(KIM_Log ** const log)
{
  KIM::Log * pLog;
  int error = KIM::Log::Create(&pLog);
  if (error)
  {
    return true;
  }
  else
  {
    (*log) = new KIM_Log;
    (*log)->p = (void *) pLog;
    return false;
  }
}

void KIM_Log_Destroy(KIM_Log ** const log)
{
  KIM::Log * pLog = reinterpret_cast<KIM::Log *>((*log)->p);

  KIM::Log::Destroy(&pLog);
  delete (*log);
  *log = NULL;
}

char const * KIM_Log_GetID(KIM_Log const * const log)
{
  CONVERT_POINTER;

  return pLog->GetID().c_str();
}

void KIM_Log_SetID(KIM_Log * const log, char const * const id)
{
  CONVERT_POINTER;

  pLog->SetID(id);
}

void KIM_Log_PushVerbosity(KIM_Log * const log,
                           KIM_LogVerbosity const logVerbosity)
{
  CONVERT_POINTER;

  pLog->PushVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Log_PopVerbosity(KIM_Log * const log)
{
  CONVERT_POINTER;

  pLog->PopVerbosity();
}

void KIM_Log_LogEntry(KIM_Log const * const log,
                      KIM_LogVerbosity const logVerbosity,
                      char const * const message,
                      int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pLog->LogEntry(makeLogVerbosityCpp(logVerbosity),
                 message,
                 lineNumber,
                 fileName);
}

}  // extern "C"

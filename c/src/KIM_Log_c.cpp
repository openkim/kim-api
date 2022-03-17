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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <cstddef>
#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C" {
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif
}  // extern "C"

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_H_
#include "KIM_Log.h"
#endif
}  // extern "C"


struct KIM_Log
{
  void * p;
};

#define CONVERT_POINTER KIM::Log * pLog = reinterpret_cast<KIM::Log *>(log->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}

KIM::LanguageName makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageNameID);
}
}  // namespace


extern "C" {
int KIM_Log_Create(KIM_Log ** const log)
{
  KIM::Log * pLog;
  int error = KIM::Log::Create(&pLog);
  if (error) return error;

  (*log) = new KIM_Log;
  (*log)->p = (void *) pLog;
  return false;
}

void KIM_Log_Destroy(KIM_Log ** const log)
{
  if (*log != NULL)
  {
    KIM::Log * pLog = reinterpret_cast<KIM::Log *>((*log)->p);

    KIM::Log::Destroy(&pLog);
  }
  delete (*log);
  *log = NULL;
}

void KIM_Log_PushDefaultVerbosity(KIM_LogVerbosity const logVerbosity)
{
  KIM::Log::PushDefaultVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Log_PopDefaultVerbosity() { KIM::Log::PopDefaultVerbosity(); }

void KIM_Log_PushDefaultPrintFunction(KIM_LanguageName const languageName,
                                      KIM_Function * const fptr)
{
  KIM::Log::PushDefaultPrintFunction(makeLanguageNameCpp(languageName), fptr);
}

void KIM_Log_PopDefaultPrintFunction() { KIM::Log::PopDefaultPrintFunction(); }

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
                      int const lineNumber,
                      char const * const fileName)
{
  CONVERT_POINTER;

  pLog->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

}  // extern "C"

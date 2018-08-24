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


namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  KIM::LogVerbosity const * const logVerbosityCpp
      = reinterpret_cast <KIM::LogVerbosity const *>(&logVerbosity);
  return *logVerbosityCpp;
}

KIM_LogVerbosity makeLogVerbosityC(KIM::LogVerbosity const logVerbosity)
{
  KIM_LogVerbosity const * const logVerbosityC
      = reinterpret_cast <KIM_LogVerbosity const *>(&logVerbosity);
  return *logVerbosityC;
}
}  // namespace

extern "C"
{
KIM_LogVerbosity KIM_LogVerbosity_FromString(char const * const str)
{
  return makeLogVerbosityC(KIM::LogVerbosity(std::string(str)));
}

int KIM_LogVerbosity_LessThan(KIM_LogVerbosity const left,
                              KIM_LogVerbosity const right)
{
  return (left.logVerbosityID < right.logVerbosityID);
}

int KIM_LogVerbosity_GreaterThan(KIM_LogVerbosity const left,
                                 KIM_LogVerbosity const right)
{
  return (left.logVerbosityID > right.logVerbosityID);
}
int KIM_LogVerbosity_LessThanEqual(KIM_LogVerbosity const left,
                                   KIM_LogVerbosity const right)
{
    return (left.logVerbosityID <= right.logVerbosityID);
}
int KIM_LogVerbosity_GreaterThanEqual(KIM_LogVerbosity const left,
                                      KIM_LogVerbosity const right)
{
  return (left.logVerbosityID >= right.logVerbosityID);
}

int KIM_LogVerbosity_Equal(KIM_LogVerbosity const left,
                           KIM_LogVerbosity const right)
{
  return (left.logVerbosityID == right.logVerbosityID);
}

int KIM_LogVerbosity_NotEqual(KIM_LogVerbosity const left,
                              KIM_LogVerbosity const right)
{
  return (! KIM_LogVerbosity_Equal(left, right));
}

char const * KIM_LogVerbosity_String(KIM_LogVerbosity const logVerbosity)
{
  return makeLogVerbosityCpp(logVerbosity).String().c_str();
}

KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent
= {KIM::LOG_VERBOSITY::silent.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal
= {KIM::LOG_VERBOSITY::fatal.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_error
= {KIM::LOG_VERBOSITY::error.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning
= {KIM::LOG_VERBOSITY::warning.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_information
= {KIM::LOG_VERBOSITY::information.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug
= {KIM::LOG_VERBOSITY::debug.logVerbosityID};

void KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities(
    int * const numberOfLogVerbosities)
{
  KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities(numberOfLogVerbosities);
}

int KIM_LOG_VERBOSITY_GetLogVerbosity(int const index,
                                      KIM_LogVerbosity * const logVerbosity)
{
  KIM::LogVerbosity logVerbosityCpp;
  int error = KIM::LOG_VERBOSITY::GetLogVerbosity(index, &logVerbosityCpp);
  if (error) return error;
  *logVerbosity = makeLogVerbosityC(logVerbosityCpp);
  return false;
}

}  // extern "C"
